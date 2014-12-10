{-# LANGUAGE ForeignFunctionInterface #-}
module BlackberryHub (HubRequest(..), hubServer) where

import Prelude ()
import BasicPrelude

import Control.Monad.Trans.State
import Control.Monad.Trans.Cont
import Control.Concurrent.STM

import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

data HubRequest = AddHubAccount Text | RemoveHubAccount Text deriving (Show, Eq)

newtype AccountID = AccountID CLLong deriving (Show, Eq, Ord)
newtype AccountType = AccountType CInt deriving (Show, Eq, Ord)

data UDSAccount

foreign import ccall safe "hub.cpp hub_init"
	c_hub_init ::
	CString -> CString ->
	IO CInt

foreign import ccall safe "hub.cpp hub_find_account_id"
	c_hub_find_account_id ::
	CString ->
	IO AccountID

foreign import ccall safe "hub.cpp hub_remove_account"
	c_hub_remove_account ::
	AccountID ->
	IO CInt

foreign import ccall safe "hub.cpp hub_update_account"
	c_hub_update_account ::
	Ptr UDSAccount ->
	IO CInt

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_create"
	c_uds_account_data_create ::
	IO (Ptr UDSAccount)

foreign import ccall safe "bb/pim/unified/unified_data_source.h &uds_account_data_destroy"
	c_uds_account_data_destroy ::
   FinalizerPtr UDSAccount

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_id"
	c_uds_account_data_set_id ::
	Ptr UDSAccount -> AccountID ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_type"
	c_uds_account_data_set_type ::
	Ptr UDSAccount -> AccountType ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_name"
	c_uds_account_data_set_name ::
	Ptr UDSAccount -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_description"
	c_uds_account_data_set_description ::
	Ptr UDSAccount -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_icon"
	c_uds_account_data_set_icon ::
	Ptr UDSAccount -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_account_data_set_target_name"
	c_uds_account_data_set_target_name ::
	Ptr UDSAccount -> CString ->
	IO ()

hubServer :: TChan HubRequest -> IO ()
hubServer chan = evalContT $ do
	owner <- ContT $ BS.useAsCString $ T.encodeUtf8 $ T.pack "net.singpolyma.txtmpp"
	assets <- ContT $ BS.useAsCString $ T.encodeUtf8 $ T.pack "/apps/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/public/assets-public/"
	void $ lift $ c_hub_init owner assets -- TODO: log error?

	lift $ evalStateT (forever $ lift (atomically $ readTChan chan) >>= msg) empty
	where
	msg (AddHubAccount jid) = mapStateT evalContT $ do
		csjid <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 jid)
		accountId <- lift $ lift $ c_hub_find_account_id csjid
		mapStateT lift $ if accountId < AccountID 0 then return () else do -- TODO: log error?
			account <- lift $ newForeignPtr c_uds_account_data_destroy =<< c_uds_account_data_create
			lift $ withForeignPtr account $ \account -> evalContT $ do
				lift $ c_uds_account_data_set_id account accountId
				lift $ c_uds_account_data_set_type account (AccountType 6) -- type IM
				lift $ c_uds_account_data_set_description account csjid

				name <- ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "txtmpp")
				lift $ c_uds_account_data_set_name account name

				icon <- ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "hub.png")
				lift $ c_uds_account_data_set_icon account icon

				target <- ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "net.singpolyma.txtmpp")
				lift $ c_uds_account_data_set_target_name account target

				void $ lift $ c_hub_update_account account -- TODO: log error?

			modify (Map.insert jid accountId)
	msg (RemoveHubAccount jid) = mapStateT evalContT $ do
		accountId <- Map.lookup jid <$> get
		case accountId of
			Just id -> void $ lift $ lift $ c_hub_remove_account id -- TODO: Log error?
			Nothing -> return () -- TODO: Log error?
