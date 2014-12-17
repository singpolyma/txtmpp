{-# LANGUAGE ForeignFunctionInterface #-}
module BlackberryHub (HubRequest(..), InboxItem(..), hubServer) where

import Prelude ()
import BasicPrelude
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Control.Monad.Trans.State
import Control.Monad.Trans.Cont
import Control.Concurrent.STM

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

data HubRequest =
	AddHubAccount Text |
	RemoveHubAccount Text |
	UpdateInboxItem Bool InboxItem
	deriving (Show, Eq)

data InboxItem = InboxItem {
		account :: Text,
		source  :: Text,
		title   :: Text,
		summary :: Text,
		updated :: UTCTime,
		unread  :: Int,
		total   :: Int
	} deriving (Show, Eq)

newtype AccountID = AccountID CLLong deriving (Show, Eq, Ord)
newtype AccountType = AccountType CInt deriving (Show, Eq, Ord)

-- Our wrappers

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

foreign import ccall safe "hub.cpp hub_update_item"
	c_hub_update_item ::
	Ptr UDSInbox ->
	IO CInt

-- Account data structure

data UDSAccount

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

-- Inbox item data structure

data UDSInbox

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_create"
	c_uds_inbox_item_data_create ::
	IO (Ptr UDSInbox)

foreign import ccall safe "bb/pim/unified/unified_data_source.h &uds_inbox_item_data_destroy"
	c_uds_inbox_item_data_destroy ::
	FinalizerPtr UDSInbox

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_account_id"
	c_uds_inbox_item_data_set_account_id ::
	Ptr UDSInbox -> AccountID ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_source_id"
	c_uds_inbox_item_data_set_source_id ::
	Ptr UDSInbox -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_name"
	c_uds_inbox_item_data_set_name ::
	Ptr UDSInbox -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_description"
	c_uds_inbox_item_data_set_description ::
	Ptr UDSInbox -> CString ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_timestamp"
	c_uds_inbox_item_data_set_timestamp ::
	Ptr UDSInbox -> CLLong ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_total_count"
	c_uds_inbox_item_data_set_total_count ::
	Ptr UDSInbox -> CInt ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_unread_count"
	c_uds_inbox_item_data_set_unread_count ::
	Ptr UDSInbox -> CInt ->
	IO ()

foreign import ccall safe "bb/pim/unified/unified_data_source.h uds_inbox_item_data_set_notification_state"
	c_uds_inbox_item_data_set_notification_state ::
	Ptr UDSInbox -> CInt ->
	IO ()

-- The server

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
		if accountId < AccountID 0 then return () else do -- TODO: log error?
			account <- lift $ lift $ newForeignPtr c_uds_account_data_destroy =<< c_uds_account_data_create
			accountP <- lift $ ContT $ withForeignPtr account
			lift $ lift $ c_uds_account_data_set_id accountP accountId
			lift $ lift $ c_uds_account_data_set_type accountP (AccountType 6) -- type IM
			lift $ lift $ c_uds_account_data_set_description accountP csjid

			name <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "txtmpp")
			lift $ lift $ c_uds_account_data_set_name accountP name

			icon <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "hub.png")
			lift $ lift $ c_uds_account_data_set_icon accountP icon

			target <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "net.singpolyma.txtmpp")
			lift $ lift $ c_uds_account_data_set_target_name accountP target

			void $ lift $ lift $ c_hub_update_account accountP -- TODO: log error?

			modify (Map.insert jid accountId)
	msg (RemoveHubAccount jid) = mapStateT evalContT $ do
		accountId <- Map.lookup jid <$> get
		case accountId of
			Just id -> void $ lift $ lift $ c_hub_remove_account id -- TODO: Log error?
			Nothing -> return () -- TODO: Log error?
	msg (UpdateInboxItem notify (InboxItem account source title summary updated unread total)) = do
		accountId <- Map.lookup account <$> get
		lift $ evalContT $ case accountId of
			Nothing -> return () -- TODO: Log error?
			Just accountId -> do
				item <- lift $ newForeignPtr c_uds_inbox_item_data_destroy =<< c_uds_inbox_item_data_create
				itemP <- ContT $ withForeignPtr item
				lift $ c_uds_inbox_item_data_set_account_id itemP accountId

				c_source <- ContT $ BS.useAsCString $ T.encodeUtf8 source
				lift $ c_uds_inbox_item_data_set_source_id itemP c_source

				c_title <- ContT $ BS.useAsCString $ T.encodeUtf8 $ T.take 50 title
				lift $ c_uds_inbox_item_data_set_name itemP c_title

				c_summary <- ContT $ BS.useAsCString $ T.encodeUtf8 $ T.take 50 summary
				lift $ c_uds_inbox_item_data_set_description itemP c_summary

				lift $ c_uds_inbox_item_data_set_timestamp itemP $ floor $ 1000 * utcTimeToPOSIXSeconds updated
				lift $ c_uds_inbox_item_data_set_unread_count itemP (fromIntegral unread)
				lift $ c_uds_inbox_item_data_set_total_count itemP (fromIntegral total)
				lift $ c_uds_inbox_item_data_set_notification_state itemP $ if notify then 1 else 0

				void $ lift $ c_hub_update_item itemP -- TODO: log error?
