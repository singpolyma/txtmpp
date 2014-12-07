{-# LANGUAGE ForeignFunctionInterface #-}
module BlackberryHub where

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

import System.Log.Logger

data HubRequest = AddHubAccount Text | RemoveHubAccount Text deriving (Show, Eq)

newtype AccountID = AccountID CLLong deriving (Show, Eq, Ord)

foreign import ccall safe "hub.cpp hub_setup_account"
	c_hub_setup_account ::
	CString -> CString -> CString -> CString -> CString ->
	IO AccountID

foreign import ccall safe "hub.cpp hub_remove_account"
	c_hub_remove_account ::
	AccountID -> CString -> CString ->
	IO CInt

owner :: BS.ByteString
owner = T.encodeUtf8 $ T.pack "net.singpolyma.txtmpp"

assets :: BS.ByteString
assets = T.encodeUtf8 $ T.pack "/apps/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/public/assets-public/"

hubServer :: TChan HubRequest -> IO ()
hubServer chan =
	evalStateT (forever $ lift (atomically $ readTChan chan) >>= msg) empty
	where
	msg (AddHubAccount jid) = mapStateT evalContT $ do
		csjid <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 jid)
		displayName <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "txtmpp")
		icon <- lift $ ContT $ BS.useAsCString (T.encodeUtf8 $ T.pack "hub.png")
		owner <- lift $ ContT $ BS.useAsCString owner
		assets <- lift $ ContT $ BS.useAsCString assets

		accountId <- lift $ lift $ c_hub_setup_account csjid displayName icon owner assets
		if accountId < AccountID 0 then return () else -- TODO: Log error?
			modify (Map.insert jid accountId)
	msg (RemoveHubAccount jid) = mapStateT evalContT $ do
		owner <- lift $ ContT $ BS.useAsCString owner
		assets <- lift $ ContT $ BS.useAsCString assets

		accountId <- Map.lookup jid <$> get
		case accountId of
			Just id -> void $ lift $ lift $ c_hub_remove_account id owner assets -- TODO: Log error?
			Nothing -> return () -- TODO: Log error?
