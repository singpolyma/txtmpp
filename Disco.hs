-- | http://xmpp.org/extensions/xep-0030.html
module Disco where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Xmpp
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Control.Error (maybeT)
import qualified Data.Text as T

import Data.XML.Types

data Identity = Identity T.Text T.Text (Maybe T.Text)
	deriving (Show, Eq)

discoNS :: T.Text
discoNS = T.pack "http://jabber.org/protocol/disco#info"

name :: String -> Name
name local = Name (T.pack local) Nothing Nothing

nsname :: String -> Name
nsname local = Name (T.pack local) (Just discoNS) Nothing

identityToElement :: Identity -> Element
identityToElement (Identity cat typ nam) = Element
	-- XXX: May want to include http://jabber.org/protocol/disco#info namespace?
	(nsname "identity")
	([
		(name "category", [ContentText cat]),
		(name "type", [ContentText typ])
	] ++ maybe [] (\n -> [(name "name", [ContentText n])]) nam)
	[]

respondToDisco ::
	[Identity]
	-> Session
	-> IO Bool          -- ^ 'False' if someone else is responding to disco
respondToDisco is s =
	listenIQChan Get discoNS s >>=
		either (const $ return False) (respondToDisco' is)

respondToDisco' :: [Identity] -> TChan IQRequestTicket -> IO Bool
respondToDisco' is' chan =
	maybeT (return False) (error "Disco.respondtoDisco' infinite loop ended") $
		forever $ do
			ticket <- liftIO $ atomically (readTChan chan)
			result <- liftIO $ answerIQ ticket $ Right (Just query)
			guard (not result)
	where
	query = Element (nsname "query") [] is
	is = map (NodeElement . identityToElement) is'
