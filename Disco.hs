-- | http://xmpp.org/extensions/xep-0030.html
module Disco (DiscoTicket, registerFeature, Feature(..), Identity(..), startDisco, stopDisco) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Xmpp
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Data.IORef (IORef, atomicModifyIORef, readIORef, newIORef)
import qualified Data.Text as T

import Data.XML.Types

-- | For registering new features, etc
newtype DiscoTicket = DiscoTicket (IORef [Feature], ThreadId)

registerFeature :: Feature -> DiscoTicket -> IO ()
registerFeature f (DiscoTicket (fsio, _)) =
	atomicModifyIORef fsio (\fs -> (f:fs,()))

data Feature = Feature T.Text
	deriving (Show, Eq)

data Identity = Identity T.Text T.Text (Maybe T.Text) (Maybe LangTag)
	deriving (Show, Eq)

discoNS :: T.Text
discoNS = T.pack "http://jabber.org/protocol/disco#info"

name :: String -> Name
name local = Name (T.pack local) Nothing Nothing

nsname :: String -> Name
nsname local = Name (T.pack local) (Just discoNS) Nothing

identityToElement :: Identity -> Element
identityToElement (Identity cat typ nam lang) = Element
	-- XXX: May want to include http://jabber.org/protocol/disco#info namespace?
	(nsname "identity")
	([
		(name "category", [ContentText cat]),
		(name "type", [ContentText typ])
	] ++ maybe [] (\n -> [(name "name", [ContentText n])]) nam
	  ++ maybe [] (\l -> [(name "xml:lang", [ContentText $ T.pack $ show l])]) lang)
	[]

featureToElement :: Feature -> Element
featureToElement (Feature var) = Element
	(nsname "feature")
	[(name "var", [ContentText var])]
	[]

stopDisco :: DiscoTicket -> IO ()
stopDisco (DiscoTicket (_,tid)) = killThread tid

startDisco ::
	[Identity]
	-> Session
	-> IO (Maybe DiscoTicket)
startDisco is s =
	listenIQChan Get discoNS s >>= either (const $ return Nothing) (\chan -> do
			fsio <- newIORef [Feature discoNS]
			threadID <- forkIO (startDisco' is fsio chan)
			return $ Just $ DiscoTicket (fsio, threadID)
		)

startDisco' :: [Identity] -> IORef [Feature] -> TChan IQRequestTicket -> IO ()
startDisco' is' fsio chan = forever $ do
	ticket <- liftIO $ atomically (readTChan chan)
	fs <- (fmap.fmap) (NodeElement . featureToElement) (readIORef fsio)
	-- This ignores duplicate send errors
	void $ liftIO $ answerIQ ticket $ Right (Just $ query fs)
	where
	query fs = Element (nsname "query") [] (is ++ fs)
	is = map (NodeElement . identityToElement) is'
