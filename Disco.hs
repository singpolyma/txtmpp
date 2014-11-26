-- | http://xmpp.org/extensions/xep-0030.html
module Disco (DiscoTicket, registerFeature, Feature(..), Identity(..), startDisco, stopDisco) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Xmpp
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically, STM)
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
	(Jid -> IO Bool) -- ^ Should be 'True' if ok to respond to discovery from this Jid
	-> [Identity]
	-> Session
	-> IO (Maybe DiscoTicket)
startDisco p is s =
	listenIQ Get discoNS s >>= either (const $ return Nothing) (\action -> do
			fsio <- newIORef [Feature discoNS]
			threadID <- forkIO (startDisco' p is fsio action)
			return $ Just $ DiscoTicket (fsio, threadID)
		)

startDisco' :: (Jid -> IO Bool) -> [Identity] -> IORef [Feature] -> STM IQRequestTicket -> IO ()
startDisco' p' is' fsio action = forever $ do
	ticket <- liftIO $ atomically action
	fs <- (fmap.fmap) (NodeElement . featureToElement) (readIORef fsio)
	allow <- maybe (return True) p (iqRequestFrom $ iqRequestBody ticket)
	-- This ignores duplicate send errors
	void $ liftIO $ answerIQ ticket (
			if allow then Right (Just $ query fs) else
				Left $ StanzaError Cancel ServiceUnavailable Nothing Nothing
		) []
	where
	query fs = Element (nsname "query") [] (is ++ fs)
	is = map (NodeElement . identityToElement) is'
	p = liftIO . p'
