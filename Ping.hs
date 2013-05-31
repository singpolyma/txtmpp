-- | http://xmpp.org/extensions/xep-0199.html
module Ping (respondToPing) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Xmpp
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Control.Error (maybeT)
import qualified Data.Text as T

import Disco

respondToPing ::
	(Jid -> IO Bool) -- ^ Should be 'True' if ok to respond to Pings from this Jid
	-> DiscoTicket
	-> Session
	-> IO Bool          -- ^ 'False' if someone else is responding to ping
respondToPing p disco s =
	listenIQChan Get (T.pack "urn:xmpp:ping") s >>=
		either (const $ return False) (respondToPing' p disco)

respondToPing' :: (Jid -> IO Bool) -> DiscoTicket -> TChan IQRequestTicket -> IO Bool
respondToPing' p' disco chan = do
	registerFeature (Feature (T.pack "urn:xmpp:ping")) disco
	maybeT (return False) (error "Ping.respondToPing' infinite loop ended") $
		forever $ do
			ticket <- liftIO $ atomically (readTChan chan)
			allow <- maybe (return True) p (iqRequestFrom $ iqRequestBody ticket)
			result <- liftIO $ answerIQ ticket $
				if allow then Right Nothing else
					Left $ StanzaError Cancel ServiceUnavailable Nothing Nothing
			guard result
	where
	p = liftIO . p'
