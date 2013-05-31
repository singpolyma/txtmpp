-- | http://xmpp.org/extensions/xep-0199.html
module Ping (respondToPing) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Xmpp
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan, TChan)
import Control.Error (maybeT)
import qualified Data.Text as T

respondToPing ::
	(Jid -> IO Bool) -- ^ Should be 'True' if ok to respond to Pings from this Jid
	-> Session
	-> IO Bool          -- ^ 'False' if someone else is responding to ping
respondToPing p s =
	listenIQChan Get (T.pack "urn:xmpp:ping") s >>=
		either (const $ return False) (respondToPing' p)

respondToPing' :: (Jid -> IO Bool) -> TChan IQRequestTicket -> IO Bool
respondToPing' p' chan =
	maybeT (return False) (error "Ping.respondToPing infinite loop ended") $
		forever $ do
			ticket <- liftIO $ atomically (readTChan chan)
			allow <- maybe (return True) p (iqRequestFrom $ iqRequestBody ticket)
			result <- liftIO $ answerIQ ticket $
				if allow then Right Nothing else
					Left $ StanzaError Cancel ServiceUnavailable Nothing Nothing
			guard (not result)
	where
	p = liftIO . p'
