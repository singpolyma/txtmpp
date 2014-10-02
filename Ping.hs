-- | http://xmpp.org/extensions/xep-0199.html
module Ping (respondToPing, doPing) where

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.Xmpp
import Control.Concurrent.STM (atomically, STM)
import Control.Error (maybeT)
import qualified Data.Text as T

import Data.XML.Types

import Disco

pingNS :: T.Text
pingNS = T.pack "urn:xmpp:ping"

respondToPing ::
	(Jid -> IO Bool) -- ^ Should be 'True' if ok to respond to Pings from this Jid
	-> DiscoTicket
	-> Session
	-> IO Bool          -- ^ 'False' if someone else is responding to ping
respondToPing p disco s =
	listenIQ Get pingNS s >>=
		either (const $ return False) (respondToPing' p disco)

respondToPing' :: (Jid -> IO Bool) -> DiscoTicket -> STM IQRequestTicket -> IO Bool
respondToPing' p' disco action = do
	registerFeature (Feature pingNS) disco
	maybeT (return False) (error "Ping.respondToPing' infinite loop ended") $
		forever $ do
			ticket <- liftIO $ atomically action
			allow <- maybe (return True) p (iqRequestFrom $ iqRequestBody ticket)
			result <- liftIO $ answerIQ ticket (
					if allow then Right Nothing else
						Left $ StanzaError Cancel ServiceUnavailable Nothing Nothing
				) []
			guard (fromMaybe False $ fmap eitherToBool result)
	where
	eitherToBool (Left _) = False
	eitherToBool (Right _) = True
	p = liftIO . p'

doPing :: Jid -> Session -> IO (Either IQSendError NominalDiffTime)
doPing jid s = do
	t1 <- getCurrentTime
	r <- sendIQ' (Just 3000000) (Just jid) Get Nothing el [] s
	case r of
		Right _ -> do -- Even an error response can be used as a ping result
			t2 <- getCurrentTime
			return $ Right $ diffUTCTime t2 t1
		Left e -> return $ Left e
	where
	el = Element (Name (T.pack "ping") (Just pingNS) Nothing) [] []
