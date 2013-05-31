module Main (main) where

import Data.IORef (IORef, newIORef)
import System.Environment (getArgs)
import Control.Concurrent
import Data.Maybe (listToMaybe, maybeToList, fromMaybe)
import Control.Monad
import Data.Either.Unwrap (unlessLeft)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import System.Log.Logger
import Network.Xmpp
import Network.Xmpp.Internal (StanzaID(..))
import Network.Xmpp.IM

import qualified Data.UUID.V4 as UUID

import UI
import Types
import Nick
import Ping
import Disco

authSession :: Jid -> Text -> IO (Either XmppFailure Session)
authSession (Jid (Just user) domain resource) pass =
	session (T.unpack domain)
	(Just ([
		plain user Nothing pass
	], resource))
	def
authSession _ _ = return $ Left XmppAuthFailure

mkIM :: Maybe StanzaID -> Jid -> Jid -> MessageType -> Maybe (Text, Maybe Text) -> Maybe Text -> Text -> Message
mkIM id from to typ thread subject body = withIM
	(Message id (Just from) (Just to) Nothing typ [])
	InstantMessage {
		imSubject = fmap (MessageSubject Nothing) $ maybeToList subject,
		imBody = [MessageBody Nothing body],
		imThread = fmap (uncurry MessageThread) thread
	}

presenceStatus :: Presence -> Maybe (Status, Maybe Text)
presenceStatus p = case (presenceType p, getIMPresence p) of
	(Unavailable, Just (IMP {showStatus = Nothing, status = s})) -> Just (Offline, s)
	(Available, Just (IMP {showStatus = Nothing, status = s})) -> Just (Online, s)
	(_, Just (IMP {showStatus = Just ss, status = s})) -> Just (SS ss, s)
	_ -> Nothing

presenceStream :: Session -> IO ()
presenceStream s = forever $ do
	-- Does not filter out ourselves or other instances of our account
	p <- waitForPresence (const True) s
	case (presenceFrom p, presenceStatus p) of
		(_,Nothing) -> return ()
		(Nothing,_) -> return ()
		(Just f, Just (ss,status)) -> do
			-- f includes resource
			emit $ PresenceSet f ss status
			unlessLeft (getNick $ presencePayload p) (emit . NickSet f)

messageErrors :: Session -> IO ()
messageErrors s = forever $ do
	m <- waitForMessageError (const True) s
	case messageErrorID m of
		Just sid -> emit $ MessageErr $ T.pack $ show sid
		Nothing -> return ()

ims :: Jid -> Session -> IO ()
ims jid s = forever $ do
	m <- getMessage s
	-- TODO: handle blank from/id ?  Inbound shouldn't have it, but shouldn't crash...
	let Just otherJid = otherSide jid m
	let Just from = messageFrom m
	let Just id = fmap (T.pack . show) (messageID m)

	unlessLeft (getNick $ messagePayload m) (emit . NickSet from)

	let im = getIM m
	let subject = fmap subjectContent $ (listToMaybe . imSubject) =<< im
	let body = fmap bodyContent $ (listToMaybe . imBody) =<< im
	case (subject, body) of
		(Nothing, Nothing) -> return () -- ignore completely empty message
		_ -> do
			thread <- maybe (newThreadID jid) return (fmap theadID $ imThread =<< im)
			emit $ ChatMessage otherJid thread from id subject (fromMaybe (T.pack "") body)

otherSide :: Jid -> Message -> Maybe Jid
otherSide myjid (Message {messageFrom = from, messageTo = to})
	| from == Just myjid = to
	| otherwise = from

newThreadID :: Jid -> IO Text
newThreadID jid = do
	uuid <- UUID.nextRandom
	return $ T.pack $ show uuid ++ show jid

signals :: IORef Presence -> Jid -> Session -> InSignal -> IO ()
signals _ jid s (SendChat tto mthread body) =
	case jidFromText tto of
		Just to -> do
			thread <- maybe (newThreadID jid) return mthread
			mid <- newStanzaId s
			sendMessage (mkIM (Just mid) jid to Chat (Just (thread, Nothing)) Nothing body) s
			emit $ ChatMessage to thread jid (T.pack $ show mid) Nothing body
		_ -> emit $ Error $ show tto ++ " is not a valid JID"

newStanzaId :: Session -> IO StanzaID
newStanzaId s = do
	jid <- fmap (fromMaybe (Jid Nothing (T.pack "example.com") Nothing)) (getJid s)
	fmap StanzaID (newThreadID jid)

main :: IO ()
main = do
	[jid, pass] <- getArgs

	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	x <- authSession (read jid) (T.pack pass)

	case x of
		Left e -> error (show e)
		Right s -> do
			jid <- fmap (fromMaybe (read jid)) (getJid s)

			roster <- getRoster s
			mapM_ (\(_,Item {jid = j, name = n}) -> do
					emit $ PresenceSet j Offline Nothing
					maybe (return ()) (emit . NickSet j) n
				) $ Map.toList (items roster)

			sendPresence initialPresence s
			presence <- newIORef initialPresence

			void $ forkIO (presenceStream =<< dupSession s)
			void $ forkIO (messageErrors =<< dupSession s)
			void $ forkIO (ims jid s)
			Just disco <- startDisco (const $ return True) [Identity (T.pack "client") (T.pack "handheld") (Just $ T.pack "txtmpp") Nothing] s
			void $ forkIO (void $ respondToPing (const $ return True) disco s)

			-- Should do service disco, etc
			void $ forkIO (forever $ doPing (read "singpolyma.net") s >>= print >> threadDelay 30000000)

			run (signals presence jid s)
	where
	initialPresence = withIMPresence (IMP Nothing (Just $ T.pack "woohoohere") (Just   12)) presenceOnline
