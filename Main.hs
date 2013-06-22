module Main (main) where

import Data.Foldable (for_)
import Control.Applicative
import Data.IORef (IORef, newIORef)
import System.Environment (getArgs)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Control.Monad
import Control.Error (hush)
import Data.Either.Unwrap (unlessLeft)
import Data.Default (def)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

import System.Log.Logger
import Network.Xmpp
import Network.Xmpp.Internal (StanzaID(..))
import Network.Xmpp.IM
import Network.DNS.Resolver (defaultResolvConf, ResolvConf(..), FileOrNumericHost(..))

import qualified Data.UUID.V4 as UUID

import UI
import Types
import Nick
import Ping
import Disco

authSession :: Jid -> Text -> IO (Either XmppFailure Session)
authSession jid pass | isJust user' =
	session (T.unpack domain) (Just (sasl, resource)) (def {sessionStreamConfiguration = def {resolvConf = defaultResolvConf {resolvInfo = RCHostName "8.8.8.8"}}})
	where
	resource = resourcepart jid
	domain = domainpart jid
	Just user = user'
	user' = localpart jid
	sasl Secured = [scramSha1 user Nothing pass, plain user Nothing pass]
	sasl _ = [scramSha1 user Nothing pass]
authSession _ _ = return $ Left $ XmppAuthFailure AuthOtherFailure

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

acceptSubscription :: Jid -> Session -> IO Bool
acceptSubscription = sendPresence . presenceSubscribed

presenceStream :: TChan RosterRequest -> TChan JidLockingRequest -> Session -> IO ()
presenceStream rosterChan lockingChan s = forever $ do
	-- Does not filter out ourselves or other instances of our account
	p <- waitForPresence (const True) s

	for_ (NickSet <$> presenceFrom p <*> hush (getNick $ presencePayload p)) emit

	for_ (presenceFrom p) (atomically . writeTChan lockingChan . JidUnlock)

	case (presenceFrom p, presenceType p) of
		(Just f, Subscribe) -> do
			subbed <- getRosterSubbed rosterChan f
			if subbed then void $ acceptSubscription f s else
				emit $ SubscriptionRequest f
		(_, _) -> return ()

	case (presenceFrom p, presenceStatus p) of
		(_,Nothing) -> return ()
		(Nothing,_) -> return ()
		(Just f, Just (ss,status)) ->
			-- f includes resource
			emit $ PresenceSet f ss status

messageErrors :: Session -> IO ()
messageErrors s = forever $ do
	m <- waitForMessageError (const True) s
	case messageErrorID m of
		Just sid -> emit $ MessageErr $ T.pack $ show sid
		Nothing -> return ()

ims :: TChan JidLockingRequest -> Jid -> Session -> IO ()
ims lockingChan jid s = forever $ do
	m <- getMessage s
	-- TODO: handle blank from/id ?  Inbound shouldn't have it, but shouldn't crash...
	let Just otherJid = otherSide jid m
	let Just from = messageFrom m
	let Just id = fmap (T.pack . show) (messageID m)

	unlessLeft (getNick $ messagePayload m) (emit . NickSet from)

	unless (messageType m == GroupChat) $
		atomically $ writeTChan lockingChan $ JidMaybeLock from

	let im = getIM m
	let subject = fmap subjectContent $ (listToMaybe . imSubject) =<< im
	let body = fmap bodyContent $ (listToMaybe . imBody) =<< im
	case (subject, body) of
		(Nothing, Nothing) -> return () -- ignore completely empty message
		_ -> do
			thread <- maybe (newThreadID jid) return (fmap threadID $ imThread =<< im)
			emit $ ChatMessage otherJid thread from id subject (fromMaybe (T.pack "") body)

otherSide :: Jid -> Message -> Maybe Jid
otherSide myjid (Message {messageFrom = from, messageTo = to})
	| from == Just myjid = fmap toBare to
	| otherwise = fmap toBare from

newThreadID :: Jid -> IO Text
newThreadID jid = do
	uuid <- UUID.nextRandom
	return $ T.pack $ show uuid ++ T.unpack (jidToText jid)

signals :: IORef Presence -> TChan JidLockingRequest -> Jid -> Session -> InSignal -> IO ()
signals _ lockingChan jid s (SendChat tto mthread body) =
	case jidFromText tto of
		Just to -> do
			thread <- maybe (newThreadID jid) return mthread
			mid <- newStanzaId s
			to' <- syncCall lockingChan (JidGetLocked to)
			sendMessage (mkIM (Just mid) jid to' Chat (Just (thread, Nothing)) Nothing body) s
			emit $ ChatMessage (toBare to) thread jid (T.pack $ show mid) Nothing body
		_ -> emit $ Error $ show tto ++ " is not a valid JID"
signals _ _ _ s (AcceptSubscription jid) = void $ acceptSubscription jid s

newStanzaId :: Session -> IO StanzaID
newStanzaId s = do
	jid <- fmap (fromMaybe dummyjid) (getJid s)
	fmap StanzaID (newThreadID jid)
	where
	Just dummyjid = jidFromTexts Nothing (T.pack "example.com") Nothing

data RosterRequest = RosterSubbed Jid (TMVar Bool)

rosterServer :: TChan RosterRequest -> Map Jid Item -> IO ()
rosterServer chan = next
	where
	next roster = atomically (readTChan chan) >>= msg
		where
		msg (RosterSubbed jid reply) = do
			atomically $ putTMVar reply $ case Map.lookup (toBare jid) roster of
				Just (Item {approved = True}) -> True
				Just (Item {subscription = From}) -> True
				Just (Item {subscription = Both}) -> True
				_ -> False
			next roster

data JidLockingRequest = JidMaybeLock Jid | JidUnlock Jid | JidGetLocked Jid (TMVar Jid)

jidLockingServer :: TChan JidLockingRequest -> IO ()
jidLockingServer chan = next Map.empty
	where
	next locks = atomically (readTChan chan) >>= msg
		where
		-- Lock if unlocked or passed jid is the same, unlock otherwise
		msg (JidMaybeLock jid) = next $ Map.alter (maybe (Just jid)
				(\exist -> if exist == jid then Just jid else Nothing)
			) (toBare jid) locks

		-- Always unlock
		msg (JidUnlock jid) = next $ Map.delete (toBare jid) locks

		-- Get the locked JID if there is one, or else the bare JID
		msg (JidGetLocked jid reply) = do
			let jid' = toBare jid
			atomically $ putTMVar reply (fromMaybe jid' $ Map.lookup jid' locks)
			next locks

syncCall' :: TChan a -> (TMVar b -> a) -> STM (TMVar b)
syncCall' chan cons = do
	reply <- newEmptyTMVar
	writeTChan chan (cons reply)
	return reply

syncCall :: TChan a -> (TMVar b -> a) -> IO b
syncCall chan cons = atomically (syncCall' chan cons) >>=
	atomically . takeTMVar

getRosterSubbed :: TChan RosterRequest -> Jid -> IO Bool
getRosterSubbed chan jid = syncCall chan (RosterSubbed jid)

main :: IO ()
main = do
	[jid, pass] <- getArgs

	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	x <- authSession (parseJid jid) (T.pack pass)

	case x of
		Left e -> error (show e)
		Right s -> do
			jid <- fmap (fromMaybe (parseJid jid)) (getJid s)

			rosterChan <- atomically newTChan
			roster <- getRoster s
			mapM_ (\(_,Item {jid = j, name = n}) -> do
					emit $ PresenceSet j Offline Nothing
					maybe (return ()) (emit . NickSet j) n
				) $ Map.toList (items roster)
			void $ forkIO (rosterServer rosterChan (items roster))

			lockingChan <- atomically newTChan
			void $ forkIO (jidLockingServer lockingChan)

			sendPresence initialPresence s
			presence <- newIORef initialPresence

			void $ forkIO (presenceStream rosterChan lockingChan =<< dupSession s)
			void $ forkIO (messageErrors =<< dupSession s)
			void $ forkIO (ims lockingChan jid s)
			Just disco <- startDisco (getRosterSubbed rosterChan) [Identity (T.pack "client") (T.pack "handheld") (Just $ T.pack "txtmpp") Nothing] s
			void $ forkIO (void $ respondToPing (getRosterSubbed rosterChan) disco s)

			-- Should do service disco, etc
			void $ forkIO (forever $ doPing (parseJid "singpolyma.net") s >>= print >> threadDelay 30000000)

			run (signals presence lockingChan jid s)
	where
	initialPresence = withIMPresence (IMP Nothing (Just $ T.pack "woohoohere") (Just   12)) presenceOnline
