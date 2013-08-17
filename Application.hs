{-# LANGUAGE CPP #-}
module Application (app) where

import Prelude ()
import BasicPrelude
import Data.Foldable (for_)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Either.Unwrap (unlessLeft)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Error (hush, runEitherT, EitherT(..), tryAssert, left, note, fmapLT, eitherT, hoistEither, assertZ)
import Data.Default (def)
import Filesystem (getAppConfigDirectory, createTree, isFile)
import qualified Data.Text as T
import qualified Data.Map as Map

import System.Log.Logger
import Network.Xmpp
import Network.Xmpp.Internal (StanzaID(..))
import Network.Xmpp.IM
import Network.DNS.Resolver (defaultResolvConf, ResolvConf(..), FileOrNumericHost(..))

import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQLite

import Types
import Nick
import Ping
import Disco
import qualified Accounts

import UIMODULE (emit)

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

presenceStream :: TChan RosterRequest -> TChan JidLockingRequest -> Jid -> Session -> IO ()
presenceStream rosterChan lockingChan accountJid s = forever $ do
	-- Does not filter out ourselves or other instances of our account
	p <- waitForPresence (const True) s

	for_ (NickSet . jidToText <$> presenceFrom p <*> hush (getNick $ presencePayload p)) emit

	for_ (presenceFrom p) (atomically . writeTChan lockingChan . JidUnlock)

	case (presenceFrom p, presenceType p) of
		(Just f, Subscribe) -> do
			subbed <- getRosterSubbed rosterChan f
			if subbed then void $ acceptSubscription f s else
				emit $ SubscriptionRequest $ jidToText f
		(_, _) -> return ()

	case (presenceFrom p, presenceStatus p) of
		(_,Nothing) -> return ()
		(Nothing,_) -> return ()
		(Just f, Just (ss,status)) ->
			-- f includes resource
			emit $ PresenceSet (jidToText $ toBare accountJid) (jidToText f) (show ss) (maybe T.empty show status)

messageErrors :: Session -> IO ()
messageErrors s = forever $ do
	m <- waitForMessageError (const True) s
	case messageErrorID m of
		Just sid -> emit $ MessageErr $ show sid
		Nothing -> return ()

ims :: TChan JidLockingRequest -> Jid -> Session -> IO ()
ims lockingChan jid s = forever $ do
	m <- getMessage s
	-- TODO: handle blank from/id ?  Inbound shouldn't have it, but shouldn't crash...
	let Just otherJid = otherSide jid m
	let Just from = messageFrom m
	let Just id = fmap show (messageID m)

	--unlessLeft (getNick $ messagePayload m) (emit . NickSet (jidToText from))

	unless (messageType m == GroupChat) $
		atomically $ writeTChan lockingChan $ JidMaybeLock from

	let im = getIM m
	let subject = fmap subjectContent $ (listToMaybe . imSubject) =<< im
	let body = fmap bodyContent $ (listToMaybe . imBody) =<< im
	case (subject, body) of
		(Nothing, Nothing) -> return () -- ignore completely empty message
		_ -> do
			thread <- maybe (newThreadID jid) return (fmap threadID $ imThread =<< im)
			emit $ ChatMessage (jidToText $ toBare jid) (jidToText otherJid) thread (jidToText from) id (maybe T.empty show subject) (fromMaybe (T.pack "") body)

otherSide :: Jid -> Message -> Maybe Jid
otherSide myjid (Message {messageFrom = from, messageTo = to})
	| from == Just myjid = fmap toBare to
	| otherwise = fmap toBare from

newThreadID :: Jid -> IO Text
newThreadID jid = do
	uuid <- UUID.nextRandom
	return $ show uuid ++ jidToText jid

jidOrError :: (MonadIO m) => Text -> (Jid -> m ()) -> m ()
jidOrError txt io =
	case jidFromText txt of
		Just jid -> io jid
		Nothing -> liftIO $ emit $ Error $ T.unpack txt ++ " is not a valid JID"

jidParse :: Text -> Either String Jid
jidParse txt = note (T.unpack txt ++ " is not a valid JID") (jidFromText txt)

connErr :: (Show e) => Jid -> e -> String
connErr jid e = "Connection for " ++ T.unpack (jidToText jid) ++ " failed with: " ++ T.unpack (show e)

signals :: TChan JidLockingRequest -> TChan ConnectionRequest -> SQLite.Connection -> SignalFromUI -> IO ()
signals _ connectionChan db (UpdateAccount jidt pass) = do
		eitherT (emit . Error . T.unpack . show) return $
			jidOrError jidt (Accounts.update db . (`Accounts.Account` pass))
		atomically $ writeTChan connectionChan RefreshAccounts
signals _ connectionChan db (RemoveAccount jidt) = do
		eitherT (emit . Error . T.unpack . show) return $
			jidOrError jidt (Accounts.remove db)
		atomically $ writeTChan connectionChan RefreshAccounts
signals _ connectionChan _ Ready =
		atomically $ writeTChan connectionChan RefreshAccounts
signals lockingChan connectionChan _ (SendChat taccountJid tto thread body) =
	eitherT (emit . Error) return $ do
		ajid <- hoistEither (jidParse taccountJid)
		to <- hoistEither (jidParse tto)
		s <- fmapLT (connErr ajid) $ EitherT $ syncCall connectionChan (GetSession ajid)
		jid <- fmapLT (connErr ajid) $ EitherT $ syncCall connectionChan (GetFullJid ajid)

		mid <- liftIO $ newStanzaId s
		to' <- liftIO $ syncCall lockingChan (JidGetLocked to)

		sent <- liftIO $ sendMessage (mkIM (Just mid) jid to' Chat (Just (thread, Nothing)) Nothing body) s
		tryAssert (connErr ajid XmppNoStream) sent

		liftIO $ emit $ ChatMessage (jidToText $ toBare ajid) (jidToText $ toBare to) thread (jidToText jid) (show mid) T.empty body
signals _ connectionChan _ (AcceptSubscription taccountJid jidt) =
	eitherT (emit . Error) return $ do
		ajid <- hoistEither (jidParse taccountJid)
		s <- fmapLT (connErr ajid) $ EitherT $ syncCall connectionChan (GetSession ajid)
		liftIO $ void $ acceptSubscription jid s
	where
	Just jid = jidFromText jidt

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

data ConnectionRequest =
	RefreshAccounts |
	GetSession Jid (TMVar (Either XmppFailure Session)) |
	GetFullJid Jid (TMVar (Either XmppFailure Jid))

data Connection = Connection {
		connectionSession :: Session,
		connectionJid :: Jid,
		connectionCleanup :: IO ()
	}

maybeConnect :: (MonadIO m) => TChan JidLockingRequest -> Accounts.Account -> Maybe (Either XmppFailure Connection) -> m (Either XmppFailure Connection)
maybeConnect lc (Accounts.Account jid pass) Nothing = liftIO $ runEitherT $ do
	session <- EitherT $ authSession jid pass
	jid' <- fmap (fromMaybe jid) (liftIO $ getJid session)

	-- Get roster and emit to UI
	roster <- liftIO $ getRoster session
	liftIO $ mapM_ (\(_,Item {jid = j, name = n}) -> do
			emit $ PresenceSet (jidToText $ toBare jid') (jidToText j) (T.pack "Offline") T.empty
			maybe (return ()) (emit . NickSet (jidToText j)) n
		) $ Map.toList (items roster)

	-- Now send presence, so that we get presence from others
	sent <- liftIO $ sendPresence initialPresence session
	tryAssert XmppNoStream sent

	-- Roster server
	rosterChan <- liftIO $ atomically newTChan
	rosterThread <- liftIO $ forkIO (rosterServer rosterChan (items roster))

	-- Stanza handling threads
	imThread <- liftIO $ forkIO (ims lc jid' session)
	errThread <- liftIO $ forkIO (messageErrors =<< dupSession session)
	pThread <- liftIO $ forkIO (presenceStream rosterChan lc jid' =<< dupSession session)

	disco <- liftIO $ startDisco (getRosterSubbed rosterChan) [Identity (T.pack "client") (T.pack "handheld") (Just $ T.pack APPNAME) Nothing] session
	case disco of
		Just disco -> do
			pingThread <- liftIO $ forkIO (void $ respondToPing (getRosterSubbed rosterChan) disco session)
			return $ Connection session jid' (do
					mapM_ killThread [imThread, errThread, pThread, rosterThread, pingThread]
					stopDisco disco
				)
		Nothing -> do
			liftIO $ mapM_ killThread [rosterThread, imThread, errThread, pThread]
			left XmppNoStream
	where
	initialPresence = withIMPresence (IMP Nothing (Just $ T.pack "woohoohere") (Just 12)) presenceOnline
maybeConnect lc a (Just (Left _)) = maybeConnect lc a Nothing
maybeConnect _ _ (Just x) = return x

lookupConnection :: (Functor m, Monad m) => Jid -> StateT (Map Jid (Either XmppFailure Connection)) m (Either XmppFailure Connection)
lookupConnection jid =
	fmap (fromMaybe (Left XmppNoStream) . Map.lookup (toBare jid)) get

connectionManager :: TChan ConnectionRequest -> TChan JidLockingRequest -> SQLite.Connection -> IO ()
connectionManager chan lockingChan db = void $ runStateT
	(forever $ liftIO (atomically $ readTChan chan) >>= msg) empty
	where
	msg RefreshAccounts = eitherT (emit . Error . T.unpack . show) return $ do
		oldAccounts <- lift get
		accounts <- Accounts.get db

		when (null accounts) (emit NoAccounts)

		-- Add any new accounts, and reconnect any failed accounts
		lift . put =<< foldM (\m a@(Accounts.Account jid _) -> do
				a' <- maybeConnect lockingChan a $ Map.lookup (toBare jid) oldAccounts
				return $! Map.insert (toBare jid) a' m
			) empty accounts

		-- Kill dead threads
		mapM_ (\k -> case Map.lookup k oldAccounts of
				Just (Right (Connection _ _ cleanup)) -> liftIO cleanup
				_ -> return ()
			) (Map.keys oldAccounts \\ map Accounts.jid accounts)
	msg (GetSession jid r) =
		lookupConnection jid >>= liftIO . atomically . putTMVar r . fmap connectionSession
	msg (GetFullJid jid r) =
		lookupConnection jid >>= liftIO . atomically . putTMVar r . fmap connectionJid

app :: IO (SignalFromUI -> IO ())
app = do
	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	dir <- getAppConfigDirectory (T.pack APPNAME)
	let dbPath = dir </> FilePath.fromText (T.pack "db.sqlite3")
	createTree dir
	dbExists <- isFile dbPath
	db <- SQLite.open (FilePath.encodeString dbPath)

	-- Create tables if the DB is new
	unless dbExists (Accounts.createTable db >>= assertZ)

	lockingChan <- atomically newTChan
	void $ forkIO (jidLockingServer lockingChan)

	connectionChan <- atomically newTChan
	void $ forkIO (connectionManager connectionChan lockingChan db)

	return (signals lockingChan connectionChan db)

{-
			-- TODO: Should do service disco, etc -- report to connection manager when connection has failed
			void $ forkIO (forever $ doPing (parseJid "singpolyma.net") s >>= print >> threadDelay 30000000)

-}
