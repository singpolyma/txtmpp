{-# LANGUAGE CPP #-}
module Application (app) where

import Prelude ()
import BasicPrelude
import Data.Foldable (for_)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.State (StateT, runStateT, get, put, modify)
import Control.Error (hush, runEitherT, EitherT(..), left, note, readZ, rightZ, fmapLT, eitherT, hoistEither, throwT)
import Filesystem (getAppConfigDirectory, createTree, isFile)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import UnexceptionalIO (UnexceptionalIO, runUnexceptionalIO)
import qualified UnexceptionalIO
import qualified Data.Text as T
import qualified Data.Map as Map

import System.Log.Logger
import Network.Xmpp
import Network.Xmpp.IM
import Network.DNS.Resolver (defaultResolvConf, ResolvConf(..), FileOrNumericHost(..))

import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQLite

import Types
import Nick
import Ping
import Disco
import DelayedDelivery
import qualified Accounts
import qualified Messages

import UIMODULE (emit)

jidForUi :: Jid -> Text
jidForUi jid | (l,d,r) <- jidToTexts jid =
	fromMaybe empty l ++ T.pack "@" ++ d ++ T.pack "/" ++ fromMaybe empty r

jidFromUi :: Text -> Maybe Jid
jidFromUi tjid
	| T.last tjid == '/' = jidFromUi $ T.init tjid
	| otherwise = jidFromText tjid

authSession :: Jid -> Text -> (Session -> XmppFailure -> IO ()) -> [Plugin] -> IO (Either XmppFailure Session)
authSession jid pass onClosed plugins | isJust user' =
	session (T.unpack domain) (Just (sasl, resource)) (def {
		onConnectionClosed = onClosed,
		plugins = plugins,
		keepAlive = Nothing,
		sessionStreamConfiguration = def {resolvConf = defaultResolvConf {resolvInfo = RCFilePath "app/native/assets/resolv.conf"}}
	})
	where
	resource = resourcepart jid
	domain = domainpart jid
	Just user = user'
	user' = localpart jid
	sasl Secured = [scramSha1 user Nothing pass, plain user Nothing pass]
	sasl _ = [scramSha1 user Nothing pass]
authSession _ _ _ _ = return $ Left $ XmppAuthFailure AuthOtherFailure

presenceStatus :: Presence -> Maybe (Status, Maybe Text)
presenceStatus p = case (presenceType p, getIMPresence p) of
	(Unavailable, Just (IMP {showStatus = Nothing, status = s})) -> Just (Offline, s)
	(Available, Just (IMP {showStatus = Nothing, status = s})) -> Just (Online, s)
	(_, Just (IMP {showStatus = Just ss, status = s})) -> Just (SS ss, s)
	_ -> Nothing

acceptSubscription :: Jid -> Session -> IO (Either XmppFailure ())
acceptSubscription = sendPresence . presenceSubscribed

presenceStream :: TChan RosterRequest -> TChan JidLockingRequest -> Jid -> Session -> IO ()
presenceStream rosterChan lockingChan accountJid s = forever $ do
	-- Does not filter out ourselves or other instances of our account
	p <- waitForPresence (const True) s

	for_ (NickSet . jidForUi <$> presenceFrom p <*> hush (getNick $ presencePayload p)) emit

	for_ (presenceFrom p) (atomically . writeTChan lockingChan . JidUnlock)

	case (presenceFrom p, presenceType p) of
		(Just f, Subscribe) -> do
			subbed <- getRosterSubbed rosterChan f
			if subbed then void $ acceptSubscription f s else
				emit $ SubscriptionRequest $ jidForUi f
		(_, _) -> return ()

	case (presenceFrom p, presenceStatus p) of
		(_,Nothing) -> return ()
		(Nothing,_) -> return ()
		(Just f, Just (ss,status)) ->
			-- f includes resource
			emit $ PresenceSet (jidToText $ toBare accountJid) (jidForUi f) (show ss) (maybe T.empty show status)

messageErrors :: Session -> IO ()
messageErrors s = forever $ do
	m <- waitForMessageError (const True) s
	case messageErrorID m of
		Just sid -> emit $ MessageErr $ show sid
		Nothing -> return ()

ims :: TChan JidLockingRequest -> SQLite.Connection -> Jid -> Session -> IO ()
ims lockingChan db jid s = forever $ do
	m <- getMessage s
	defaultId <- fmap ((T.pack "noStanzaId-" ++) . show) UUID.nextRandom

	-- TODO: handle blank from/id ?  Inbound shouldn't have it, but shouldn't crash...
	let Just otherJid = otherSide jid m
	let Just from = messageFrom m
	let id = fromMaybe defaultId $ messageID m

	unless (messageType m == GroupChat) $
		atomically $ writeTChan lockingChan $ JidMaybeLock from

	let im = getIM m
	let subject = fmap subjectContent $ (listToMaybe . imSubject) =<< im
	let body = fmap bodyContent $ (listToMaybe . imBody) =<< im
	case (subject, body) of
		(Nothing, Nothing) -> return () -- ignore completely empty message
		_ -> do
			thread <- maybe (newThreadID jid) return (fmap threadID $ imThread =<< im)

			case getDelay (messagePayload m) of
				(Right (Just (Delay stamp _ _))) -> do
					[[dupe]] <- SQLite.query db (SQLite.Query $ T.pack "SELECT COUNT(1) FROM messages WHERE datetime(receivedAt) > datetime(?, '-10 seconds') AND datetime(receivedAt) < datetime(?, '+10 seconds') AND body=?") (stamp, stamp, body)

					when (dupe < (1 :: Int)) $ do
						eitherT (emit . Error . T.unpack . show) return $
							Messages.insert db $ Messages.Message from jid otherJid thread id (messageType m) Messages.Received (fmap show subject) body stamp
						emit $ ChatMessage (jidToText $ toBare jid) (jidForUi otherJid) thread (jidForUi from) id (fromMaybe T.empty subject) (fromMaybe T.empty body)
				_ -> do
					receivedAt <- getCurrentTime
					eitherT (emit . Error . T.unpack . show) return $
						Messages.insert db $ Messages.Message from jid otherJid thread id (messageType m) Messages.Received (fmap show subject) body receivedAt
					emit $ ChatMessage (jidToText $ toBare jid) (jidForUi otherJid) thread (jidForUi from) id (fromMaybe T.empty subject) (fromMaybe T.empty body)

-- TODO: do not toBare messages from a MUC alias, and maybe other cases?
-- otherSide represents what should be a seperate conversation view
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
	case jidFromUi txt of
		Just jid -> io jid
		Nothing -> liftIO $ emit $ Error $ T.unpack txt ++ " is not a valid JID"

jidParse :: Text -> Either String Jid
jidParse txt = note (T.unpack txt ++ " is not a valid JID") (jidFromUi txt)

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
signals _ connectionChan _ NetworkChanged =
	atomically $ writeTChan connectionChan ReconnectAll
signals lockingChan connectionChan db (SendChat taccountJid totherSide thread typ body) =
	eitherT (emit . Error) return $ do
		ajid <- hoistEither (jidParse taccountJid)
		otherSide <- hoistEither (jidParse totherSide)
		typ' <- readZ $ T.unpack typ
		s <- liftIO $ syncCall connectionChan (GetSession ajid)
		jid' <- liftIO $ syncCall connectionChan (GetFullJid ajid)
		jid <- case jid' of
			Left XmppNoStream -> return ajid
			Left e            -> throwT $ T.unpack $ show e
			Right jid         -> return jid

		-- Stanza id
		mid <- liftIO $ newThreadID jid
		thread' <- if thread == empty then liftIO (newThreadID jid) else return thread
		to <- liftIO $ syncCall lockingChan (JidGetLocked otherSide)

		receivedAt <- liftIO $ getCurrentTime

		fmapLT (T.unpack . show) $
			Messages.send db s $ Messages.Message jid to otherSide thread' mid typ' Messages.Pending Nothing (Just body) receivedAt

		liftIO $ emit $ ChatMessage (jidToText $ toBare ajid) (jidForUi $ toBare to) thread (jidForUi jid) (show mid) T.empty body
signals _ connectionChan _ (AcceptSubscription taccountJid jidt) =
	eitherT (emit . Error) return $ do
		ajid <- hoistEither (jidParse taccountJid)
		s <- fmapLT (connErr ajid) $ EitherT $ syncCall connectionChan (GetSession ajid)
		liftIO $ void $ acceptSubscription jid s
	where
	Just jid = jidFromUi jidt
signals _ connectionChan _ (JoinMUC taccountjid tmucjid) =
	case (jidFromUi taccountjid, jidFromUi tmucjid) of
		(Nothing, _) -> emit $ Error $ "Invalid account JID when joining MUC: " ++ T.unpack taccountjid
		(_, Nothing) -> emit $ Error $ "Invalid MUC JID: " ++ T.unpack tmucjid
		(Just accountjid, Just mucjid) -> do
			let mucjid' = case jidToTexts mucjid of
				(_, _, Just _) -> mucjid
				(l, d, Nothing) -> let Just jid = jidFromTexts l d (localpart accountjid) in jid
			maybeS <- syncCall connectionChan (GetSession accountjid)
			case maybeS of
				Left _ -> emit $ Error $ "Not connected"
				Right s -> joinMUC s mucjid'

joinMUC :: Session -> Jid -> IO ()
joinMUC s mucjid = eitherT (emit . Error . T.unpack . show) return $ EitherT $
	sendPresence (presenceOnline { presenceTo = Just mucjid }) s

data RosterRequest = RosterSubbed Jid (TMVar Bool)

rosterServer :: TChan RosterRequest -> Map Jid Item -> IO ()
rosterServer chan = next
	where
	next roster = atomically (readTChan chan) >>= msg
		where
		msg (RosterSubbed jid reply) = do
			atomically $ putTMVar reply $ case Map.lookup (toBare jid) roster of
				Just (Item {riApproved = True}) -> True
				Just (Item {riSubscription = From}) -> True
				Just (Item {riSubscription = Both}) -> True
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
	GetFullJid Jid (TMVar (Either XmppFailure Jid)) |
	Reconnect Jid |
	DoneClosing Jid |
	ReconnectAll |
	PingDone Jid NominalDiffTime |
	MaybePing

data Connection = Connection {
		connectionSession :: Session,
		connectionJid :: Jid,
		connectionCleanup :: UnexceptionalIO ()
	}

afterConnect :: SQLite.Connection -> Connection -> IO (Either XmppFailure (Jid, Roster))
afterConnect db (Connection session jid _) = do
	jid' <- fmap (fromMaybe jid) (getJid session)

	-- Get roster and emit to UI
	roster <- liftIO $ getRoster session
	liftIO $ mapM_ (\(_,Item {riJid = j, riName = n}) -> do
			emit $ PresenceSet (jidToText $ toBare jid') (jidForUi j) (T.pack "Offline") T.empty
			maybe (return ()) (emit . NickSet (jidForUi j)) n
		) $ Map.toList (items roster)

	-- Now send presence, so that we get presence from others
	result <- (fmap . fmap) (const (jid', roster)) $ sendPresence initialPresence session

	-- Re-join MUCs
	mapM_ (\(Messages.Conversation jid) ->
			let Just mjid = jidFromTexts (localpart jid) (domainpart jid) (localpart jid') in
			joinMUC session mjid
		) =<< Messages.getConversations db jid' GroupChat

	-- Re-try pending messages
	eitherT (emit . Error . T.unpack . show) return $ mapM_ (
			Messages.resend db (Right session)
		) =<< Messages.getMessages db jid' Messages.Pending

	return result
	where
	initialPresence = withIMPresence (IMP Nothing Nothing (Just 0)) presenceOnline

doReconnect :: SQLite.Connection -> Connection -> IO ()
doReconnect db c@(Connection session _ _) = do
	result <- reconnectNow session
	case result of
		Just _ ->  void $ reconnect' session
		Nothing -> return ()

	result <- afterConnect db c
	case result of
		Left _ -> doReconnect db c
		Right _ -> return ()

maybeConnect :: (MonadIO m) => TChan JidLockingRequest -> TChan ConnectionRequest -> SQLite.Connection -> Accounts.Account -> Maybe Connection -> m (Either XmppFailure Connection)
maybeConnect lc cc db (Accounts.Account jid pass) Nothing = liftIO $ runEitherT $ do
	session <- EitherT $ authSession jid pass (\s _ -> doReconnect db $ Connection s jid (return ()))
		-- Plugins to check if it's reasonable to ping every time we have the radio on anyway
		[(\out -> return $ Plugin'
			(\stanza anns -> atomically (writeTChan cc MaybePing) >> return [(stanza, anns)])
			(\stanza -> atomically (writeTChan cc MaybePing) >> out stanza)
			(const $ return ())
		)]
	(jid', roster) <- EitherT $ afterConnect db (Connection session jid (return ()))

	-- Roster server
	rosterChan <- liftIO $ atomically newTChan
	rosterThread <- liftIO $ forkIO (rosterServer rosterChan (items roster))

	-- Stanza handling threads
	imThread <- liftIO $ forkIO (ims lc db jid' session)
	errThread <- liftIO $ forkIO (messageErrors =<< dupSession session)
	pThread <- liftIO $ forkIO (presenceStream rosterChan lc jid' =<< dupSession session)

	disco <- liftIO $ startDisco (getRosterSubbed rosterChan) [Identity (T.pack "client") (T.pack "handheld") (Just $ T.pack APPNAME) Nothing] session
	case disco of
		Just disco -> do
			pingThread <- liftIO $ forkIO (void $ respondToPing (getRosterSubbed rosterChan) disco session)
			return $ Connection session jid' (void $ UnexceptionalIO.fromIO $ do -- Ignore exceptions
					mapM_ killThread [imThread, errThread, pThread, rosterThread, pingThread]
					stopDisco disco
					void $ sendPresence presenceOffline session
					endSession session
				)
		Nothing -> do
			liftIO $ mapM_ killThread [rosterThread, imThread, errThread, pThread]
			left XmppNoStream
maybeConnect _ _ _ _ (Just x) = return (Right x)

lookupConnection :: (Functor m, Monad m) => Jid -> StateT (Map Jid ConnectionManagerStateItem) m (Either XmppFailure Connection)
lookupConnection jid =
	fmap (fromMaybe (Left XmppNoStream) . fmap cmConnection . Map.lookup (toBare jid)) get

data ConnectionManagerStateItem = CM {
		cmConnection :: Either XmppFailure Connection,
		cmLastPing   :: UTCTime,
		cmLastPingR  :: NominalDiffTime,
		cmClosing    :: Bool
	}

connectionManager :: TChan ConnectionRequest -> TChan JidLockingRequest -> SQLite.Connection -> IO ()
connectionManager chan lockingChan db = void $ runStateT
	(forever $ liftIO (atomically $ readTChan chan) >>= msg) empty
	where
	msg RefreshAccounts = eitherT (emit . Error . T.unpack . show) return $ do
		emit AccountsChanged -- Probably, that's why we're here

		oldAccounts <- lift get
		accounts <- Accounts.get db

		when (null accounts) (emit NoAccounts)

		-- Add any new accounts, and reconnect any failed accounts
		lift . put =<< foldM (\m a@(Accounts.Account jid _) -> do
				a' <- maybeConnect lockingChan chan db a $ do
					oa <- rightZ =<< fmap cmConnection (Map.lookup (toBare jid) oldAccounts)
					guard (connectionJid oa == jid) -- do not reuse if resource has changed
					return $! oa
				return $! Map.insert (toBare jid) (CM a' (posixSecondsToUTCTime 0) 0 False) m
			) empty accounts

		-- Kill dead threads
		mapM_ (\k -> case Map.lookup k oldAccounts of
				Just (CM { cmConnection = Right (Connection _ _ cleanup) }) ->
					liftIO $ runUnexceptionalIO cleanup
				_ -> return ()
			) (Map.keys oldAccounts \\ map Accounts.jid accounts)

		emit AccountsChanged -- In case we made changes
	msg (GetSession jid r) =
		lookupConnection jid >>= liftIO . atomically . putTMVar r . fmap connectionSession
	msg (GetFullJid jid r) =
		lookupConnection jid >>= liftIO . atomically . putTMVar r . fmap connectionJid
	msg (DoneClosing jid) =
		modify (Map.adjust (\st -> st { cmClosing = False }) jid)
	msg (Reconnect jid) = do
		st <- Map.lookup (toBare jid) <$> get
		case st of
			Just (CM { cmConnection = (Right (Connection s _ _)), cmClosing = False }) -> do
				liftIO $ void $ forkIO $ (closeConnection s >> atomically (writeTChan chan $ DoneClosing jid))
				modify (Map.adjust (\st -> st { cmClosing = True }) jid)
			_ -> return ()
	msg ReconnectAll =
		get >>= mapM_ (liftIO . atomically . writeTChan chan . Reconnect) . Map.keys
	msg (PingDone jid r) =
		modify (Map.adjust (\st -> st { cmLastPingR = r }) jid)
	msg MaybePing = do
		time <- liftIO $ getCurrentTime
		put =<< (get >>= Map.traverseWithKey (\jid st@(CM c lastPing lastPingR closing) ->
				let Just serverJid = jidFromTexts Nothing (domainpart jid) Nothing in
				case c of
					Right (Connection s _ _) | diffUTCTime time lastPing > 30 -> do
						void $ liftIO $ forkIO $ do
							pingResult <- doPing serverJid s
							case pingResult of
								Left  _ -> atomically $ writeTChan chan (Reconnect jid)
								Right r -> atomically $ writeTChan chan (PingDone jid r)
						return $! CM c time lastPingR closing
					_ -> return st
			))

app :: IO (SignalFromUI -> IO ())
app = do
	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	dir <- getAppConfigDirectory (T.pack APPNAME)
	let dbPath = dir </> FilePath.fromText (T.pack "db.sqlite3")
	createTree dir
	dbExists <- isFile dbPath
	db <- SQLite.open (FilePath.encodeString dbPath)

	-- WAL mode means we can read and write at the same time
	SQLite.execute_ db $ SQLite.Query $ T.pack "PRAGMA journal_mode=WAL"

	-- Create tables if the DB is new
	unless dbExists $ eitherT (fail . T.unpack . show) return $ do
		Accounts.createTable db
		Messages.createTable db

	lockingChan <- atomically newTChan
	void $ forkIO (jidLockingServer lockingChan)

	connectionChan <- atomically newTChan
	void $ forkIO (connectionManager connectionChan lockingChan db)
	void $ forkIO (atomically (writeTChan connectionChan MaybePing) >> threadDelay 270000000)

	return (signals lockingChan connectionChan db)
