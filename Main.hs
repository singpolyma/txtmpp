module Main (main) where

import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (getArgs)
import Control.Concurrent
import Data.Maybe (listToMaybe, maybeToList, fromMaybe)
import Control.Monad
import Control.Monad.Trans
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import System.Log.Logger
import Network.Xmpp
import Network.Xmpp.IM
import Data.XML.Types

import qualified Data.UUID.V4 as UUID

import UI
import Types

authSession :: Jid -> Text -> IO (Either XmppFailure Session)
authSession (Jid (Just user) domain resource) pass =
	session (T.unpack domain)
	(Just ([
		plain user Nothing pass
	], resource))
	def
authSession _ _ = return $ Left XmppAuthFailure

message' :: Jid -> Jid -> MessageType -> Maybe (Text, Maybe Text) -> Maybe Text -> Text -> Message
message' from to typ thread subject body = withIM
	(Message Nothing (Just from) (Just to) Nothing typ [])
	InstantMessage {
		imSubject = fmap (MessageSubject Nothing) $ maybeToList subject,
		imBody = [MessageBody Nothing body],
		imThread = fmap (uncurry MessageThread) thread
	}

presenceStatus p = case (presenceType p, getIMPresence p) of
	(Unavailable, Just (IMP {showStatus = Nothing, status = s})) -> Just (Offline, s)
	(Available, Just (IMP {showStatus = Nothing, status = s})) -> Just (Online, s)
	(_, Just (IMP {showStatus = Just ss, status = s})) -> Just (SS ss, s)
	_ -> Nothing

presenceStream s = forever $ do
	-- Does not filter out ourselves or other instances of our account
	p <- waitForPresence (const True) s
	case (presenceFrom p, presenceStatus p) of
		(_,Nothing) -> return ()
		(Nothing,_) -> return ()
		(Just f, Just (ss,status)) ->
			-- f includes resource
			emit $ PresenceSet f ss status

ims jid s = forever $ do
	m <- getMessage s
	let Just otherJid = otherSide jid m
	let im = getIM m
	let subject = fmap subjectContent $ (listToMaybe . imSubject) =<< im
	let body = maybe (T.pack "") subjectContent ((listToMaybe . imSubject) =<< im)
	thread <- maybe (newThreadID jid) return (fmap theadID $ imThread =<< im)
	emit $ ChatMessage otherJid thread subject body

otherSide myjid (Message {messageFrom = from, messageTo = to})
	| from == Just myjid = to
	| otherwise = from

newThreadID jid = do
	uuid <- UUID.nextRandom
	return $ T.pack $ show uuid ++ show jid

signals presence jid s (SendChat tto mthread body) =
	case jidFromText tto of
		Just to -> do
			thread <- maybe (initPresence to >> newThreadID jid) return mthread
			sendMessage (message' jid to Chat (Just (thread, Nothing)) Nothing body) s
		_ -> emit $ Error $ show tto ++ " is not a valid JID"
	where
	initPresence to = do
		p <- readIORef presence
		sendPresence (p {presenceID = Nothing, presenceTo = Just to}) s

main = do
	[jid, pass] <- getArgs

	updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

	x <- authSession (read jid) (T.pack pass)

	case x of
		Left e -> error (show e)
		Right s -> do
			jid <- fmap (fromMaybe (read jid)) (getJid s)

			roster <- getRoster s
			mapM_ (\j -> emit $ PresenceSet j Offline Nothing) $ Map.keys (items roster)

			sendPresence initialPresence s
			presence <- newIORef initialPresence

			void $ forkIO (presenceStream =<< dupSession s)
			void $ forkIO (ims jid s)

			run (signals presence jid s)
	where
	initialPresence = ((withIMPresence (IMP Nothing (Just $ T.pack "woohoohere") (Just   12)) presenceOnline))
