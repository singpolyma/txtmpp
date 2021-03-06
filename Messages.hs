module Messages where

import Prelude ()
import BasicPrelude
import Control.Error
import Data.Time (UTCTime)

import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp hiding (Message, from)
import Network.Xmpp.IM hiding (status)
import qualified Network.Xmpp as Pontarius

import DelayedDelivery hiding (from)
import JidSql

data Message = Message {
		from :: Jid, -- ^ Full jid this message came from
		to :: Jid,   -- ^ Full jid this message came to
		otherSide :: Jid, -- ^ Full jid of the other side (user or MUC)
		threadId :: Text,
		stanzaId :: Text,
		typ :: MessageType,
		status :: Status,
		subject :: Maybe Text,
		body :: Maybe Text,
		receivedAt :: UTCTime
	} deriving (Show, Eq)

data Status = Received | ReceivedOld | Sent | Pending deriving (Show, Read, Eq)

readFromRow :: (Read a) => RowParser a
readFromRow = readZ =<< field

instance FromRow Message where
	fromRow = Message <$>
		jidFromRow <*>
		jidFromRow <*>
		jidFromRow <*>
		field <*>
		field <*>
		readFromRow <*>
		readFromRow <*>
		field <*>
		field <*>
		field

instance ToRow Message where
	toRow (Message from to otherSide threadId stanzaId typ status subject body receivedAt) = concat [
			jidToRow from,
			jidToRow to,
			jidToRow otherSide,
			[
				toField threadId,
				toField stanzaId,
				toField $ show typ,
				toField $ show status,
				toField subject,
				toField body,
				toField receivedAt
			]
		]

qs :: String -> Query
qs = Query . T.pack
{-# INLINE qs #-}

createTable :: (MonadIO m) => Connection -> EitherT SomeException m ()
createTable conn = syncIO $
	execute conn (qs $ "CREATE TABLE messages (\
		\ " ++ jidSchema "from" ++ ", \
		\ " ++ jidSchema "to" ++ ", \
		\ " ++ jidSchema "otherSide" ++ ", \
		\ threadId TEXT NOT NULL, \
		\ stanzaId TEXT NOT NULL, \
		\ `type` TEXT NOT NULL, \
		\ status TEXT NOT NULL, \
		\ subject TEXT, \
		\ body TEXT, \
		\ receivedAt TEXT NOT NULL, \
		\ PRIMARY KEY (otherSide_localpart, otherSide_domainpart, otherSide_resourcepart, threadId, stanzaId) \
		\ ON CONFLICT ABORT \
		\ )") ()

-- | Insert new message
insert :: (MonadIO m) => Connection -> Bool -> Message -> EitherT SomeException m ()
insert conn ignoreExisting = syncIO .
	execute conn (qs$"INSERT " ++ orIgnore ++ " INTO messages VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
	where
	orIgnore
		| ignoreExisting = "OR IGNORE"
		| otherwise = ""

updateStatus :: (MonadIO m) => Connection -> Jid -> Status -> Status -> m ()
updateStatus conn otherSide oldStatus newStatus = liftIO $ execute conn
	(qs$"UPDATE messages SET status=? WHERE status=? AND " ++ otherSideSql)
	([toField (show newStatus), toField (show oldStatus)] ++ otherSideFields)
	where
	(otherSideSql, otherSideFields) = jidQuery True "otherSide" otherSide

toXMPP :: Message -> Pontarius.Message
toXMPP (Message from to _ threadId stanzaId typ _ subject body _) = withIM
	(Pontarius.Message (Just stanzaId) (Just from) (Just to) Nothing typ [] [])
	InstantMessage {
		imSubject = MessageSubject Nothing <$> maybeToList subject,
		imBody    = MessageBody Nothing <$> maybeToList body,
		imThread  = Just $ MessageThread threadId Nothing
	}

resend :: (MonadIO m) => Connection -> (Either XmppFailure Session) -> Message -> EitherT SomeException m ()
resend db session msg@(Message { from = from, otherSide = otherSide, receivedAt = receivedAt }) = do
	result <- case session of
		Left  e -> return $! Left e
		Right s -> syncIO $ sendMessage xml s
	case result of
		Left XmppNoStream -> return () -- No status change
		Left e            -> throwT $ toException e
		Right ()          -> syncIO $ execute db
			(qs"UPDATE messages SET status=? WHERE otherSide_localpart=? AND otherSide_domainpart=? AND otherSide_resourcepart=? AND threadId=? AND stanzaId=?")
			(show Sent, localpart otherSide, domainpart otherSide, resourcepart otherSide, threadId msg, stanzaId msg)
	where
	xml = originalXML {
			Pontarius.messagePayload = Pontarius.messagePayload originalXML ++
				delayXml (Delay receivedAt (Just from) (Just $ T.pack "Connection issue"))
		}
	originalXML = toXMPP msg

send :: (MonadIO m) => Connection -> (Either XmppFailure Session) -> Message -> EitherT SomeException m ()
send db session msg = do
	result <- case session of
		Left  e -> return $! Left e
		Right s -> syncIO $ sendMessage (toXMPP msg) s
	case result of
		Left XmppNoStream -> Messages.insert db False (msg { status = Pending })
		Left e            -> throwT $ toException e
		Right ()          -> Messages.insert db False (msg { status = Sent })

getMessages :: (MonadIO m) => Connection -> Jid -> Status -> m [Message]
getMessages conn account status = liftIO $ query conn
	(qs"SELECT * FROM messages WHERE ((from_localpart=? AND from_domainpart=?) OR (to_localpart=? AND to_domainpart=?)) AND status=?")
	(localpart account, domainpart account, localpart account, domainpart account, show status)
