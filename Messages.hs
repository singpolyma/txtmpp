module Messages where

import Prelude ()
import BasicPrelude
import Control.Error
import Data.Time (UTCTime)
import qualified Control.Applicative (empty)

import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp hiding (Message)

data Message = Message {
		from :: Jid, -- ^ Full jid this message came from
		to :: Jid,   -- ^ Full jid this message came to
		otherSide :: Jid, -- ^ Full jid of the other side (user or MUC)
		threadId :: Text,
		stanzaId :: Text,
		typ :: MessageType,
		subject :: Maybe Text,
		body :: Maybe Text,
		receivedAt :: UTCTime
	} deriving (Show, Eq)

jidFromRow :: RowParser Jid
jidFromRow = justZ =<< jidFromTexts <$> field <*> field <*> field

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
		field <*>
		field <*>
		field

jidToRow :: Jid -> [SQLData]
jidToRow jid = [
		toField $ localpart jid,
		toField $ domainpart jid,
		toField $ resourcepart jid
	]

instance ToRow Message where
	toRow (Message from to otherSide threadId stanzaId typ subject body receivedAt) = concat [
			jidToRow from,
			jidToRow to,
			jidToRow otherSide,
			[
				toField threadId,
				toField stanzaId,
				toField $ show typ,
				toField subject,
				toField body,
				toField receivedAt
			]
		]

qs :: String -> Query
qs = Query . T.pack
{-# INLINE qs #-}

jidSchema :: String -> String
jidSchema columnName =
	       columnName ++ "_localpart TEXT, \
	\ " ++ columnName ++ "_domainpart TEXT NOT NULL, \
	\ " ++ columnName ++ "_resourcepart TEXT"

createTable :: (MonadIO m) => Connection -> EitherT SomeException m ()
createTable conn = syncIO $
	execute conn (qs $ "CREATE TABLE messages (\
		\ " ++ jidSchema "from" ++ ", \
		\ " ++ jidSchema "to" ++ ", \
		\ " ++ jidSchema "otherSide" ++ ", \
		\ threadId TEXT NOT NULL, \
		\ stanzaId TEXT NOT NULL, \
		\ `type` TEXT NOT NULL, \
		\ subject TEXT, \
		\ body TEXT, \
		\ receivedAt TEXT NOT NULL, \
		\ PRIMARY KEY (otherSide_localpart, otherSide_domainpart, otherSide_resourcepart, threadId, stanzaId) \
		\ ON CONFLICT ABORT \
		\ )") ()

-- | Insert new message
insert :: (MonadIO m) => Connection -> Message -> EitherT SomeException m ()
insert conn = syncIO .
	execute conn (qs"INSERT INTO messages VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")
