module Conversations where

import Prelude ()
import BasicPrelude
import Control.Error

import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp
import qualified Network.Xmpp as Pontarius

import JidSql

readFromRow :: (Read a) => RowParser a
readFromRow = readZ =<< field

qs :: String -> Query
qs = Query . T.pack
{-# INLINE qs #-}

data Status = Active | Hidden deriving (Show, Read, Eq)
data Type   = Chat | GroupChat deriving (Show, Read, Eq)

data Conversation = Conversation {
		jid       :: Jid,
		otherSide :: Jid,
		typ       :: Type,
		status    :: Status,
		nickname  :: Text
	} deriving (Show, Eq)

instance FromRow Conversation where
	fromRow = Conversation <$>
		jidFromRow <*>
		jidFromRow <*>
		readFromRow <*>
		readFromRow <*>
		field

instance ToRow Conversation where
	toRow (Conversation jid otherSide status typ nick) = concat [
			jidToRow jid,
			jidToRow otherSide,
			[
				toField $ show status,
				toField $ show typ,
				toField nick
			]
		]

def :: Jid -> Jid -> MessageType -> Conversation
def ajid from Pontarius.GroupChat = Conversations.Conversation
	(toBare ajid) (let Just jid = jidFromTexts (localpart from) (domainpart from) (localpart ajid) in jid)
	Conversations.GroupChat Conversations.Active
	(fromMaybe (domainpart from) $ localpart from)
def ajid from _ = Conversations.Conversation
	(toBare ajid) (toBare from) Conversations.Chat Conversations.Active
	(fromMaybe (domainpart from) $ localpart from)

createTable :: (MonadIO m) => Connection -> EitherT SomeException m ()
createTable conn = syncIO $
	execute conn (qs $ "CREATE TABLE conversations (\
		\ " ++ jidSchema "jid" ++ ", \
		\ " ++ jidSchema "otherSide" ++ ", \
		\ `type` TEXT NOT NULL, \
		\ status TEXT NOT NULL, \
		\ nickname TEXT NOT NULL, \
		\ PRIMARY KEY (jid_localpart, jid_domainpart, jid_resourcepart, otherSide_localpart, otherSide_domainpart, otherSide_resourcepart) \
		\ ON CONFLICT ABORT \
		\ )") ()

-- | Insert new conversation
insert :: (MonadIO m) => Connection -> Bool -> Conversation -> m ()
insert conn ignoreExisting = liftIO . execute conn (qs$"INSERT " ++ ifNotExists ++ " INTO conversations VALUES (?,?,?,?,?,?,?,?,?)")
	where
	ifNotExists
		| ignoreExisting = "OR IGNORE"
		| otherwise      = ""

update :: (MonadIO m) => Connection -> Conversation -> m ()
update conn c = liftIO $ execute conn
	(qs$"UPDATE conversations SET `type`=?, status=?, nickname=? WHERE " ++ jidSql ++ " AND " ++ otherSideSql)
	([toField $ show $ typ c, toField $ show $ status c, toField $ nickname c] ++ jidFields ++ otherSideFields)
	where
	(jidSql, jidFields) = jidQuery False "jid" (jid c)
	(otherSideSql, otherSideFields) = jidQuery True "otherSide" (otherSide c)

getConversations :: (MonadIO m) => Connection -> Jid -> Type -> m [Conversation]
getConversations conn jid typ = liftIO $ query conn
	(qs"SELECT * FROM conversations WHERE (jid_localpart=? AND jid_domainpart=?) AND `type`=?")
	(localpart jid, domainpart jid, show typ)

getConversation :: (MonadIO m) => Connection -> Bool -> Jid -> Jid -> m (Maybe Conversation)
getConversation conn bareOtherSide jid otherSide = liftIO $ fmap listToMaybe $ query conn
	(qs$"SELECT * FROM conversations WHERE " ++ jidSql ++ " AND " ++ otherSideSql)
	(jidFields ++ otherSideFields)
	where
	(jidSql, jidFields) = jidQuery False "jid" jid
	(otherSideSql, otherSideFields) = jidQuery (not bareOtherSide) "otherSide" otherSide
