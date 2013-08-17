module Accounts where

import Prelude (Show(..))
import BasicPrelude
import Control.Error
import qualified Control.Applicative (empty)

import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp

data Account = Account {
		jid :: Jid,
		password :: Text
	}

instance Show Account where
	show (Account jid _) = "Account " ++ T.unpack (jidToText jid) ++ " REDACTED"

-- Orphan instance should live upstream
instance MonadPlus RowParser where
	mplus = (<|>)
	mzero = Control.Applicative.empty

instance FromRow Account where
	fromRow = Account <$>
		(justZ =<< jidFromTexts <$> field <*> field <*> field) <*>
		field

instance ToRow Account where
	toRow (Account jid password) = [
			toField $ localpart jid,
			toField $ domainpart jid,
			toField $ resourcepart jid,
			toField password
		]

qs :: String -> Query
qs = Query . T.pack
{-# INLINE qs #-}

createTable :: (MonadIO m) => Connection -> EitherT SomeException m ()
createTable conn = syncIO $
	execute conn (qs"CREATE TABLE accounts (\
		\ localpart TEXT, \
		\ domainpart TEXT NOT NULL, \
		\ resourcepart TEXT, \
		\ password TEXT, \
		\ PRIMARY KEY (localpart, domainpart) \
		\ ON CONFLICT ABORT \
		\ )") ()

-- | Create or update account (unique index: bare JID)
update :: (MonadIO m) => Connection -> Account -> EitherT SomeException m ()
update conn = syncIO .
	execute conn (qs"INSERT OR REPLACE INTO accounts VALUES (?,?,?,?)")

remove :: (MonadIO m) => Connection -> Jid -> EitherT SomeException m ()
remove conn jid = syncIO $
	execute conn (qs"DELETE FROM accounts WHERE localpart = ? AND domainpart = ?")
		(localpart jid, domainpart jid)

get :: (MonadIO m) => Connection -> EitherT SomeException m [Account]
get conn = syncIO $
	query conn (qs "SELECT localpart, domainpart, resourcepart, password FROM accounts") ()
