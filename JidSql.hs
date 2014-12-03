module JidSql where

import Prelude ()
import BasicPrelude
import Control.Error

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp

import qualified Data.Text as T

jidFromRow :: RowParser Jid
jidFromRow = justZ =<< jidFromTexts <$> emptyIsNoneF <*> field <*> emptyIsNoneF
	where
	emptyIsNoneF = fmap emptyIsNone field
	emptyIsNone x
		| T.null x = Nothing
		| otherwise = Just x

jidToRow :: Jid -> [SQLData]
jidToRow jid = [
		toField $ fromMaybe empty $ localpart jid,
		toField $ domainpart jid,
		toField $ fromMaybe empty $ resourcepart jid
	]

-- Optional parts still NOT NULL for unique index purposes -- we use empty string instead
jidSchema :: String -> String
jidSchema columnName =
	       columnName ++ "_localpart TEXT NOT NULL, \
	\ " ++ columnName ++ "_domainpart TEXT NOT NULL, \
	\ " ++ columnName ++ "_resourcepart TEXT NOT NULL"

jidQuery :: Bool -> String -> Jid -> (String, [SQLData])
jidQuery True c jid
	| Just _ <- localpart jid, Just _ <- resourcepart jid =
		(
			"("++c++"_localpart=? AND "++c++"_domainpart=? AND "++c++"_resourcepart=?)",
			jidToRow jid
		)
	| Just l <- localpart jid =
		(
			"("++c++"_localpart=? AND "++c++"_domainpart=? AND "++c++"_resourcepart IS NULL)",
			[toField l, toField $ domainpart jid]
		)
	| Just r <- resourcepart jid =
		(
			"("++c++"_localpart IS NULL AND "++c++"_domainpart=? AND "++c++"_resourcepart=?)",
			[toField $ domainpart jid, toField r]
		)
	| otherwise =
		(
			"("++c++"_localpart IS NULL AND "++c++"_domainpart=? AND "++c++"_resourcepart IS NULL)",
			[toField $ domainpart jid]
		)
jidQuery False c jid
	| Just l <- localpart jid =
		(
			"("++c++"_localpart=? AND "++c++"_domainpart=?)",
			[toField l, toField $ domainpart jid]
		)
	| otherwise =
		(
			"("++c++"_localpart IS NULL AND "++c++"_domainpart=?)",
			[toField $ domainpart jid]
		)
