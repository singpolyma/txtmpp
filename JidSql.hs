module JidSql where

import Prelude ()
import BasicPrelude
import Control.Error

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.ToField (toField)
import Network.Xmpp

jidFromRow :: RowParser Jid
jidFromRow = justZ =<< jidFromTexts <$> field <*> field <*> field

jidToRow :: Jid -> [SQLData]
jidToRow jid = [
		toField $ localpart jid,
		toField $ domainpart jid,
		toField $ resourcepart jid
	]

jidSchema :: String -> String
jidSchema columnName =
	       columnName ++ "_localpart TEXT, \
	\ " ++ columnName ++ "_domainpart TEXT NOT NULL, \
	\ " ++ columnName ++ "_resourcepart TEXT"

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
