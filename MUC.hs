module MUC (joinPresence) where

import Data.XML.Types (Element, Name(..))
import Data.XML.Pickle (PU, xpUnliftElems, xpElemBlank, pickle)
import qualified Data.Text as T

import Network.Xmpp

joinPresence :: Jid -> Presence
joinPresence jid = presenceOnline {
	presenceTo = Just jid,
	presencePayload = mucXml
}

mucXml :: [Element]
mucXml = pickle xpMUC ()

xpMUC :: PU [Element] ()
xpMUC = xpUnliftElems $ xpElemBlank
	(Name (T.pack "x") (Just $ T.pack "http://jabber.org/protocol/muc") Nothing)
