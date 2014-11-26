module DelayedDelivery (getDelay, delayXml, Delay(..)) where

import Data.Time (UTCTime)
import Control.Error (note)
import Data.Time.ISO8601 (formatISO8601, parseISO8601)
import Data.XML.Types (Element, Name(..))
import Data.XML.Pickle (UnpickleError, PU, xpUnliftElems, xpOption, xpWrap, xpPair, xpId, xpElem, xpPartial, xpContent, xpAttribute, xpAttribute', unpickle, pickle)
import qualified Data.Text as T

import Network.Xmpp

data Delay = Delay {
	stamp :: UTCTime,
	from :: Maybe Jid,
	reason :: Maybe T.Text
} deriving (Show, Read, Eq)

-- | Extract XEP-0203 delayed delivery from list of elements
getDelay :: [Element] -> Either UnpickleError (Maybe Delay)
getDelay = unpickle (xpOption xpDelay)

delayXml :: Delay -> [Element]
delayXml = pickle xpDelay

xpDelay :: PU [Element] Delay
xpDelay =
	xpWrap (\((s,f),r) -> Delay s f r) (\(Delay s f r) -> ((s,f), r)) $
	xpUnliftElems $ xpElem
	(inNS "delay")
	(xpPair
		(xpAttribute (noNS "stamp") (xpPartial (note dtErr . parseISO8601 . T.unpack) (T.pack . formatISO8601)))
		(xpAttribute' (noNS "from") (xpPartial (note jidErr . jidFromText) jidToText))
	)
	(xpOption (xpContent xpId))

dtErr :: T.Text
dtErr = T.pack "Bad date format"

jidErr :: T.Text
jidErr = T.pack "Bad JID format"

inNS :: String -> Name
inNS local = Name (T.pack local)
	(Just $ T.pack "urn:xmpp:delay") Nothing

noNS :: String -> Name
noNS local = Name (T.pack local) Nothing Nothing
