module Nick (getNick) where

import Data.XML.Types (Element, Name(..))
import Data.XML.Pickle (UnpickleError, PU, xpUnliftElems, xpElemText, unpickle)
import qualified Data.Text as T

-- | Extract XEP-0172 nickname from list of elements
getNick :: [Element] -> Either UnpickleError T.Text
getNick = unpickle xpNick

xpNick :: PU [Element] T.Text
xpNick = xpUnliftElems $ xpElemText nickElem

nickElem :: Name
nickElem = Name (T.pack "nick")
	(Just $ T.pack "http://jabber.org/protocol/nick") Nothing
