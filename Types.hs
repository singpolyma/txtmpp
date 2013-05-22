module Types where

import Data.Text (Text)
import Network.Xmpp
import Network.Xmpp.IM
import Data.XML.Types

data Status = SS ShowStatus | Online | Offline
	deriving (Show)

data OutSignal =
	PresenceSet Jid Status (Maybe Text) |
	ChatMessage Jid Text Jid Text (Maybe Text) Text | -- other side (user or MUC) threadID fromJid stanzaID subject body
	Error String
	deriving (Show)

data InSignal =
	SendChat Text (Maybe Text) Text -- other side (user or MUC) threadID body
	deriving (Read, Show)
