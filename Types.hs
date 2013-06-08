module Types where

import Data.Text (Text)
import Network.Xmpp (Jid)
import Network.Xmpp.IM (ShowStatus)

data Status = SS ShowStatus | Online | Offline
	deriving (Show)

data OutSignal =
	NickSet Jid Text |
	PresenceSet Jid Status (Maybe Text) |
	SubscriptionRequest Jid |
	ChatMessage Jid Text Jid Text (Maybe Text) Text | -- other side (user or MUC) threadID fromJid stanzaID subject body
	MessageErr Text | -- stanzaID of message that errored
	Error String
	deriving (Show)

data InSignal =
	SendChat Text (Maybe Text) Text | -- other side (user or MUC) threadID body
	AcceptSubscription Jid
	deriving (Read, Show)
