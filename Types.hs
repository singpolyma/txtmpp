module Types where

import Data.Text (Text)
import Network.Xmpp (Jid)
import Network.Xmpp.IM (ShowStatus)

data Status = SS ShowStatus | Online | Offline
	deriving (Show)

data SignalToUI =
	NickSet Text Text |
	PresenceSet Text Text Text |
	SubscriptionRequest Text |
	ChatMessage Text Text Text Text Text Text | -- other side (user or MUC) threadID fromJid stanzaID subject body
	MessageErr Text | -- stanzaID of message that errored
	Error String
	deriving (Show)

data SignalFromUI =
	SendChat Text Text Text | -- other side (user or MUC) threadID body
	AcceptSubscription Text
	deriving (Read, Show)
