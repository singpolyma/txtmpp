module Types where

import Data.Text (Text)
import Network.Xmpp.IM (ShowStatus)

data Status = SS ShowStatus | Online | Offline
	deriving (Show)

data SignalToUI =
	NickSet Text Text |
	PresenceSet Text Text Text |
	SubscriptionRequest Text |
	ChatMessage Text Text Text Text Text Text Text | -- AccountBareJid otherSide (user or MUC) threadID fromJid stanzaID subject body
	MessageErr Text | -- stanzaID of message that errored
	NoAccounts |
	Error String
	deriving (Show)

data SignalFromUI =
	Ready |
	UpdateAccount Text Text | -- Jid Password
	RemoveAccount Text | -- Jid
	SendChat Text Text Text Text | -- AccountBareJid otherSide (user or MUC) threadID body
	AcceptSubscription Text Text -- AccountBareJid otherSide
	deriving (Read, Show)
