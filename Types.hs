module Types where

import Data.Text (Text)
import Network.Xmpp.IM (ShowStatus)

data Status = SS ShowStatus | Online | Offline
	deriving (Show)

data SignalToUI =
	NickSet Text Text |
	PresenceSet Text Text Text Text | -- AccountBareJid JID status msg
	SubscriptionRequest Text |
	ChatMessage Text Text Text Text Text Text Text | -- AccountBareJid otherSide (user or MUC) threadID fromJid stanzaID subject body
	MessageErr Text | -- stanzaID of message that errored
	AccountsChanged |
	NoAccounts |
	Log Text |
	Error String
	deriving (Show)

data SignalFromUI =
	Ready |
	NetworkChanged |
	UpdateAccount Text Text | -- Jid Password
	RemoveAccount Text | -- Jid
	SendChat Text Text Text Text Text | -- AccountBareJid otherSide (user or MUC) threadID type body
	ChatActive Text Text | -- AccountBareJid otherSide
	AcceptSubscription Text Text | -- AccountBareJid otherSide
	JoinMUC Text Text -- AccountBareJid MucJid(with optional /nick)
	deriving (Read, Show)
