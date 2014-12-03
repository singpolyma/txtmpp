import bb.cascades 1.2
import bb.system 1.2
import bb.cascades.datamanager 1.2
import haskades.qtnetwork 1.0

import "prettyDate.js" as PrettyDate
import "jid.js" as JID

NavigationPane {
	id: navigationPane
	property variant nicknames: {}

	Page {
		Container {
			ListView {
				id: conversationsView
				dataModel: dm

				listItemComponents: [
					ListItemComponent {
						StandardListItem {
							title: ListItemData.nickname
							description: ListItemData.lastMessage
							status: ListItemData.time * 1 > 0 ? PrettyDate.format(new Date(ListItemData.time * 1000)) : ''
						}
					}
				]

				onTriggered: {
					var selected = dataModel.data(indexPath);
					var conversation = conversationDefinition.createObject();
					conversation.setup(selected.jid, selected.otherSide);
					navigationPane.push(conversation);
				}
			}

			attachedObjects: [
				AsyncDataModel {
					id: dm
					cacheSize: 20 // this is the default in-memory capacity

					query: SqlDataQuery {
						source: "file:///accounts/1000/appdata/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/data/.config/txtmpp/db.sqlite3"
						query: "SELECT conversations.ROWID as id, 1 AS revision_id, nickname, body AS lastMessage, MAX(strftime('%s', datetime(receivedAt))) AS time, COALESCE(conversations.otherSide_localpart, '') || '@' || conversations.otherSide_domainpart || '/' || COALESCE(conversations.otherSide_resourcepart, '') AS otherSide, COALESCE(jid_localpart, '') || '@' || jid_domainpart AS jid FROM conversations LEFT JOIN (SELECT * FROM messages WHERE body IS NOT NULL) USING (otherSide_localpart, otherSide_domainpart, otherSide_resourcepart) GROUP BY conversations.jid_localpart, conversations.jid_domainpart, conversations.jid_resourcepart, conversations.otherSide_localpart, conversations.otherSide_domainpart, conversations.otherSide_resourcepart ORDER BY CASE WHEN receivedAt IS NULL THEN 1 ELSE 0 END, receivedAt DESC, nickname, jid_localpart"
						countQuery: "SELECT COUNT(*) FROM conversations"
						keyColumn: "id"
						revisionColumn: "revision_id"
						revisionQuery: "SELECT 1"
						onError: console.log("SQL query error: " + code + ", " + message)
					}
				}
			]
		}

		actions: [
			ActionItem {
				title: "Join Chatroom"
				ActionBar.placement: ActionBarPlacement.OnBar

				onTriggered: {
					navigationPane.push(chatroomAccountSelector);
				}
			},
			ActionItem {
				title: "Edit Accounts"
				ActionBar.placement: ActionBarPlacement.OnBar

				onTriggered: {
					navigationPane.push(editAccountSelector);
				}
			}
		]

		onCreationCompleted: {
			dm.load();

			var ignoreFirstFourNetworkChanges = 0;
			networkConfiguration.configurationChanged.connect(function(conf) {
				ignoreFirstFourNetworkChanges++;
				if(ignoreFirstFourNetworkChanges > 4) {
					app.NetworkChanged();
				}
			});

			networkConfiguration.onlineStateChanged.connect(function(state) {
				if(!state) app.NetworkChanged();
			});

			app.NoAccounts.connect(function() {
				accountUpdatePane.title = "Login"
				accountUpdatePane.save = "Login"
				navigationPane.push(accountUpdatePane);
			});

			app.AccountsChanged.connect(function() {
				editAccountSelector.refresh();
				chatroomAccountSelector.refresh();
			});

			app.Log.connect(function(msg) {
				console.log("Backend log: " + msg);
			});

			app.Error.connect(function(msg) {
				console.log("Backend error: " + msg);
				errorDialog.body = msg;
				errorDialog.show();
			});

			app.NickSet.connect(function(jid, nickname) {
				// This hack is because Qt4 properties cannot be real objects
				var tmp = nicknames;
				tmp[JID.toBare(jid)] = nickname;
				nicknames = tmp;
				dm.query.emitDataChanged(2);
			});

			app.ChatMessage.connect(function(accountJid, otherSide, threadID, fromJid, stanzaID, subject, body) {
				dm.query.emitDataChanged(2);
			});

			app.Ready();
		}
	}

	onPopTransitionEnded: {
		if(page.destroyOnPop) page.destroy();
	}

	attachedObjects: [
		ComponentDefinition {
			id: conversationDefinition
			source: "messages.qml"
		},
		AccountSelector {
			id: chatroomAccountSelector

			onSelected: {
				chatroomPrompt.account = JID.toBare(account.jid);
				chatroomPrompt.show();
				navigationPane.pop();
			}
		},
		ChatroomPrompt {
			id: chatroomPrompt
		},
		AccountSelector {
			id: editAccountSelector

			onSelected: {
				accountUpdatePane.jid = account.jid;
				accountUpdatePane.password = account.password;
				navigationPane.push(accountUpdatePane);
			}
		},
		AccountUpdatePane {
			id: accountUpdatePane

			onFinished: {
				accountUpdatePane.title = "Update Account";
				accountUpdatePane.save = "Save";
				accountUpdatePane.jid = "";
				accountUpdatePane.password = "";
				navigationPane.pop();
			}
		},
		QNetworkConfigurationManager {
			id: networkConfiguration
		},
		SystemDialog {
			id: errorDialog
			cancelButton.label: undefined
			title: "Error"
			body: ""
		}
	]
}
