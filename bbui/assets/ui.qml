import bb.cascades 1.2
import bb.system 1.2
import bb.cascades.datamanager 1.2

import "prettyDate.js" as PrettyDate
import "jid.js" as JID

NavigationPane {
	id: navigationPane
	property variant nicknames: {}
	property variant onPop

	Page {
		Container {
			ListView {
				id: conversationsView
				dataModel: dm

				function getNickname(jid) {
					return navigationPane.nicknames[JID.toBare(jid)] || JID.localpart(jid);
				}

				listItemComponents: [
					ListItemComponent {
						StandardListItem {
							title: ListItem.view.getNickname(ListItemData.otherSide)
							description: ListItemData.lastMessage
							status: PrettyDate.format(new Date(ListItemData.time * 1000))
						}
					}
				]

				onTriggered: {
					var conversation = dataModel.data(indexPath);
					var page = conversationDefinition.createObject();
					page.setup(conversation.jid, conversation.otherSide);
					navigationPane.push(page);
				}
			}

			attachedObjects: [
				AsyncDataModel {
					id: dm
					cacheSize: 20 // this is the default in-memory capacity

					query: SqlDataQuery {
						source: "file:///accounts/1000/appdata/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/data/.config/txtmpp/db.sqlite3"
						query: "SELECT ROWID as id, 1 AS revision_id, body AS lastMessage, MAX(strftime('%s', datetime(receivedAt))) AS time, COALESCE(otherSide_localpart, '') || '@' || otherSide_domainpart AS otherSide, COALESCE(to_localpart, '') || '@' || to_domainpart AS jid FROM messages WHERE body IS NOT NULL GROUP BY otherSide_localpart, otherSide_domainpart ORDER BY receivedAt DESC"
						countQuery: "SELECT COUNT(*) FROM (SELECT DISTINCT otherSide_localpart, otherSide_domainpart FROM messages WHERE body IS NOT NULL)"
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
					navigationPane.push(selectAccountDefinition.createObject());
					navigationPane.onPop = 'joinChatroom';
				}
			},
			ActionItem {
				title: "Edit Accounts"
				ActionBar.placement: ActionBarPlacement.OnBar
				onTriggered: {
					navigationPane.push(selectAccountDefinition.createObject());
					navigationPane.onPop = 'editAccount';
				}
			}
		]

		onCreationCompleted: {
			dm.load();

			app.NoAccounts.connect(function() {
				var page = updateAccountDefinition.createObject();
				page.title = "Login";
				navigationPane.push(page);
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
		if(navigationPane.onPop == 'joinChatroom' && page.selected && page.selected.jid) {
			chatroomPrompt.account = JID.toBare(page.selected.jid);
			chatroomPrompt.show();
		}

		if(navigationPane.onPop == 'editAccount' && page.selected && page.selected.jid) {
			var update = updateAccountDefinition.createObject();
			update.jid = page.selected.jid;
			update.password = page.selected.password;
			navigationPane.push(update);
		}

		navigationPane.onPop = null;
		page.destroy();
	}

	attachedObjects: [
		ComponentDefinition {
			id: conversationDefinition
			source: "messages.qml"
		},
		ComponentDefinition {
			id: updateAccountDefinition
			source: "UpdateAccount.qml"
		},
		ComponentDefinition {
			id: selectAccountDefinition
			source: "SelectAccount.qml"
		},
		ChatroomPrompt {
			id: chatroomPrompt
		},
		SystemDialog {
			id: errorDialog
			cancelButton.label: undefined
			title: "Error"
			body: ""
		}
	]
}
