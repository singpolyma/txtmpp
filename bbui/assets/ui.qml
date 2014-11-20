import bb.cascades 1.2
import bb.system 1.2
import bb.cascades.datamanager 1.2

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
					chatroomPrompt.open();
				}

				attachedObjects: [
					CustomDialog {
						id: chatroomPrompt
						title: "Join Chatroom"

						Picker {
							id: chatroomPromptAccount
							title: "Account"
							description: chatroomPromptAccount.selectedValue.jid

							pickerItemComponents: [
								PickerItemComponent {
									type: ""
									content: Container { Label { text: pickerItemData.jid } }
								}
							]

							dataModel: SimpleQueryDataModel {
								id: chatroomPromptDM
								query: SqlDataQuery {
									source: "file:///accounts/1000/appdata/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/data/.config/txtmpp/db.sqlite3"
									query: "SELECT ROWID as id, (COALESCE(localpart, '') || '@' || domainpart) AS jid FROM accounts"
									keyColumn: "id"
									onError: console.log("SQL query error: " + code + ", " + message)
								}
							}
						}

						TextField {
							id: chatroomPromptJid
							inputMode: TextFieldInputMode.EmailAddress
							hintText: "Chatroom Address"
						}

						Container {
							layout: StackLayout {
								orientation: LayoutOrientation.LeftToRight
							}

							Button {
								text: "Cancel"
								onClicked: {
									chatroomPromptJid.text = "";
									chatroomPrompt.close();
								}
							}

							Button {
								text: "Join"
								onClicked: {
console.log("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
console.log("items: " + chatroomPromptDM.totalSize());
console.log("data1: " + chatroomPromptDM.data([0]).jid);
console.log('hai' + chatroomPromptAccount.selectedValue.jid);
app.JoinMUC(chatroomPromptAccount.selectedValue.jid, chatroomPromptJid.text);
chatroomPrompt.close();
}
							}
						}
					}
				]
			}
		]

		onCreationCompleted: {
			dm.load();
			chatroomPromptDM.load();

			app.NoAccounts.connect(function() {
				navigationPane.push(loginDefinition.createObject());
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

	onPopTransitionEnded: { page.destroy(); }

	attachedObjects: [
		ComponentDefinition {
			id: conversationDefinition
			source: "messages.qml"
		},
		ComponentDefinition {
			id: loginDefinition
			source: "login.qml"
		},
		SystemDialog {
			id: errorDialog
			cancelButton.label: undefined
			title: "Error"
			body: ""
		}
	]
}
