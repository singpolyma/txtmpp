import bb.cascades 1.0

import "prettyDate.js" as PrettyDate

NavigationPane {
	id: navigationPane
	property bool shouldNotify: false

	onCreationCompleted: {
		/* Init UI */

		app.ChatMessage.connect(function(accountJid, otherSide, threadID, fromJid, stanzaID, subject, body) {
			var conversation = {};

			// TODO: think about what happens with a very long list
			for(var i = 0; i < conversations.size(); i++) {
				var val = conversations.value(i);
				if(val.threadID == threadID && val.otherSide == otherSide) {
					conversation = val;
					conversations.removeAt(i);
					break;
				}
			}

			conversation.accountJid = accountJid;
			conversation.lastMessage = body;
			conversation.fn = fromJid;
			conversation.updated = new Date();
			conversation.threadID = threadID;
			conversation.otherSide = otherSide;

			if(!conversation.page) {
				conversation.page = conversationDefinition.createObject();
				conversation.page.newParticipant(fromJid);
				conversation.page.accountJid = accountJid;
				conversation.page.threadID = threadID;
				// XXX: show self as participant as well?
			}

			conversations.insert(0, [conversation]);
			conversation.page.newMessage(subject, body, conversation.updated);
		});

		app.NoAccounts.connect(function() {
			navigationPane.push(loginDefinition.createObject());
		});

		app.Ready();
	}

	Page {
		Container {
			ListView {
				dataModel: ArrayDataModel {
					id: conversations
				}

				// Use a ListItemComponent to determine which property in the
				// data model is displayed for each list item
				listItemComponents: [
					ListItemComponent {
						type: ""

						StandardListItem {
							title: ListItemData.fn
							description: ListItemData.lastMessage
							status: PrettyDate.format(ListItemData.updated)
						}
					}
				]

				onTriggered: {
					var conversation = dataModel.data(indexPath);
					navigationPane.push(conversation.page);
				}
			}
		}
	}

	attachedObjects: [
		ComponentDefinition {
			id: conversationDefinition
			source: "conversation.qml"
		},
		ComponentDefinition {
			id: loginDefinition
			source: "login.qml"
		}
	]
}
