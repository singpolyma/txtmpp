import bb.cascades 1.0
import bb.system 1.0

import "prettyDate.js" as PrettyDate
import "jid.js" as JID

NavigationPane {
	id: navigationPane
	property variant nicknames: {}

	onCreationCompleted: {
		/* Init UI */

		app.invoked.connect(function(target, act, mimeType, uri, data) {
			if(mimeType == "application/x-xmpp-conversation") {
				for(var i = 0; i < conversations.size(); i++) {
					var val = conversations.value(i);
					if(val.threadID == data) {
						navigationPane.push(val.page);
					}
				}
			}
		});

		Application.awake.connect(function() {
			if(navigationPane.top.isOnTop) navigationPane.top.isOnTop();
		});

		Application.asleep.connect(function() {
			if(navigationPane.top.isNotOnTop) navigationPane.top.isNotOnTop();
		});

		app.Error.connect(function(msg) {
			console.log("ERROR: " + msg);
			errorDialog.body = msg;
			errorDialog.show();
		});

		app.NickSet.connect(function(jid, nickname) {
			// This hack is because Qt4 properties cannot be real objects
			var tmp = nicknames;
			tmp[JID.toBare(jid)] = nickname;
			nicknames = tmp;
		});

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
			conversation.updated = new Date();
			conversation.threadID = threadID;
			conversation.otherSide = otherSide;

			if(!conversation.page) {
				conversation.page = conversationDefinition.createObject();
				conversation.page.newParticipant(fromJid);
				conversation.page.accountJid = accountJid;
				conversation.page.threadID = threadID;
				conversation.page.otherSide = otherSide;
				// XXX: show self as participant as well?
			}

			conversations.insert(0, [conversation]);
			conversation.page.newMessage(subject, body, fromJid, conversation.updated);
		});

		app.NoAccounts.connect(function() {
			navigationPane.push(loginDefinition.createObject());
		});

		app.PresenceSet.connect(function(accountJid, jid, ss, msg) {
			var otherSide = JID.toBare(jid);
			var found = false;

			for(var i = 0; i < conversations.size(); i++) {
				var val = conversations.value(i);
				if(val.otherSide == otherSide) found = true;
			}

			if(!found) {
				var page = conversationDefinition.createObject();
				page.newParticipant(jid);
				page.accountJid = accountJid;
				page.threadID = (jid + new Date()); // TODO
				page.otherSide = otherSide;

				conversations.insert(0, [{
					accountJid: accountJid,
					lastMessage: msg,
					updated: new Date(),
					threadID: page.threadID,
					otherSide: otherSide,
					page: page
				}]);
			}
		});

		app.Ready();
	}

	onTopChanged: {
		if(page.isOnTop) page.isOnTop();
	}

	onPopTransitionEnded: {
		if(page.isNotOnTop) page.isNotOnTop();
	}

	onNavigateToTransitionEnded: {
		for(var i = 0; i < pages.length; i++) {
			if(pages[i].isNotOnTop) pages[i].isNotOnTop();
		}
	}

	Menu.definition: MenuDefinition {
		actions: [
			ActionItem {
				title: "Join Chatroom"
				onTriggered: {
					chatroomPrompt.inputField.inputMode = SystemUiInputMode.Email;
					chatroomPrompt.show();
				}
			}
		]

		attachedObjects: [
			SystemPrompt {
				id: chatroomPrompt
				title: "Enter Chatroom Address"
				onFinished: {
					if(chatroomPrompt.buttonSelection() == chatroomPrompt.confirmButton) {
						app.JoinChatroom(chatroomPrompt.inputFieldTextEntry());
					}
				}
			}
		]
	}

	Page {
		Container {
			ListView {
				dataModel: ArrayDataModel {
					id: conversations
				}

				function getNickname(jid) {
					return navigationPane.nicknames[JID.toBare(jid)] || jid;
				}

				// Use a ListItemComponent to determine which property in the
				// data model is displayed for each list item
				listItemComponents: [
					ListItemComponent {
						type: ""

						StandardListItem {
							title: ListItem.view.getNickname(ListItemData.otherSide)
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
		SystemDialog {
			id: errorDialog
			cancelButton.label: undefined
			title: "Error"
			body: ""
		},
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
