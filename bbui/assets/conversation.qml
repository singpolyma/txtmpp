import bb.cascades 1.0

import "prettyDate.js" as PrettyDate
import "jid.js" as JID

Page {
	property variant participants: []
	property variant threadID: ""
	property variant accountJid: ""
	property variant otherSide: ""

	function newMessage(subject, body, fromJid, updated) {
		messages.append([{body: body, updated: updated, fromJid: fromJid}]);
		messagesView.scrollToPosition(ScrollPosition.End, ScrollAnimation.None);

		if(subject && subject != '') {
			// TODO: empty subject is not the same as no subject
			subjectLabel.text = subject;
		}
	}

	function newParticipant(fn) {
		// This hack is because Qt4 properties cannot be real arrays
		// TODO: display nickname? make sure list is unique?
		var ps = participants;
		ps.push(fn);
		participants = ps;
		participantLabel.text = "With: " + participants.join(", ");
	}

	Container {
		Label {
			id: subjectLabel

			text: ""
			textStyle {
				base: SystemDefaults.TextStyles.TitleText
			}
			verticalAlignment: VerticalAlignment.Center
			multiline: true
		}

		Label {
			id: participantLabel

			text: "With: "
			textStyle {
				base: SystemDefaults.TextStyles.SubtitleText
			}
			verticalAlignment: VerticalAlignment.Center
			multiline: true
		}

		ListView {
			id: messagesView

			dataModel: ArrayDataModel {
				id: messages
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
						title: ListItemData.body
						description: ListItem.view.getNickname(ListItemData.fromJid)
						status: PrettyDate.format(ListItemData.updated)
					}
				}
			]
		}

		TextField {
			id: chatMessage
			inputMode: TextFieldInputMode.Chat
			verticalAlignment: VerticalAlignment.Center

			input {
				submitKey: SubmitKey.Send
				onSubmitted: {
					app.SendChat(accountJid, otherSide, threadID, chatMessage.text);
					chatMessage.text = '';
				}
			}
		}
	}
}
