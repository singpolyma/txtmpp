import bb.cascades 1.0
import "prettyDate.js" as PrettyDate

Page {
	property variant participants: []
	property variant threadID: ""

	function newMessage(subject, body, updated) {
		messages.append([{body: body, updated: updated}]);
		if(subject && subject != '') {
			// TODO: empty subject is not the same as no subject
			subjectLabel.text = subject;
		}
	}

	function newParticipant(fn) {
		// This hack is because Qt4 properties cannot be real arrays
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
			dataModel: ArrayDataModel {
				id: messages
			}

			// Use a ListItemComponent to determine which property in the
			// data model is displayed for each list item
			listItemComponents: [
				ListItemComponent {
					type: ""

					StandardListItem {
						title: ListItemData.body
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
					app.SendChat(participants[0], threadID, chatMessage.text);
					chatMessage.text = '';
				}
			}
		}
	}
}
