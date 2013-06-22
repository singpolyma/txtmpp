import bb.cascades 1.0
import bb.system 1.0
import bb.platform 1.0

import "prettyDate.js" as PrettyDate

NavigationPane {
	id: navigationPane
	property bool shouldNotify: false

	onCreationCompleted: {
		/* Init UI */

		app.ChatMessage.connect(function(otherSide, threadID, fromJid, stanzaID, subject, body) {
			// TODO: think about what happens with a very long list
			for(var i = 0; i < conversations.size(); i++) {
				var val = conversations.value(i);
				if(val.threadID == threadID && val.otherSide == otherSide) {
					conversations.removeAt(i);
					break;
				}
			}

			conversations.insert(0, [{lastMessage: body, fn: fromJid, updated: new Date(), threadID: threadID, otherSide: otherSide}]);
		});
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

			}
		}
	}

	attachedObjects: [
	]
}
