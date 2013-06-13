import bb.cascades 1.0
import bb.system 1.0
import bb.platform 1.0

import "prettyDate.js" as PrettyDate

NavigationPane {
	id: navigationPane
	property bool shouldNotify: false

	onCreationCompleted: {
		/* Init UI */

		conversations.append([
			{lastMessage: "Well, that's how it goes.", fn: "Jesse Rogers", updated: new Date()},
			{fn: "Christopher Vollick", lastMessage: "Hello", updated: new Date(2013,05,01,9,9,9,9)},
			{fn: "Jim Murphy", lastMessage: "Lunch?", updated: new Date(2013,05,12,9,9,9,9)}
		]);
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
