import bb.cascades 1.2
import bb.cascades.datamanager 1.2

import "jid.js" as JID

Page {
	id: selectAccount
	property variant selected

	Container {
		ListView {
			id: accountView
			dataModel: dm

			listItemComponents: [
				ListItemComponent {
					StandardListItem {
						title: JID.toBare(ListItemData.jid)
					}
				}
			]

			onTriggered: {
				selectAccount.selected = dataModel.data(indexPath);
				navigationPane.pop();
			}
		}

		attachedObjects: [
			AsyncDataModel {
				id: dm
				cacheSize: 20 // this is the default in-memory capacity

				query: SqlDataQuery {
					source: "file:///accounts/1000/appdata/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/data/.config/txtmpp/db.sqlite3"
					query: "SELECT ROWID as id, 1 AS revision_id, COALESCE(localpart, '') || '@' || domainpart || '/' || COALESCE(resourcepart, '') AS jid, password FROM accounts ORDER BY localpart, domainpart"
					countQuery: "SELECT COUNT(*) FROM accounts"
					keyColumn: "id"
					revisionColumn: "revision_id"
					revisionQuery: "SELECT 1"
					onError: console.log("SQL query error: " + code + ", " + message)
				}
			}
		]
	}

	onCreationCompleted: {
		dm.load();
	}
}