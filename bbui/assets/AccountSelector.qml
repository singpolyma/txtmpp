import bb.cascades 1.2
import bb.system 1.2
import bb.cascades.datamanager 1.2

import "jid.js" as JID

Page {
	id: selectAccount

	signal selected(variant account)

	function refresh() {
		dm.query.emitDataChanged(2);
	}

	Container {
		ListView {
			id: accountView
			dataModel: dm

			function removeAccount(account) {
				app.RemoveAccount(JID.toBare(account.jid));
			}

			listItemComponents: [
				ListItemComponent {
					StandardListItem {
						id: accountViewItem
						title: JID.toBare(ListItemData.jid)

						contextActions: [
							ActionSet {
								DeleteActionItem {
									title: "Remove account"

									onTriggered: {
										confirmDelete.show();
									}

									attachedObjects: [
										SystemDialog {
											id: confirmDelete
											title: "Confirm Removal"
											body: "Are you sure you want to remove this account?"
											confirmButton.label: "Yes"
											cancelButton.label: "No"

											onFinished: {
												if(result == SystemUiResult.ConfirmButtonSelection) {
													var account = accountViewItem.ListItem.view.dataModel.data(accountViewItem.ListItem.indexPath);
													accountViewItem.ListItem.view.removeAccount(account);
												}
											}
										}
									]
								}
							}
						]
					}
				}
			]

			onTriggered: {
				selectAccount.selected(dataModel.data(indexPath));
			}
		}

		attachedObjects: [
			AsyncDataModel {
				id: dm
				cacheSize: 20 // this is the default in-memory capacity

				query: SqlDataQuery {
					source: "file://" + app.homePath + "/.config/txtmpp/db.sqlite3"
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

	actions: [
		ActionItem {
			title: "Add Account"
			ActionBar.placement: ActionBarPlacement.OnBar

			onTriggered: {
				accountUpdatePane.title = "Add Account";
				navigationPane.push(accountUpdatePane);
			}
		}
	]

	onCreationCompleted: {
		dm.load();
	}
}