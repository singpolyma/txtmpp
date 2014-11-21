import bb.cascades 1.2
import bb.cascades.datamanager 1.2
import haskades.qtcore 1.0

import "prettyDate.js" as PrettyDate
import "jid.js" as JID

Page {
	id: messagesPage
	property variant accountJid

	function setup(accountJid, otherSide) {
		messagesPage.accountJid = accountJid;
		dm.query.bindValues = {
			"otherSide": otherSide
		};
		dm.load();
		scrollBottomTimer.start();
	}

	Container {
		Label {
			id: subjectLabel

			text: ""
			textStyle {
				base: SystemDefaults.TextStyles.TitleText
			}
			verticalAlignment: VerticalAlignment.Center
			visible: false
		}

		ListView {
			id: messagesView
			dataModel: dm

			function getNickname(jid, otherSide) {
				var nick = navigationPane.nicknames[JID.toBare(jid)];
				if(nick) return nick;
				if(!nick && jid == otherSide) {
					return JID.localpart(jid);
				} else {
					return JID.resourcepart(jid);
				}
			}

			listItemComponents: [
				ListItemComponent {
					Container {
						id: messageListItem

						layout: StackLayout {
							orientation: LayoutOrientation.LeftToRight
						}

						Container {
							topPadding: 20

							layoutProperties: StackLayoutProperties {
								spaceQuota: 1
							}

							Divider { }

							Label {
								multiline: true
								horizontalAlignment: HorizontalAlignment.Fill

								text: ListItemData.body
							}

							Container {
								layout: DockLayout {}
								horizontalAlignment: HorizontalAlignment.Fill

								Label {
									horizontalAlignment: HorizontalAlignment.Left
									verticalAlignment: VerticalAlignment.Fill
									textStyle {
										base: SystemDefaults.TextStyles.SmallText
										color: Color.LightGray
									}

									text: messageListItem.ListItem.view.getNickname(ListItemData.from, ListItemData.otherSide)
								}

								Label {
									horizontalAlignment: HorizontalAlignment.Right
									verticalAlignment: VerticalAlignment.Fill
									textStyle {
										base: SystemDefaults.TextStyles.SmallText
										color: Color.LightGray
									}

									text: PrettyDate.format(new Date(ListItemData.time * 1000))
								}
							}
						}
					}
				}
			]

			attachedObjects: [
				ListScrollStateHandler {
					id: messagesViewScrollState
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
					app.SendChat(accountJid, dm.query.bindValues["otherSide"], "", chatMessage.text);
					chatMessage.text = '';
				}
			}
		}


		attachedObjects: [
			// an example to demonstrate how to use an AsyncDataModel
			AsyncDataModel {
				id: dm
				cacheSize: 20 // this is the default in-memory capacity

				// can use any query that implements the DataQuery interface
				// SqlDataQuery is the default implementation provided with the library
				query: SqlDataQuery {
					source: "file:///accounts/1000/appdata/net.singpolyma.txtmpp.testDev_lyma_txtmpp4fc765cb/data/.config/txtmpp/db.sqlite3"
					query: "SELECT ROWID AS id, 1 AS revision_id, body, strftime('%s', datetime(receivedAt)) AS time, (COALESCE(from_localpart, '') || '@' || from_domainpart || '/' || COALESCE(from_resourcepart, '')) AS `from`, (COALESCE(otherSide_localpart, '') || '@' || otherSide_domainpart || '/' || COALESCE(otherSide_resourcepart, '')) AS otherSide FROM messages WHERE body IS NOT NULL AND (COALESCE(otherSide_localpart, '') || '@' || otherSide_domainpart) = :otherSide ORDER BY receivedAt"
					countQuery: "SELECT COUNT(*) FROM messages WHERE body IS NOT NULL AND (COALESCE(otherSide_localpart, '') || '@' || otherSide_domainpart) = :otherSide"
					keyColumn: "id"
					revisionColumn: "revision_id"
					revisionQuery:  "SELECT 1"
					onError: console.log("SQL query error: " + code + ", " + message)
				}

				onLoaded: console.log("initial model data is loaded")
			},

			QTimer {
				id: scrollBottomTimer
				interval: 150
				singleShot: true
			}
		]
	}

	onCreationCompleted: {
		app.ChatMessage.connect(function(accountJid, otherSide, threadID, fromJid, stanzaID, subject, body) {
			if(body) {
				if(messagesViewScrollState.atEnd) scrollBottomTimer.start();
				dm.query.emitDataChanged(2);
			}

			subjectLabel.text = subject;
			if(subjectLabel.text) subjectLabel.visible = true;
		});

		scrollBottomTimer.timeout.connect(function() {
			messagesView.scrollToPosition(ScrollPosition.End, ScrollAnimation.None);
		});
	}
}
