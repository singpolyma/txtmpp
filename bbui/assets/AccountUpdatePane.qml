import bb.cascades 1.0

Page {
	id: pane

	property alias title: titleLabel.text
	property alias save: saveButton.title
	property alias jid: jidField.text
	property alias password: passwordField.text

	signal finished()

	Container {
		Label {
			id: titleLabel
			text: "Update Account"
			textStyle.base: SystemDefaults.TextStyles.TitleText
			verticalAlignment: VerticalAlignment.Center
		}

		TextField {
			id: jidField
			hintText: "Jabber ID"
			inputMode: TextFieldInputMode.EmailAddress
			verticalAlignment: VerticalAlignment.Center
		}

		TextField {
			id: passwordField
			hintText: "Password"
			inputMode: TextFieldInputMode.Password
			verticalAlignment: VerticalAlignment.Center

			input {
				submitKey: SubmitKey.Connect
				onSubmitted: {
					app.UpdateAccount(jid, password);
					finished();
				}
			}
		}
	}

	paneProperties: NavigationPaneProperties {
		backButton: ActionItem {
			id: saveButton

			title: "Save"
			onTriggered: {
				app.UpdateAccount(jid, password);
				finished();
			}
		}
	}
}
