import bb.cascades 1.0

Page {
	property alias title: titleLabel.text
	property alias jid: jidField.text
	property alias password: passwordField.text

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
					navigationPane.pop();
				}
			}
		}
	}

	paneProperties: NavigationPaneProperties {
		backButton: ActionItem {
			title: "Login"
			onTriggered: {
				app.UpdateAccount(jid, password);
				navigationPane.pop();
			}
		}
	}
}
