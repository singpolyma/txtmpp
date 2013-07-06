import bb.cascades 1.0

Page {
	Container {
		Label {
			text: "Login"
			textStyle {
				base: SystemDefaults.TextStyles.TitleText
			}
			verticalAlignment: VerticalAlignment.Center
		}

		TextField {
			id: jid
			hintText: "Jabber ID"
			inputMode: TextFieldInputMode.EmailAddress
			verticalAlignment: VerticalAlignment.Center
		}

		TextField {
			id: password
			hintText: "Password"
			inputMode: TextFieldInputMode.Password
			verticalAlignment: VerticalAlignment.Center

			input {
				submitKey: SubmitKey.Connect
				onSubmitted: {
					app.UpdateAccount(jid.text, password.text);
					navigationPane.pop();
				}
			}
		}
	}

	paneProperties: NavigationPaneProperties {
		backButton: ActionItem {
			title: "Login"
			onTriggered: {
				app.UpdateAccount(jid.text, password.text);
				navigationPane.pop();
			}
		}
	}
}
