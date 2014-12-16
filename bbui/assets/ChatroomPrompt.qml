import bb.cascades 1.2
import bb.system 1.2

SystemPrompt {
	property variant account

	title: "Join Chatroom"
	// Email input mode seems right, but makes entering spaces very hard
	// inputField.inputMode: SystemUiInputMode.Email

	onFinished: {
		if(result == SystemUiResult.ConfirmButtonSelection) {
			app.JoinMUC(account, inputFieldTextEntry());
		}
	}
}
