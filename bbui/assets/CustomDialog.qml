import bb.cascades 1.2

Dialog {
	id: customDialog
	property alias title: titleLabel.text
	default property alias innerContent: innerContentContainer.controls

	Container {
		horizontalAlignment: HorizontalAlignment.Fill
		verticalAlignment: VerticalAlignment.Fill
		background: Color.create(0.0, 0.0, 0.0, 0.5)

		layout: DockLayout { }

		Container {
			maxHeight: ui.du(39.7)
			leftPadding: ui.du(3)
			rightPadding: leftPadding

			horizontalAlignment: HorizontalAlignment.Center
			verticalAlignment: VerticalAlignment.Center

			layout: DockLayout { }

			/*
			ImageView {
				imageSource: ""
				verticalAlignment: VerticalAlignment.Fill
			}
			*/

			// This Container contains the title and body of the dialog box.
			Container {
				topPadding: ui.du(.5)
				bottomPadding: ui.du(2)
				leftPadding: ui.du(2)

				horizontalAlignment: HorizontalAlignment.Fill
				verticalAlignment: VerticalAlignment.Fill

				Label {
					id: titleLabel
					text: ""
					textStyle.base: SystemDefaults.TextStyles.TitleText
					textStyle.color: Color.create("#fafafa")
					horizontalAlignment: HorizontalAlignment.Center
					layoutProperties: StackLayoutProperties {
						spaceQuota: 1
					}
				}

				Container {
					id: innerContentContainer
					horizontalAlignment: HorizontalAlignment.Center

					layoutProperties: StackLayoutProperties {
						spaceQuota: 3.5
					}
				}
			}
		}
	}
}
