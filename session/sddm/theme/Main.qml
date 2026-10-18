import QtQuick 2.0
import SddmComponents 2.0

Rectangle {
  id: root
  width: 640
  height: 480
  color: "#1a1b26"

  property string currentUser: userModel.lastUser
  property bool loginFailed: false

  // Default session: last chosen if remembered, otherwise Hyprland (uwsm-managed)
  property int sessionIndex: {
    if (sessionModel.lastIndex >= 0)
      return sessionModel.lastIndex
    for (var i = 0; i < sessionModel.rowCount(); i++) {
      var name = (sessionModel.data(sessionModel.index(i, 0), Qt.DisplayRole) || "").toString()
      if (name.indexOf("uwsm") !== -1)
        return i
    }
    return 0
  }

  Connections {
    target: sddm
    function onLoginFailed() {
      root.loginFailed = true
      password.text = ""
      password.forceActiveFocus()
    }
    function onLoginSucceeded() {
      root.loginFailed = false
    }
  }

  Column {
    anchors.centerIn: parent
    spacing: 40

    Image {
      id: logo
      source: "logo.png"
      width: Math.min(sourceSize.width, root.width * 0.8)
      height: sourceSize.width > 0 ? Math.round(width * sourceSize.height / sourceSize.width) : 0
      fillMode: Image.PreserveAspectFit
      anchors.horizontalCenter: parent.horizontalCenter
    }

    Column {
      anchors.horizontalCenter: parent.horizontalCenter
      spacing: 14

      Row {
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 15

        Image {
          source: root.loginFailed ? "lock-failed.png" : "lock.png"
          width: 34
          height: 38
          fillMode: Image.PreserveAspectFit
          anchors.verticalCenter: parent.verticalCenter
        }

        Item {
          width: entry.width
          height: entry.height

          Image {
            id: entry
            source: root.loginFailed ? "entry-failed.png" : "entry.png"
            anchors.centerIn: parent
          }

          Row {
            anchors.left: parent.left
            anchors.leftMargin: 20
            anchors.verticalCenter: parent.verticalCenter
            spacing: 5

            Repeater {
              model: Math.min(password.text.length, 21)

              Image {
                source: "bullet.png"
                width: 7
                height: 7
              }
            }
          }

          TextInput {
            id: password
            anchors.fill: parent
            anchors.leftMargin: 20
            anchors.rightMargin: 20
            verticalAlignment: TextInput.AlignVCenter
            echoMode: TextInput.Password
            font.family: "JetBrainsMono Nerd Font"
            font.pixelSize: 24
            font.letterSpacing: 5
            passwordCharacter: "•"
            color: "transparent"
            selectionColor: "transparent"
            selectedTextColor: "transparent"
            cursorDelegate: Item {}
            focus: true

            onTextChanged: root.loginFailed = false

            KeyNavigation.tab: sessionBox

            Keys.onPressed: (event) => {
              if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter) {
                sddm.login(root.currentUser, password.text, sessionBox.index)
                event.accepted = true
              } else if (event.key === Qt.Key_Escape) {
                password.text = ""
                root.loginFailed = false
                event.accepted = true
              }
            }
          }
        }
      }

      Row {
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 12

        Text {
          text: "session ▾"
          color: "#565f89"
          font.family: "JetBrainsMono Nerd Font"
          font.pixelSize: 13
          anchors.verticalCenter: parent.verticalCenter
        }

        ComboBox {
          id: sessionBox
          width: entry.width
          height: 36
          font.family: "JetBrainsMono Nerd Font"
          font.pixelSize: 13
          color: "#1a1b26"
          borderColor: "#565f89"
          focusColor: "#7aa2f7"
          hoverColor: "#7aa2f7"
          menuColor: "#1a1b26"
          textColor: "#c0caf5"
          model: sessionModel
          index: root.sessionIndex
          KeyNavigation.backtab: password

          onValueChanged: {
            root.sessionIndex = sessionBox.index
            password.forceActiveFocus()
          }
        }
      }
    }

  }

  Component.onCompleted: password.forceActiveFocus()
}
