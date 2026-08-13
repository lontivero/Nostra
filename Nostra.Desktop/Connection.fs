namespace Nostra.Desktop

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Styles

module Connection =

    type Model = {
        RelayUrl: string
        Status: ConnectionStatus
    }

    type Msg =
        | UpdateUrl of string
        | Connect
        | StatusChanged of ConnectionStatus

    type ExternalMsg =
        | NoOp
        | ConnectRequested of relayUrl: string

    let init () = {
        RelayUrl = "wss://relay.damus.io"
        Status = Disconnected
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | UpdateUrl url ->
            { model with RelayUrl = url }, NoOp
        | Connect ->
            { model with Status = Connecting }, ConnectRequested model.RelayUrl
        | StatusChanged status ->
            { model with Status = status }, NoOp

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Top
            DockPanel.margin Spacing.small
            DockPanel.children [
                TextBox.create [
                    TextBox.dock Dock.Left
                    TextBox.width Dimensions.relayInputWidth
                    TextBox.text model.RelayUrl
                    TextBox.onTextChanged (UpdateUrl >> dispatch)
                ]
                Button.create [
                    Button.dock Dock.Left
                    Button.margin (Spacing.horizontal 10.0)
                    Button.content (
                        match model.Status with
                        | Disconnected -> "Connect"
                        | Connecting -> "Connecting..."
                        | Connected -> "Connected"
                    )
                    Button.isEnabled (model.Status = Disconnected)
                    Button.onClick (fun _ -> dispatch Connect)
                ]
                TextBlock.create [
                    TextBlock.dock Dock.Right
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Spacing.horizontal 10.0)
                    TextBlock.foreground (
                        match model.Status with
                        | Connected -> Colors.connected
                        | Connecting -> Colors.connecting
                        | Disconnected -> Colors.disconnected
                    )
                    TextBlock.text (
                        match model.Status with
                        | Connected -> "● Connected"
                        | Connecting -> "● Connecting"
                        | Disconnected -> "○ Disconnected"
                    )
                ]
            ]
        ]
