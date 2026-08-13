namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nostra.Desktop.Common
open Styles

module Settings =

    type SettingsTab =
        | RelaysTab
        | AccountTab
        | AppearanceTab

    type Model = {
        CurrentTab: SettingsTab
        Relays: Relays.Model
    }

    type Msg =
        | SwitchTab of SettingsTab
        | RelaysMsg of Relays.Msg

    type ExternalMsg =
        | NoOp
        | ConnectToRelay of string
        | DisconnectFromRelay of string
        | FetchRelayInfo of string

    let init () = {
        CurrentTab = RelaysTab
        Relays = Relays.init ()
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | SwitchTab tab ->
            { model with CurrentTab = tab }, NoOp

        | RelaysMsg subMsg ->
            let newRelays, extMsg = Relays.update subMsg model.Relays
            let externalMsg =
                match extMsg with
                | Relays.NoOp -> NoOp
                | Relays.ConnectToRelay url -> ConnectToRelay url
                | Relays.DisconnectFromRelay url -> DisconnectFromRelay url
                | Relays.FetchRelayInfo url -> FetchRelayInfo url
            { model with Relays = newRelays }, externalMsg

    let private tabButton (currentTab: SettingsTab) (tab: SettingsTab) (label: string) dispatch =
        let isSelected = currentTab = tab
        Button.create [
            Button.content label
            Button.padding (Thickness(15.0, 8.0))
            Button.margin (Thickness(0.0, 0.0, 5.0, 0.0))
            Button.background (if isSelected then Colors.primaryButton else Colors.cardBackground)
            Button.foreground (if isSelected then Colors.buttonText else Colors.normalText)
            Button.onClick (fun _ -> dispatch (SwitchTab tab))
        ]

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.medium
            DockPanel.children [
                header "Settings"

                // Tab bar
                StackPanel.create [
                    StackPanel.dock Dock.Top
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.margin (Spacing.bottom 15.0)
                    StackPanel.children [
                        tabButton model.CurrentTab RelaysTab "Relays" dispatch
                        tabButton model.CurrentTab AccountTab "Account" dispatch
                        tabButton model.CurrentTab AppearanceTab "Appearance" dispatch
                    ]
                ]

                // Tab content
                Border.create [
                    Border.child (
                        match model.CurrentTab with
                        | RelaysTab ->
                            Relays.view model.Relays (RelaysMsg >> dispatch)

                        | AccountTab ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children [
                                    TextBlock.create (Attrs.subheading @ [
                                        TextBlock.text "Account"
                                    ])
                                    TextBlock.create [
                                        TextBlock.text "Account settings coming soon..."
                                        TextBlock.foreground Colors.muted
                                        TextBlock.margin (Spacing.top 10.0)
                                    ]
                                ]
                            ] :> IView

                        | AppearanceTab ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children [
                                    TextBlock.create (Attrs.subheading @ [
                                        TextBlock.text "Appearance"
                                    ])
                                    TextBlock.create [
                                        TextBlock.text "Appearance settings coming soon..."
                                        TextBlock.foreground Colors.muted
                                        TextBlock.margin (Spacing.top 10.0)
                                    ]
                                ]
                            ] :> IView
                    )
                ]
            ]
        ]
