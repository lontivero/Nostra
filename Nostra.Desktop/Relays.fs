namespace Nostra.Desktop

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Nostra.Relay.InfoDocument
open Styles

module Relays =

    type RelayInfoState =
        | NotLoaded
        | Loading
        | Loaded of RelayInfo
        | LoadError of string

    type SelectedRelay = {
        Url: string
        Info: RelayInfoState
    }

    type Model = {
        Relays: RelayConfig list
        NewRelayUrl: string
        SelectedRelay: SelectedRelay option
    }

    type Msg =
        | AddRelay
        | RemoveRelay of string
        | UpdateNewRelayUrl of string
        | ToggleRelay of string
        | Connect of string
        | RelayStatusChanged of string * RelayStatus
        | ShowRelayInfo of string
        | CloseRelayInfo
        | RelayInfoLoaded of string * Result<RelayInfo, string>
        | AddRelayError of string * string

    type ExternalMsg =
        | NoOp
        | ConnectToRelay of string
        | DisconnectFromRelay of string
        | FetchRelayInfo of string

    let defaultRelays = [
        { Url = "wss://relay.damus.io"; Status = RelayDisconnected; Enabled = true; Errors = [] }
        { Url = "wss://relay.primal.net"; Status = RelayDisconnected; Enabled = true; Errors = [] }
        { Url = "wss://nos.lol"; Status = RelayDisconnected; Enabled = false; Errors = [] }
        { Url = "wss://relay.wasabiwallet.io"; Status = RelayDisconnected; Enabled = false; Errors = [] }
    ]

    let init () = {
        Relays = defaultRelays
        NewRelayUrl = ""
        SelectedRelay = None
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | AddRelay ->
            if model.NewRelayUrl.StartsWith("wss://") || model.NewRelayUrl.StartsWith("ws://") then
                let newRelay = { Url = model.NewRelayUrl; Status = RelayDisconnected; Enabled = true; Errors = [] }
                let exists = model.Relays |> List.exists (fun r -> r.Url = model.NewRelayUrl)
                if exists then
                    model, NoOp
                else
                    { model with
                        Relays = model.Relays @ [newRelay]
                        NewRelayUrl = "" }, NoOp
            else
                model, NoOp

        | RemoveRelay url ->
            { model with Relays = model.Relays |> List.filter (fun r -> r.Url <> url) }, NoOp

        | UpdateNewRelayUrl url ->
            { model with NewRelayUrl = url }, NoOp

        | ToggleRelay url ->
            let updatedRelays =
                model.Relays
                |> List.map (fun r ->
                    if r.Url = url then { r with Enabled = not r.Enabled }
                    else r)
            let relay = updatedRelays |> List.tryFind (fun r -> r.Url = url)
            let extMsg =
                match relay with
                | Some r when r.Enabled -> ConnectToRelay url
                | Some _ -> DisconnectFromRelay url
                | None -> NoOp
            { model with Relays = updatedRelays }, extMsg

        | Connect url ->
            let updatedRelays =
                model.Relays
                |> List.map (fun r ->
                    if r.Url = url then { r with Status = RelayConnecting }
                    else r)
            { model with Relays = updatedRelays }, ConnectToRelay url

        | RelayStatusChanged (url, status) ->
            let updatedRelays =
                model.Relays
                |> List.map (fun r ->
                    if r.Url = url then { r with Status = status }
                    else r)
            { model with Relays = updatedRelays }, NoOp

        | ShowRelayInfo url ->
            { model with SelectedRelay = Some { Url = url; Info = Loading } }, FetchRelayInfo url

        | CloseRelayInfo ->
            { model with SelectedRelay = None }, NoOp

        | RelayInfoLoaded (url, result) ->
            match model.SelectedRelay with
            | Some selected when selected.Url = url ->
                let info =
                    match result with
                    | Ok relayInfo -> Loaded relayInfo
                    | Error msg -> LoadError msg
                { model with SelectedRelay = Some { selected with Info = info } }, NoOp
            | _ -> model, NoOp

        | AddRelayError (url, errorMsg) ->
            let newError = { Timestamp = DateTime.Now; Message = errorMsg }
            let updatedRelays =
                model.Relays
                |> List.map (fun r ->
                    if r.Url = url then
                        // Keep only the last 100 errors
                        let errors = (newError :: r.Errors) |> List.truncate 100
                        { r with Errors = errors }
                    else r)
            { model with Relays = updatedRelays }, NoOp

    let private relayItemView (relay: RelayConfig) dispatch =
        Border.create [
            Border.margin (Thickness(0.0, 0.0, 0.0, 8.0))
            Border.padding Spacing.small
            Border.cornerRadius Radius.medium
            Border.background Colors.cardBackground
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        // Remove button
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "X"
                            Button.padding (Thickness(8.0, 4.0))
                            Button.margin (Thickness(5.0, 0.0, 0.0, 0.0))
                            Button.onClick (fun _ -> dispatch (RemoveRelay relay.Url))
                        ]

                        // Info button
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Info"
                            Button.padding (Thickness(8.0, 4.0))
                            Button.margin (Thickness(5.0, 0.0, 0.0, 0.0))
                            Button.onClick (fun _ -> dispatch (ShowRelayInfo relay.Url))
                        ]

                        // Connect button
                        Button.create [
                            Button.dock Dock.Right
                            Button.content (
                                match relay.Status with
                                | RelayDisconnected -> "Connect"
                                | RelayConnecting -> "..."
                                | RelayConnected -> "Connected"
                                | RelayError _ -> "Retry"
                            )
                            Button.isEnabled (relay.Enabled && relay.Status <> RelayConnecting && relay.Status <> RelayConnected)
                            Button.padding (Thickness(10.0, 4.0))
                            Button.onClick (fun _ -> dispatch (Connect relay.Url))
                        ]

                        // Toggle button (instead of checkbox)
                        Button.create [
                            Button.dock Dock.Left
                            Button.content (if relay.Enabled then "✓" else "○")
                            Button.padding (Thickness(5.0, 2.0))
                            Button.margin (Thickness(0.0, 0.0, 10.0, 0.0))
                            Button.onClick (fun _ -> dispatch (ToggleRelay relay.Url))
                        ]

                        // Status and URL
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.verticalAlignment VerticalAlignment.Center
                            StackPanel.children [
                                TextBlock.create (Attrs.relayStatusDot relay.Status @ [
                                    TextBlock.text (
                                        match relay.Status with
                                        | RelayConnected -> "●"
                                        | RelayConnecting -> "◐"
                                        | RelayDisconnected -> "○"
                                        | RelayError _ -> "✕"
                                    )
                                    TextBlock.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                                ])
                                TextBlock.create [
                                    TextBlock.text relay.Url
                                    TextBlock.foreground (if relay.Enabled then Colors.normalText else Colors.muted)
                                ]
                            ]
                        ]
                    ]
                ]
            )
        ]

    let private infoRowView (label: string) (value: string) =
        DockPanel.create [
            DockPanel.margin (Thickness(0.0, 4.0))
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.text label
                    TextBlock.foreground Colors.muted
                    TextBlock.width 150.0
                ]
                TextBlock.create (Attrs.wrappedText @ [
                    TextBlock.text value
                    TextBlock.foreground Colors.normalText
                ])
            ]
        ]

    let private errorItemView (error: RelayError) =
        Border.create [
            Border.margin (Thickness(0.0, 2.0))
            Border.padding (Thickness(8.0, 4.0))
            Border.cornerRadius Radius.small
            Border.background Colors.cardBackgroundAlt
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create (Attrs.tinyText @ [
                            TextBlock.dock Dock.Left
                            TextBlock.text (error.Timestamp.ToString("yyyy-MM-dd HH:mm:ss"))
                            TextBlock.width 140.0
                        ])
                        TextBlock.create (Attrs.wrappedText @ [
                            TextBlock.text error.Message
                            TextBlock.foreground Colors.error
                            TextBlock.fontSize FontSizes.small
                        ])
                    ]
                ]
            )
        ]

    let private relayInfoView (selected: SelectedRelay) (errors: RelayError list) dispatch =
        DockPanel.create [
            DockPanel.lastChildFill true
            DockPanel.children [
                // Header with back button
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.margin (Spacing.bottom 15.0)
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Left
                            Button.content "< Back"
                            Button.padding (Thickness(10.0, 5.0))
                            Button.onClick (fun _ -> dispatch CloseRelayInfo)
                        ]
                        TextBlock.create (Attrs.subheading @ [
                            TextBlock.text selected.Url
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.margin (Thickness(15.0, 0.0, 0.0, 0.0))
                        ])
                    ]
                ]

                // Content area
                ScrollViewer.create [
                    ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 10.0
                            StackPanel.children [
                                // Relay Info Section
                                TextBlock.create (Attrs.subheading @ [
                                    TextBlock.text "Relay Information (NIP-11)"
                                    TextBlock.margin (Spacing.bottom 5.0)
                                ])

                                Border.create (Attrs.card @ [
                                    Border.child (
                                        match selected.Info with
                                        | NotLoaded ->
                                            TextBlock.create [
                                                TextBlock.text "Not loaded"
                                                TextBlock.foreground Colors.muted
                                            ] :> IView
                                        | Loading ->
                                            TextBlock.create [
                                                TextBlock.text "Loading..."
                                                TextBlock.foreground Colors.muted
                                            ] :> IView
                                        | LoadError msg ->
                                            TextBlock.create [
                                                TextBlock.text $"Error: {msg}"
                                                TextBlock.foreground Colors.error
                                            ] :> IView
                                        | Loaded info ->
                                            StackPanel.create [
                                                StackPanel.orientation Orientation.Vertical
                                                StackPanel.children [
                                                    infoRowView "Name:" info.Name
                                                    infoRowView "Description:" info.Description
                                                    infoRowView "Software:" info.Software
                                                    infoRowView "Version:" info.Version
                                                    infoRowView "Contact:" info.Contact
                                                    infoRowView "Pubkey:" (if info.Pubkey.Length > 20 then info.Pubkey[..19] + "..." else info.Pubkey)
                                                    infoRowView "Supported NIPs:" (info.SupportedNips |> List.map string |> String.concat ", ")

                                                    // Limitations
                                                    TextBlock.create [
                                                        TextBlock.text "Limitations:"
                                                        TextBlock.foreground Colors.muted
                                                        TextBlock.margin (Thickness(0.0, 10.0, 0.0, 5.0))
                                                    ]
                                                    infoRowView "  Max Message Length:" (string info.Limitation.MaxMessageLength)
                                                    infoRowView "  Max Subscriptions:" (string info.Limitation.MaxSubscriptions)
                                                    infoRowView "  Max Filters:" (string info.Limitation.MaxFilters)
                                                    infoRowView "  Max Limit:" (string info.Limitation.MaxLimit)
                                                    infoRowView "  Auth Required:" (if info.Limitation.AuthRequired then "Yes" else "No")
                                                    infoRowView "  Payment Required:" (if info.Limitation.PaymentRequired then "Yes" else "No")
                                                ]
                                            ] :> IView
                                    )
                                ])

                                // Errors Section
                                TextBlock.create (Attrs.subheading @ [
                                    TextBlock.text $"Recent Errors ({List.length errors})"
                                    TextBlock.margin (Thickness(0.0, 20.0, 0.0, 5.0))
                                ])

                                if List.isEmpty errors then
                                    TextBlock.create [
                                        TextBlock.text "No errors recorded"
                                        TextBlock.foreground Colors.muted
                                        TextBlock.fontStyle Avalonia.Media.FontStyle.Italic
                                    ]
                                else
                                    Border.create (Attrs.card @ [
                                        Border.child (
                                            StackPanel.create [
                                                StackPanel.orientation Orientation.Vertical
                                                StackPanel.children [
                                                    for error in errors do
                                                        errorItemView error
                                                ]
                                            ]
                                        )
                                    ])
                            ]
                        ]
                    )
                ]
            ]
        ]

    let private relayListView (model: Model) dispatch =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 10.0
            StackPanel.children [
                TextBlock.create (Attrs.subheading @ [
                    TextBlock.text "Relays"
                    TextBlock.margin (Spacing.bottom 10.0)
                ])

                // Add new relay
                DockPanel.create [
                    DockPanel.margin (Spacing.bottom 15.0)
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Add"
                            Button.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                            Button.isEnabled (model.NewRelayUrl.Length > 0)
                            Button.onClick (fun _ -> dispatch AddRelay)
                        ]
                        TextBox.create [
                            TextBox.placeHolderText "wss://relay.example.com"
                            TextBox.text model.NewRelayUrl
                            TextBox.onTextChanged (UpdateNewRelayUrl >> dispatch)
                            TextBox.onKeyDown (fun e ->
                                if e.Key = Avalonia.Input.Key.Enter then
                                    dispatch AddRelay
                            )
                        ]
                    ]
                ]

                // Relay list
                ScrollViewer.create [
                    ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                for relay in model.Relays do
                                    relayItemView relay dispatch
                            ]
                        ]
                    )
                ]
            ]
        ]

    let view (model: Model) dispatch =
        match model.SelectedRelay with
        | Some selected ->
            let errors =
                model.Relays
                |> List.tryFind (fun r -> r.Url = selected.Url)
                |> Option.map (fun r -> r.Errors)
                |> Option.defaultValue []
            relayInfoView selected errors dispatch :> IView
        | None ->
            relayListView model dispatch :> IView
