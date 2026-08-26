namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Nostra.Desktop.Common
open Styles

module Notifications =

    type Model = {
        Notifications: FeedEvent list
    }

    type Msg =
        | NotificationReceived of FeedEvent
        | Clear

    type ExternalMsg =
        | NoOp

    let init () = {
        Notifications = []
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | NotificationReceived event ->
            let updated = event :: model.Notifications |> List.truncate 100
            { model with Notifications = updated }, NoOp

        | Clear ->
            { model with Notifications = [] }, NoOp

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.medium
            DockPanel.children [
                // Header
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.margin (Spacing.bottom 15.0)
                    DockPanel.children [
                        header $"Notifications ({model.Notifications.Length})"
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Clear"
                            Button.isEnabled (model.Notifications.Length > 0)
                            Button.onClick (fun _ -> dispatch Clear)
                        ]
                    ]
                ]

                // Content
                if model.Notifications.IsEmpty then
                    StackPanel.create [
                        StackPanel.verticalAlignment VerticalAlignment.Center
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children [
                            TextBlock.create [
                                TextBlock.text "🔔"
                                TextBlock.fontSize 48.0
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                TextBlock.margin (Spacing.bottom 10.0)
                            ]
                            TextBlock.create [
                                TextBlock.text "No notifications yet"
                                TextBlock.foreground Colors.muted
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                            ]
                            TextBlock.create [
                                TextBlock.text "Mentions and replies will appear here"
                                TextBlock.foreground Colors.muted
                                TextBlock.fontSize FontSizes.small
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                            ]
                        ]
                    ]
                else
                    ScrollViewer.create [
                        ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 8.0
                                StackPanel.children [
                                    for notif in model.Notifications do
                                        Border.create (Attrs.card @ [
                                            Border.child (
                                                StackPanel.create [
                                                    StackPanel.orientation Orientation.Vertical
                                                    StackPanel.spacing 5.0
                                                    StackPanel.children [
                                                        TextBlock.create [
                                                            TextBlock.text (
                                                                notif.AuthorName
                                                                |> Option.defaultValue (
                                                                    let npub = NostrService.formatAuthorId notif.Author
                                                                    npub[..15] + "...")
                                                            )
                                                            TextBlock.fontWeight Avalonia.Media.FontWeight.SemiBold
                                                        ]
                                                        TextBlock.create (Attrs.wrappedText @ [
                                                            TextBlock.text notif.Content
                                                        ])
                                                        TextBlock.create (Attrs.tinyText @ [
                                                            TextBlock.text (notif.CreatedAt.ToString("MMM dd, HH:mm"))
                                                        ])
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
