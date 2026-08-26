namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Styles

module Bookmarks =

    type Model = {
        Bookmarks: FeedEvent list
    }

    type Msg =
        | AddBookmark of FeedEvent
        | RemoveBookmark of Nostra.EventId
        | Clear

    type ExternalMsg =
        | NoOp

    let init () = {
        Bookmarks = []
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | AddBookmark event ->
            let exists = model.Bookmarks |> List.exists (fun b -> b.Id = event.Id)
            if exists then
                model, NoOp
            else
                { model with Bookmarks = event :: model.Bookmarks }, NoOp

        | RemoveBookmark eventId ->
            { model with Bookmarks = model.Bookmarks |> List.filter (fun b -> b.Id <> eventId) }, NoOp

        | Clear ->
            { model with Bookmarks = [] }, NoOp

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.medium
            DockPanel.children [
                // Header
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.margin (Spacing.bottom 15.0)
                    DockPanel.children [
                        TextBlock.create (Attrs.heading @ [
                            TextBlock.dock Dock.Left
                            TextBlock.text $"Bookmarks ({model.Bookmarks.Length})"
                        ])
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Clear"
                            Button.isEnabled (model.Bookmarks.Length > 0)
                            Button.onClick (fun _ -> dispatch Clear)
                        ]
                    ]
                ]

                // Content
                if model.Bookmarks.IsEmpty then
                    StackPanel.create [
                        StackPanel.verticalAlignment VerticalAlignment.Center
                        StackPanel.horizontalAlignment HorizontalAlignment.Center
                        StackPanel.children [
                            TextBlock.create [
                                TextBlock.text "🔖"
                                TextBlock.fontSize 48.0
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                TextBlock.margin (Spacing.bottom 10.0)
                            ]
                            TextBlock.create [
                                TextBlock.text "No bookmarks yet"
                                TextBlock.foreground Colors.muted
                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                            ]
                            TextBlock.create [
                                TextBlock.text "Save notes to read later"
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
                                    for bookmark in model.Bookmarks do
                                        Border.create (Attrs.card @ [
                                            Border.child (
                                                DockPanel.create [
                                                    DockPanel.children [
                                                        Button.create [
                                                            Button.dock Dock.Right
                                                            Button.content "X"
                                                            Button.padding (Thickness(8.0, 4.0))
                                                            Button.onClick (fun _ -> dispatch (RemoveBookmark bookmark.Id))
                                                        ]
                                                        StackPanel.create [
                                                            StackPanel.orientation Orientation.Vertical
                                                            StackPanel.spacing 5.0
                                                            StackPanel.children [
                                                                TextBlock.create [
                                                                    TextBlock.text (
                                                                        bookmark.AuthorName
                                                                        |> Option.defaultValue (
                                                                            let npub = NostrService.formatAuthorId bookmark.Author
                                                                            npub[..15] + "...")
                                                                    )
                                                                    TextBlock.fontWeight Avalonia.Media.FontWeight.SemiBold
                                                                ]
                                                                TextBlock.create (Attrs.wrappedText @ [
                                                                    TextBlock.text bookmark.Content
                                                                ])
                                                                TextBlock.create (Attrs.tinyText @ [
                                                                    TextBlock.text (bookmark.CreatedAt.ToString("MMM dd, HH:mm"))
                                                                ])
                                                            ]
                                                        ]
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
