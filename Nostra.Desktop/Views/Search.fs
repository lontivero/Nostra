namespace Nostra.Desktop

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Nostra
open Nostra.Desktop.Common
open Styles

module Search =

    type Model = {
        Query: string
        Result: SearchResult
    }

    type Msg =
        | UpdateQuery of string
        | Search
        | ProfileReceived of UserProfile * IsFollowed: bool
        | SearchFailed of string
        | Follow of AuthorId
        | Unfollow of AuthorId

    type ExternalMsg =
        | NoOp
        | SearchRequested of AuthorId
        | SearchNip05 of string
        | FollowRequested of AuthorId
        | UnfollowRequested of AuthorId

    let init () = {
        Query = ""
        Result = NotSearched
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | UpdateQuery query ->
            { model with Query = query }, NoOp

        | Search ->
            let query = model.Query.Trim()
            // Check if it's a NIP-05 identifier (contains @)
            if NostrService.isNip05Identifier query then
                { model with Result = Searching }, SearchNip05 query
            // Otherwise try parsing as npub or hex
            else
                match NostrService.parseAuthorId query with
                | Ok author ->
                    { model with Result = Searching }, SearchRequested author
                | Error e ->
                    { model with Result = NotFound e }, NoOp

        | ProfileReceived (profile, isFollowed) ->
            { model with Result = Found (profile, isFollowed) }, NoOp

        | SearchFailed error ->
            { model with Result = NotFound error }, NoOp

        | Follow author ->
            let updatedResult =
                match model.Result with
                | Found (profile, _) when NostrService.authorIdToBytes profile.AuthorId = NostrService.authorIdToBytes author ->
                    Found (profile, true)
                | other -> other
            { model with Result = updatedResult }, FollowRequested author

        | Unfollow author ->
            let updatedResult =
                match model.Result with
                | Found (profile, _) when NostrService.authorIdToBytes profile.AuthorId = NostrService.authorIdToBytes author ->
                    Found (profile, false)
                | other -> other
            { model with Result = updatedResult }, UnfollowRequested author

    let private searchInputView (model: Model) (isConnected: bool) dispatch =
        DockPanel.create [
            DockPanel.dock Dock.Top
            DockPanel.margin Spacing.small
            DockPanel.children [
                TextBox.create [
                    TextBox.dock Dock.Left
                    TextBox.width Dimensions.searchInputWidth
                    TextBox.text model.Query
                    TextBox.onTextChanged (UpdateQuery >> dispatch)
                    TextBox.onKeyDown (fun e ->
                        if e.Key = Avalonia.Input.Key.Enter then
                            dispatch Search
                    )
                ]
                Button.create [
                    Button.dock Dock.Left
                    Button.margin (Spacing.horizontal 10.0)
                    Button.content "Search"
                    Button.isEnabled (isConnected && model.Query.Length > 0)
                    Button.onClick (fun _ -> dispatch Search)
                ]
            ]
        ]

    let private searchResultView (model: Model) dispatch =
        Border.create [
            Border.margin Spacing.small
            Border.child (
                match model.Result with
                | NotSearched ->
                    TextBlock.create [
                        TextBlock.text "Search by npub, NIP-05 (user@domain.com), or hex public key"
                        TextBlock.foreground Colors.muted
                    ] :> IView
                | Searching ->
                    TextBlock.create [
                        TextBlock.text "Searching..."
                        TextBlock.foreground Colors.connecting
                    ] :> IView
                | NotFound error ->
                    TextBlock.create [
                        TextBlock.text $"Not found: {error}"
                        TextBlock.foreground Colors.error
                    ] :> IView
                | Found (profile, isFollowed) ->
                    Border.create (Attrs.card @ [
                        Border.child (
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 8.0
                                StackPanel.children [
                                    TextBlock.create (Attrs.heading @ [
                                        TextBlock.text (
                                            profile.DisplayName
                                            |> Option.orElse profile.Name
                                            |> Option.defaultValue "Unknown"
                                        )
                                    ])
                                    TextBlock.create (Attrs.caption @ Attrs.wrappedText @ [
                                        TextBlock.text (NostrService.formatAuthorId profile.AuthorId)
                                    ])
                                    if profile.Nip05.IsSome then
                                        TextBlock.create [
                                            TextBlock.foreground Colors.accent
                                            TextBlock.text $"NIP-05: {profile.Nip05.Value}"
                                        ]
                                    if profile.About.IsSome && not (String.IsNullOrWhiteSpace profile.About.Value) then
                                        TextBlock.create (Attrs.wrappedText @ [
                                            TextBlock.text profile.About.Value
                                            TextBlock.maxWidth Dimensions.maxContentWidth
                                        ])
                                    Button.create [
                                        Button.margin (Spacing.top 10.0)
                                        Button.content (if isFollowed then "Unfollow" else "Follow")
                                        Button.background (if isFollowed then Colors.secondaryButton else Colors.primaryButton)
                                        Button.foreground Colors.buttonText
                                        Button.onClick (fun _ ->
                                            if isFollowed then
                                                dispatch (Unfollow profile.AuthorId)
                                            else
                                                dispatch (Follow profile.AuthorId)
                                        )
                                    ]
                                ]
                            ]
                        )
                    ]) :> IView
            )
        ]

    let view (model: Model) (isConnected: bool) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.medium
            DockPanel.children [
                header "Search"
                StackPanel.create [
                    StackPanel.dock Dock.Top
                    StackPanel.children [
                        searchInputView model isConnected dispatch
                        searchResultView model dispatch
                    ]
                ]
            ]
        ]
