namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Nostra
open Styles

module Home =

    type Model = {
        Feed: Feed.Model
        FollowedUsers: Map<byte[], UserProfile>
        ComposeText: string
    }

    type Msg =
        | FeedMsg of Feed.Msg
        | Follow of AuthorId
        | AddToFollowed of AuthorId  // Just adds to FollowedUsers, no external message
        | Unfollow of AuthorId
        | ProfileUpdated of byte[] * UserProfile
        | UpdateComposeText of string
        | SendNote

    type ExternalMsg =
        | NoOp
        | SubscribeToAuthor of AuthorId
        | RequestProfile of AuthorId
        | RequestEvents of EventId list
        | PublishNote of string

    let init () = {
        Feed = Feed.init ()
        FollowedUsers = Map.empty
        ComposeText = ""
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
        | FeedMsg subMsg ->
            let newFeed, extMsg = Feed.update subMsg model.Feed
            let externalMsg =
                match extMsg with
                | Feed.NoOp -> NoOp
                | Feed.RequestProfile author -> RequestProfile author
                | Feed.RequestEvents eventIds -> RequestEvents eventIds
                | Feed.SubscribeAuthor author -> SubscribeToAuthor author
            { model with Feed = newFeed }, externalMsg

        | Follow author ->
            let bytes = NostrService.authorIdToBytes author
            let existingProfile =
                model.FollowedUsers
                |> Map.tryFind bytes
                |> Option.defaultValue {
                    AuthorId = author
                    Name = None
                    DisplayName = None
                    About = None
                    Picture = None
                    Nip05 = None
                    IsFollowed = true
                }
            let updatedProfile = { existingProfile with IsFollowed = true }
            let updatedUsers = model.FollowedUsers |> Map.add bytes updatedProfile
            { model with FollowedUsers = updatedUsers }, SubscribeToAuthor author

        | AddToFollowed author ->
            let bytes = NostrService.authorIdToBytes author
            let existingProfile =
                model.FollowedUsers
                |> Map.tryFind bytes
                |> Option.defaultValue {
                    AuthorId = author
                    Name = None
                    DisplayName = None
                    About = None
                    Picture = None
                    Nip05 = None
                    IsFollowed = true
                }
            let updatedProfile = { existingProfile with IsFollowed = true }
            let updatedUsers = model.FollowedUsers |> Map.add bytes updatedProfile
            { model with FollowedUsers = updatedUsers }, NoOp

        | Unfollow author ->
            let bytes = NostrService.authorIdToBytes author
            let updatedUsers =
                model.FollowedUsers
                |> Map.change bytes (Option.map (fun p -> { p with IsFollowed = false }))
            { model with FollowedUsers = updatedUsers }, NoOp

        | ProfileUpdated (authorBytes, profile) ->
            let isFollowed =
                model.FollowedUsers
                |> Map.tryFind authorBytes
                |> Option.map (fun p -> p.IsFollowed)
                |> Option.defaultValue false

            if isFollowed then
                let updatedUsers = model.FollowedUsers |> Map.add authorBytes profile
                { model with FollowedUsers = updatedUsers }, NoOp
            else
                model, NoOp

        | UpdateComposeText text ->
            { model with ComposeText = text }, NoOp

        | SendNote ->
            if model.ComposeText.Trim().Length > 0 then
                let note = model.ComposeText
                { model with ComposeText = "" }, PublishNote note
            else
                model, NoOp

    let private composeAreaView (model: Model) dispatch =
        Border.create [
            Border.padding Spacing.small
            Border.margin (Spacing.bottom 10.0)
            Border.cornerRadius Radius.medium
            Border.background Colors.cardBackground
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        StackPanel.create [
                            StackPanel.dock Dock.Bottom
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.horizontalAlignment HorizontalAlignment.Right
                            StackPanel.margin (Thickness(0.0, 10.0, 0.0, 0.0))
                            StackPanel.children [
                                Button.create [
                                    Button.content "Cancel"
                                    Button.margin (Thickness(0.0, 0.0, 10.0, 0.0))
                                    Button.isEnabled (model.ComposeText.Length > 0)
                                    Button.onClick (fun _ -> dispatch (UpdateComposeText ""))
                                ]
                                Button.create (Attrs.primaryButton @ [
                                    Button.content "Send note"
                                    Button.isEnabled (model.ComposeText.Trim().Length > 0)
                                    Button.onClick (fun _ -> dispatch SendNote)
                                ])
                            ]
                        ]
                        TextBox.create [
                            TextBox.acceptsReturn true
                            TextBox.minHeight 60.0
                            TextBox.maxHeight 150.0
                            TextBox.placeHolderText "Type your message here..."
                            TextBox.text model.ComposeText
                            TextBox.onTextChanged (UpdateComposeText >> dispatch)
                        ]
                    ]
                ]
            )
        ]

    let private feedHeaderView (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin (Spacing.bottom 10.0)
            DockPanel.children [
                TextBlock.create (Attrs.subheading @ [
                    TextBlock.dock Dock.Left
                    TextBlock.text $"Feed ({model.Feed.Events.Length})"
                ])
            ]
        ]

    let private followedUsersPanelView (model: Model) dispatch =
        let followedUsers =
            model.FollowedUsers
            |> Map.toList
            |> List.filter (fun (_, p) -> p.IsFollowed)
            |> List.map snd

        Border.create [
            Border.dock Dock.Right
            Border.width Dimensions.followedPanelWidth
            Border.padding Spacing.small
            Border.background Colors.cardBackgroundAlt
            Border.child (
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create (Attrs.subheading @ [
                            TextBlock.dock Dock.Top
                            TextBlock.text $"Following ({followedUsers.Length})"
                            TextBlock.margin (Spacing.bottom 10.0)
                        ])

                        ScrollViewer.create [
                            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.spacing 5.0
                                    StackPanel.children [
                                        for user in followedUsers do
                                            Border.create (Attrs.cardSmall @ [
                                                Border.child (
                                                    DockPanel.create [
                                                        DockPanel.children [
                                                            Button.create [
                                                                Button.dock Dock.Right
                                                                Button.content "x"
                                                                Button.padding (Thickness(5.0, 0.0))
                                                                Button.onClick (fun _ -> dispatch (Unfollow user.AuthorId))
                                                            ]
                                                            TextBlock.create (Attrs.ellipsisText @ [
                                                                TextBlock.text (
                                                                    user.DisplayName
                                                                    |> Option.orElse user.Name
                                                                    |> Option.defaultValue (
                                                                        let npub = NostrService.formatAuthorId user.AuthorId
                                                                        npub[..10] + "..."
                                                                    )
                                                                )
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
            )
        ]

    let view (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.small
            DockPanel.children [
                // Right panel: followed users
                followedUsersPanelView model dispatch

                // Main content
                DockPanel.create [
                    DockPanel.children [
                        // Compose area at top
                        Border.create [
                            Border.dock Dock.Top
                            Border.child (composeAreaView model dispatch)
                        ]

                        // Feed header
                        Border.create [
                            Border.dock Dock.Top
                            Border.child (feedHeaderView model dispatch)
                        ]

                        // Feed content
                        Feed.view model.Feed (FeedMsg >> dispatch)
                    ]
                ]
            ]
        ]
