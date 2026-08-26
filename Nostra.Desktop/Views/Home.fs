namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Nostra
open Nostra.Desktop.Infrastructure
open Nostra.Desktop.Store
open Styles

module Home =

    type Model = {
        Feed: Feed.Model
        FollowedAuthors: Set<byte[]>  // Just the set of who is followed
        ComposeText: string
    }

    type Msg =
        | FeedMsg of Feed.Msg
        | Follow of AuthorId
        | AddToFollowed of AuthorId  // Just adds to FollowedAuthors, no external message
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
        | RequestUrlPreview of string

    let init () = {
        Feed = Feed.init ()
        FollowedAuthors = Set.empty
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
                | Feed.RequestUrlPreview url -> RequestUrlPreview url
            { model with Feed = newFeed }, externalMsg

        | Follow author ->
            let bytes = NostrService.authorIdToBytes author
            let updatedFollowed = model.FollowedAuthors |> Set.add bytes
            // Also notify Feed about the follow
            let newFeed, _ = Feed.update (Feed.AuthorFollowed author) model.Feed
            { model with FollowedAuthors = updatedFollowed; Feed = newFeed }, SubscribeToAuthor author

        | AddToFollowed author ->
            let bytes = NostrService.authorIdToBytes author
            let updatedFollowed = model.FollowedAuthors |> Set.add bytes
            // Also notify Feed about the follow
            let newFeed, _ = Feed.update (Feed.AuthorFollowed author) model.Feed
            { model with FollowedAuthors = updatedFollowed; Feed = newFeed }, NoOp

        | Unfollow author ->
            let bytes = NostrService.authorIdToBytes author
            let updatedFollowed = model.FollowedAuthors |> Set.remove bytes
            // Also notify Feed about the unfollow
            let newFeed, _ = Feed.update (Feed.AuthorUnfollowed author) model.Feed
            { model with FollowedAuthors = updatedFollowed; Feed = newFeed }, NoOp

        | ProfileUpdated (authorBytes, profile) ->
            // Forward to Feed's profile cache
            let newFeed, _ = Feed.update (Feed.ProfileUpdated (authorBytes, profile)) model.Feed
            { model with Feed = newFeed }, NoOp

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

    let private followedUsersPanelView (store: DomainStore) (model: Model) dispatch =
        // Get profiles for followed authors from DomainStore, falling back to Feed's cache
        let followedProfiles =
            model.FollowedAuthors
            |> Set.toList
            |> List.choose (fun authorBytes ->
                // Try DomainStore first
                store.ProfileCache
                |> Map.tryFind authorBytes
                |> Option.map (fun cached ->
                    // Convert to UserProfile for display
                    let profile: UserProfile = {
                        AuthorId = cached.Profile.AuthorId
                        Name = cached.Profile.Name
                        DisplayName = cached.Profile.DisplayName
                        About = cached.Profile.About
                        Picture = cached.Profile.Picture
                        PictureData = cached.PictureData
                        Nip05 = cached.Profile.Nip05
                    }
                    (authorBytes, profile))
                // Fall back to Feed's legacy cache
                |> Option.orElse (
                    model.Feed.ProfileCache
                    |> Map.tryFind authorBytes
                    |> Option.map (fun profile -> (authorBytes, profile))))

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
                            TextBlock.text $"Following ({model.FollowedAuthors.Count})"
                            TextBlock.margin (Spacing.bottom 10.0)
                        ])

                        ScrollViewer.create [
                            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.spacing 5.0
                                    StackPanel.children [
                                        for _, profile in followedProfiles do
                                            Border.create (Attrs.cardSmall @ [
                                                Border.child (
                                                    DockPanel.create [
                                                        DockPanel.children [
                                                            Button.create [
                                                                Button.dock Dock.Right
                                                                Button.content "x"
                                                                Button.padding (Thickness(5.0, 0.0))
                                                                Button.onClick (fun _ -> dispatch (Unfollow profile.AuthorId))
                                                            ]
                                                            TextBlock.create (Attrs.ellipsisText @ [
                                                                TextBlock.text (
                                                                    profile.DisplayName
                                                                    |> Option.orElse profile.Name
                                                                    |> Option.defaultValue (
                                                                        let npub = NostrService.formatAuthorId profile.AuthorId
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

    let view (store: DomainStore) (model: Model) dispatch =
        DockPanel.create [
            DockPanel.margin Spacing.small
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
                Feed.view store model.Feed (FeedMsg >> dispatch)
            ]
        ]
