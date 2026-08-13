namespace Nostra.Desktop

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Nostra
open Styles

module Sidebar =

    type Model = {
        FollowedUsers: Map<byte[], UserProfile>
    }

    type Msg =
        | Follow of AuthorId
        | Unfollow of AuthorId
        | ProfileUpdated of byte[] * UserProfile

    type ExternalMsg =
        | NoOp
        | SubscribeToAuthor of AuthorId

    let init () = {
        FollowedUsers = Map.empty
    }

    let update (msg: Msg) (model: Model) : Model * ExternalMsg =
        match msg with
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

    let view (model: Model) dispatch =
        let followedUsers =
            model.FollowedUsers
            |> Map.toList
            |> List.filter (fun (_, p) -> p.IsFollowed)
            |> List.map snd

        StackPanel.create [
            StackPanel.margin Spacing.small
            StackPanel.orientation Orientation.Vertical
            StackPanel.width Dimensions.sidebarWidth
            StackPanel.children [
                TextBlock.create (Attrs.subheading @ [
                    TextBlock.fontSize FontSizes.normal
                    TextBlock.margin (Spacing.bottom 10.0)
                    TextBlock.text $"Following ({followedUsers.Length})"
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
                                                    TextBlock.create (Attrs.ellipsisText @ [
                                                        TextBlock.dock Dock.Left
                                                        TextBlock.text (
                                                            user.DisplayName
                                                            |> Option.orElse user.Name
                                                            |> Option.defaultValue (
                                                                let npub = NostrService.formatAuthorId user.AuthorId
                                                                npub[..10] + "..."
                                                            )
                                                        )
                                                    ])
                                                    Button.create [
                                                        Button.dock Dock.Right
                                                        Button.content "x"
                                                        Button.padding (Thickness(5.0, 0.0))
                                                        Button.onClick (fun _ -> dispatch (Unfollow user.AuthorId))
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
