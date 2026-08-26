namespace Nostra.Desktop.ViewModels

open System
open Nostra
open Nostra.Desktop.Domain
open Nostra.Desktop.Infrastructure

/// View Model types - combine domain data with UI-specific state
/// These are what the views actually render

/// Profile as displayed in the UI
type ProfileViewModel = {
    Profile: Profile
    PictureData: byte[] option
    IsFollowed: bool
    IsCurrentUser: bool
}

module ProfileViewModel =
    let create
        (profile: Profile)
        (pictureData: byte[] option)
        (isFollowed: bool)
        (currentUser: AuthorId option)
        : ProfileViewModel =
        {
            Profile = profile
            PictureData = pictureData
            IsFollowed = isFollowed
            IsCurrentUser =
                currentUser
                |> Option.map (fun cu -> AuthorId.toBytes cu = AuthorId.toBytes profile.AuthorId)
                |> Option.defaultValue false
        }

    let fromCached
        (cached: CachedProfile)
        (isFollowed: bool)
        (currentUser: AuthorId option)
        : ProfileViewModel =
        create cached.Profile cached.PictureData isFollowed currentUser

    /// Shorthand accessors
    let authorId vm = vm.Profile.AuthorId
    let name vm = vm.Profile.Name
    let displayName vm = Profile.displayName vm.Profile
    let about vm = vm.Profile.About
    let picture vm = vm.Profile.Picture
    let nip05 vm = vm.Profile.Nip05

/// Note as displayed in the feed
type NoteViewModel = {
    Note: Note
    Author: ProfileViewModel option
    AuthorName: string option  // Fallback if no profile loaded
    IsExpanded: bool
    IsBookmarked: bool
    IsOwnNote: bool
}

module NoteViewModel =
    let create
        (note: Note)
        (author: ProfileViewModel option)
        (authorName: string option)
        (bookmarks: Set<EventId>)
        (currentUser: AuthorId option)
        : NoteViewModel =
        {
            Note = note
            Author = author
            AuthorName = authorName
            IsExpanded = false
            IsBookmarked = bookmarks |> Set.contains note.Id
            IsOwnNote =
                currentUser
                |> Option.map (fun cu -> AuthorId.toBytes cu = AuthorId.toBytes note.Author)
                |> Option.defaultValue false
        }

    let fromCached
        (cached: CachedNote)
        (author: ProfileViewModel option)
        (bookmarks: Set<EventId>)
        (currentUser: AuthorId option)
        : NoteViewModel =
        create cached.Note author cached.AuthorName bookmarks currentUser

    /// Shorthand accessors
    let id vm = vm.Note.Id
    let content vm = vm.Note.Content
    let createdAt vm = vm.Note.CreatedAt
    let authorId vm = vm.Note.Author

/// Feed as displayed - a list of notes with loading state
type FeedViewModel = {
    Notes: NoteViewModel list
    IsLoading: bool
    HasMore: bool
    ErrorMessage: string option
}

module FeedViewModel =
    let empty : FeedViewModel = {
        Notes = []
        IsLoading = false
        HasMore = true
        ErrorMessage = None
    }

    let loading : FeedViewModel = {
        Notes = []
        IsLoading = true
        HasMore = true
        ErrorMessage = None
    }

/// Search result view model
type SearchResultViewModel =
    | NotSearched
    | Searching
    | Found of ProfileViewModel
    | NotFound of reason: string

/// Dialog state for profile/note info dialogs
type DialogViewModel =
    | NoDialog
    | ShowingProfile of ProfileViewModel
    | ShowingNote of NoteViewModel
