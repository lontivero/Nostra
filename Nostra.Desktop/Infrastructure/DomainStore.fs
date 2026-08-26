namespace Nostra.Desktop.Store

open System
open Nostra
open Nostra.Desktop.Domain
open Nostra.Desktop.Infrastructure
open Nostra.Desktop.ViewModels

/// The domain store holds all domain data in a centralized location
/// This separates domain state from UI state

type DomainStore = {
    /// All known profiles (domain data only)
    Profiles: Map<byte[], Profile>

    /// Cached profile data (includes image bytes)
    ProfileCache: Map<byte[], CachedProfile>

    /// All known notes
    Notes: Map<byte[], Note>

    /// Cached notes (includes denormalized author name)
    NoteCache: Map<byte[], CachedNote>

    /// Contact lists (who follows whom)
    /// Key is the follower, value is list of who they follow
    Contacts: Map<byte[], AuthorId list>

    /// Relay lists per user
    RelayLists: Map<byte[], string list>

    /// The current user's author ID
    CurrentUser: AuthorId option

    /// Set of bookmarked event IDs
    Bookmarks: Set<EventId>
}

module DomainStore =
    let empty : DomainStore = {
        Profiles = Map.empty
        ProfileCache = Map.empty
        Notes = Map.empty
        NoteCache = Map.empty
        Contacts = Map.empty
        RelayLists = Map.empty
        CurrentUser = None
        Bookmarks = Set.empty
    }

    let private authorToBytes (author: AuthorId) = AuthorId.toBytes author
    let private eventToBytes (event: EventId) = EventId.toBytes event

    // Profile operations

    let getProfile (authorId: AuthorId) (store: DomainStore) : Profile option =
        store.Profiles |> Map.tryFind (authorToBytes authorId)

    let getCachedProfile (authorId: AuthorId) (store: DomainStore) : CachedProfile option =
        store.ProfileCache |> Map.tryFind (authorToBytes authorId)

    let setProfile (profile: Profile) (store: DomainStore) : DomainStore =
        let bytes = authorToBytes profile.AuthorId
        let updatedProfiles = store.Profiles |> Map.add bytes profile
        let updatedCache =
            match store.ProfileCache |> Map.tryFind bytes with
            | Some cached -> store.ProfileCache |> Map.add bytes (CachedProfile.updateProfile profile cached)
            | None -> store.ProfileCache |> Map.add bytes (CachedProfile.fromProfile profile)
        { store with Profiles = updatedProfiles; ProfileCache = updatedCache }

    let setProfilePictureData (authorId: AuthorId) (data: byte[]) (store: DomainStore) : DomainStore =
        let bytes = authorToBytes authorId
        match store.ProfileCache |> Map.tryFind bytes with
        | Some cached ->
            let updated = CachedProfile.withPictureData data cached
            { store with ProfileCache = store.ProfileCache |> Map.add bytes updated }
        | None ->
            // If no cached profile, create one with just the image
            match store.Profiles |> Map.tryFind bytes with
            | Some profile ->
                let cached = CachedProfile.fromProfile profile |> CachedProfile.withPictureData data
                { store with ProfileCache = store.ProfileCache |> Map.add bytes cached }
            | None -> store

    // Note operations

    let getNote (eventId: EventId) (store: DomainStore) : Note option =
        store.Notes |> Map.tryFind (eventToBytes eventId)

    let getCachedNote (eventId: EventId) (store: DomainStore) : CachedNote option =
        store.NoteCache |> Map.tryFind (eventToBytes eventId)

    let setNote (note: Note) (store: DomainStore) : DomainStore =
        let bytes = eventToBytes note.Id
        let authorName =
            store.Profiles
            |> Map.tryFind (authorToBytes note.Author)
            |> Option.bind Profile.displayName
        let cached = CachedNote.fromNoteWithProfile note (store.Profiles |> Map.tryFind (authorToBytes note.Author))
        { store with
            Notes = store.Notes |> Map.add bytes note
            NoteCache = store.NoteCache |> Map.add bytes cached }

    let setNotes (notes: Note list) (store: DomainStore) : DomainStore =
        notes |> List.fold (fun s n -> setNote n s) store

    // Contact operations

    let isFollowing (follower: AuthorId) (target: AuthorId) (store: DomainStore) : bool =
        store.Contacts
        |> Map.tryFind (authorToBytes follower)
        |> Option.map (List.exists (fun a -> authorToBytes a = authorToBytes target))
        |> Option.defaultValue false

    let isFollowedByCurrentUser (target: AuthorId) (store: DomainStore) : bool =
        match store.CurrentUser with
        | Some user -> isFollowing user target store
        | None -> false

    let getFollowing (authorId: AuthorId) (store: DomainStore) : AuthorId list =
        store.Contacts
        |> Map.tryFind (authorToBytes authorId)
        |> Option.defaultValue []

    let setContacts (owner: AuthorId) (following: AuthorId list) (store: DomainStore) : DomainStore =
        { store with Contacts = store.Contacts |> Map.add (authorToBytes owner) following }

    let addContact (follower: AuthorId) (target: AuthorId) (store: DomainStore) : DomainStore =
        let bytes = authorToBytes follower
        let current = store.Contacts |> Map.tryFind bytes |> Option.defaultValue []
        if current |> List.exists (fun a -> authorToBytes a = authorToBytes target) then
            store
        else
            { store with Contacts = store.Contacts |> Map.add bytes (target :: current) }

    let removeContact (follower: AuthorId) (target: AuthorId) (store: DomainStore) : DomainStore =
        let bytes = authorToBytes follower
        let current = store.Contacts |> Map.tryFind bytes |> Option.defaultValue []
        let updated = current |> List.filter (fun a -> authorToBytes a <> authorToBytes target)
        { store with Contacts = store.Contacts |> Map.add bytes updated }

    // Bookmark operations

    let isBookmarked (eventId: EventId) (store: DomainStore) : bool =
        store.Bookmarks |> Set.contains eventId

    let addBookmark (eventId: EventId) (store: DomainStore) : DomainStore =
        { store with Bookmarks = store.Bookmarks |> Set.add eventId }

    let removeBookmark (eventId: EventId) (store: DomainStore) : DomainStore =
        { store with Bookmarks = store.Bookmarks |> Set.remove eventId }

    // ViewModel builders

    let buildProfileViewModel (authorId: AuthorId) (store: DomainStore) : ProfileViewModel option =
        store.ProfileCache
        |> Map.tryFind (authorToBytes authorId)
        |> Option.map (fun cached ->
            ProfileViewModel.fromCached
                cached
                (isFollowedByCurrentUser authorId store)
                store.CurrentUser)

    let buildNoteViewModel (eventId: EventId) (store: DomainStore) : NoteViewModel option =
        store.NoteCache
        |> Map.tryFind (eventToBytes eventId)
        |> Option.map (fun cached ->
            let author = buildProfileViewModel cached.Note.Author store
            NoteViewModel.fromCached cached author store.Bookmarks store.CurrentUser)

    let buildFeedViewModel (noteIds: EventId list) (store: DomainStore) : FeedViewModel =
        let notes =
            noteIds
            |> List.choose (fun id -> buildNoteViewModel id store)
        { Notes = notes; IsLoading = false; HasMore = true; ErrorMessage = None }

/// Messages for updating the domain store
type StoreMsg =
    | ProfileReceived of Profile
    | ProfilePictureReceived of AuthorId * byte[]
    | NoteReceived of Note
    | NotesReceived of Note list
    | ContactsReceived of owner: AuthorId * following: AuthorId list
    | FollowUser of AuthorId
    | UnfollowUser of AuthorId
    | SetCurrentUser of AuthorId option
    | AddBookmark of EventId
    | RemoveBookmark of EventId

module StoreUpdate =
    let update (msg: StoreMsg) (store: DomainStore) : DomainStore =
        match msg with
        | ProfileReceived profile ->
            DomainStore.setProfile profile store

        | ProfilePictureReceived (authorId, data) ->
            DomainStore.setProfilePictureData authorId data store

        | NoteReceived note ->
            DomainStore.setNote note store

        | NotesReceived notes ->
            DomainStore.setNotes notes store

        | ContactsReceived (owner, following) ->
            DomainStore.setContacts owner following store

        | FollowUser target ->
            match store.CurrentUser with
            | Some user -> DomainStore.addContact user target store
            | None -> store

        | UnfollowUser target ->
            match store.CurrentUser with
            | Some user -> DomainStore.removeContact user target store
            | None -> store

        | SetCurrentUser user ->
            { store with CurrentUser = user }

        | AddBookmark eventId ->
            DomainStore.addBookmark eventId store

        | RemoveBookmark eventId ->
            DomainStore.removeBookmark eventId store
