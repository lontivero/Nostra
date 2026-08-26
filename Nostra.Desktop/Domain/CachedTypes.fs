namespace Nostra.Desktop.Infrastructure

open System
open Nostra
open Nostra.Desktop.Domain

/// Infrastructure types - caching and persistence concerns
/// These wrap domain types with additional data needed for storage/caching

/// A profile with cached image data
type CachedProfile = {
    Profile: Profile
    PictureData: byte[] option
    LastUpdated: DateTime
}

module CachedProfile =
    let fromProfile (profile: Profile) : CachedProfile = {
        Profile = profile
        PictureData = None
        LastUpdated = DateTime.UtcNow
    }

    let withPictureData (data: byte[]) (cached: CachedProfile) : CachedProfile =
        { cached with PictureData = Some data; LastUpdated = DateTime.UtcNow }

    let updateProfile (profile: Profile) (cached: CachedProfile) : CachedProfile =
        { cached with Profile = profile; LastUpdated = DateTime.UtcNow }

/// A note with denormalized author name for efficient display
type CachedNote = {
    Note: Note
    AuthorName: string option  // Denormalized for display performance
    LastUpdated: DateTime
}

module CachedNote =
    let fromNote (note: Note) : CachedNote = {
        Note = note
        AuthorName = None
        LastUpdated = DateTime.UtcNow
    }

    let withAuthorName (name: string option) (cached: CachedNote) : CachedNote =
        { cached with AuthorName = name }

    /// Create from note with author profile lookup
    let fromNoteWithProfile (note: Note) (profile: Profile option) : CachedNote = {
        Note = note
        AuthorName = profile |> Option.bind Profile.displayName
        LastUpdated = DateTime.UtcNow
    }

/// Image fetch status
type ImageFetchStatus =
    | NotFetched
    | Fetching
    | Fetched of byte[]
    | Failed of reason: string
