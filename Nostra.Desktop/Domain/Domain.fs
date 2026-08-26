namespace Nostra.Desktop.Domain

open System
open Nostra

/// Pure domain types - no UI concerns, no caching concerns
/// These represent the core business entities from the Nostr protocol

/// A user profile from the Nostr network
type Profile = {
    AuthorId: AuthorId
    Name: string option
    DisplayName: string option
    About: string option
    Picture: string option  // URL only, no cached bytes
    Nip05: string option
}

module Profile =
    let empty authorId : Profile = {
        AuthorId = authorId
        Name = None
        DisplayName = None
        About = None
        Picture = None
        Nip05 = None
    }

    /// Get the best display name for a profile
    let displayName (profile: Profile) =
        profile.DisplayName
        |> Option.orElse profile.Name

    /// Create from Nostr.Profile (protocol type)
    let fromNostrProfile (authorId: AuthorId) (p: Nostra.Profile) : Profile = {
        AuthorId = authorId
        Name = Some p.Name
        DisplayName = Nostra.Profile.displayName p
        About = Some p.About
        Picture = Some p.Picture
        Nip05 = Nostra.Profile.nip05 p
    }

/// A note (kind 1 event) from the Nostr network
type Note = {
    Id: EventId
    Author: AuthorId
    Content: string
    CreatedAt: DateTime
    ReplyTo: EventId option
    Relays: string list
    RawJson: string
}

module Note =
    /// Create a Note directly from components
    let create
        (id: EventId)
        (author: AuthorId)
        (content: string)
        (createdAt: DateTime)
        (replyTo: EventId option)
        (relays: string list)
        (rawJson: string)
        : Note =
        {
            Id = id
            Author = author
            Content = content
            CreatedAt = createdAt
            ReplyTo = replyTo
            Relays = relays
            RawJson = rawJson
        }

/// Represents a follow relationship
type Contact = {
    Follower: AuthorId
    Following: AuthorId
}

/// A user's contact list (who they follow)
type ContactList = {
    Owner: AuthorId
    Following: AuthorId list
}

/// A user's relay list
type RelayList = {
    Owner: AuthorId
    Relays: string list
}
