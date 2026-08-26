namespace Nostra.Desktop

open System
open Nostra

// Account represents a user identity
type Account = {
    Name: string
    PublicKey: AuthorId
    SecretKey: SecretKey option
    Picture: string option
    Relays: string list
    Following: AuthorId list
}

// Navigation
type AppPage =
    | HomePage
    | NotificationsPage
    | BookmarksPage
    | SearchPage
    | SettingsPage

// Pure domain type - profile data from Nostr (no UI state like IsFollowed)
type UserProfile = {
    AuthorId: AuthorId
    Name: string option
    DisplayName: string option
    About: string option
    Picture: string option
    PictureData: byte[] option  // Cached image data
    Nip05: string option
}

module UserProfile =
    let empty authorId = {
        AuthorId = authorId
        Name = None
        DisplayName = None
        About = None
        Picture = None
        PictureData = None
        Nip05 = None
    }

// Feed events
type FeedEvent = {
    Id: EventId
    Author: AuthorId
    AuthorName: string option
    Content: string
    CreatedAt: DateTime
    Kind: Kind
    ReplyTo: EventId option
    Relays: string list
    RawJson: string
}

// Search
type SearchResult =
    | NotSearched
    | Searching
    | Found of UserProfile * IsFollowed: bool
    | NotFound of string

// Connection status (for NostrService compatibility)
type ConnectionStatus =
    | Disconnected
    | Connecting
    | Connected

// Relay connection
type RelayStatus =
    | RelayDisconnected
    | RelayConnecting
    | RelayConnected
    | RelayError of string

type RelayError = {
    Timestamp: DateTime
    Message: string
}

type RelayConfig = {
    Url: string
    Status: RelayStatus
    Enabled: bool
    Errors: RelayError list
}

// Stats for status panel
type ConnectionStats = {
    EventsReceived: int
    EventsStored: int
    RelaysConnected: int
    ActiveSubscriptions: int
}