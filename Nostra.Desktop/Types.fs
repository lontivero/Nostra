namespace Nostra.Desktop

open System
open Nostra

// Navigation
type AppPage =
    | HomePage
    | NotificationsPage
    | BookmarksPage
    | SearchPage
    | SettingsPage

// User profiles
type UserProfile = {
    AuthorId: AuthorId
    Name: string option
    DisplayName: string option
    About: string option
    Picture: string option
    Nip05: string option
    IsFollowed: bool
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
    | Found of UserProfile
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