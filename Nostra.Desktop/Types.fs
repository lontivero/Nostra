namespace Nostra.Desktop

open System
open Nostra

type UserProfile = {
    AuthorId: AuthorId
    Name: string option
    DisplayName: string option
    About: string option
    Picture: string option
    Nip05: string option
    IsFollowed: bool
}

type FeedEvent = {
    Id: EventId
    Author: AuthorId
    AuthorName: string option
    Content: string
    CreatedAt: DateTime
    Kind: Kind
    ReplyTo: EventId option
}

type SearchResult =
    | NotSearched
    | Searching
    | Found of UserProfile
    | NotFound of string

type ConnectionStatus =
    | Disconnected
    | Connecting
    | Connected

type Model = {
    SearchQuery: string
    SearchResult: SearchResult
    FollowedUsers: Map<byte[], UserProfile>
    ProfileCache: Map<byte[], UserProfile>
    EventCache: Map<byte[], FeedEvent>
    PendingEventRequests: Set<byte[]>
    Feed: FeedEvent list
    RelayUrl: string
    ConnectionStatus: ConnectionStatus
    StatusMessage: string
}

type Msg =
    | UpdateSearchQuery of string
    | SearchUser
    | SearchCompleted of Result<UserProfile, string>
    | FollowUser of AuthorId
    | UnfollowUser of AuthorId
    | EventReceived of FeedEvent
    | ProfileReceived of AuthorId * Profile
    | RequestProfile of AuthorId
    | RequestEvents of EventId list
    | UpdateRelayUrl of string
    | Connect
    | ConnectionStatusChanged of ConnectionStatus
    | SetStatusMessage of string
    | ClearFeed

module Model =
    let init () = {
        SearchQuery = ""
        SearchResult = NotSearched
        FollowedUsers = Map.empty
        ProfileCache = Map.empty
        EventCache = Map.empty
        PendingEventRequests = Set.empty
        Feed = []
        RelayUrl = "wss://relay.damus.io"
        ConnectionStatus = Disconnected
        StatusMessage = "Enter an npub or hex pubkey to search"
    }