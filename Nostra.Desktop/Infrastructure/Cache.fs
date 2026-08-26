namespace Nostra.Desktop

open System
open System.IO
open LiteDB
open Nostra

/// Storage DTOs for LiteDB serialization
/// These are plain record types that LiteDB can serialize directly
module CacheTypes =

    /// Stored profile record
    [<CLIMutable>]
    type StoredProfile = {
        Id: string              // AuthorId as hex string (primary key)
        Name: string
        DisplayName: string
        About: string
        Picture: string         // Picture URL
        PictureData: byte[]     // Cached image data
        Nip05: string
        UpdatedAt: DateTime
    }

    /// Stored contact list for an author
    [<CLIMutable>]
    type StoredContacts = {
        Id: string              // AuthorId as hex string (owner)
        Following: string[]     // Array of AuthorId hex strings
        UpdatedAt: DateTime
    }

    /// Stored event record
    [<CLIMutable>]
    type StoredEvent = {
        Id: string              // EventId as hex string (primary key)
        Author: string          // AuthorId as hex string
        AuthorName: string
        Content: string
        CreatedAt: DateTime
        Kind: int
        ReplyTo: string         // EventId as hex string or null
        Relays: string[]
        RawJson: string
    }

    /// Stored relay list for an author (NIP-65)
    [<CLIMutable>]
    type StoredRelayList = {
        Id: string              // AuthorId as hex string
        Relays: string[]        // Relay URLs
        UpdatedAt: DateTime
    }


/// Pure functional cache operations
/// All functions are pure - they take inputs and return outputs without side effects
module Cache =
    open CacheTypes

    // Conversion functions (pure)

    let private authorIdToHex (authorId: AuthorId) =
        AuthorId.toHex authorId

    let private eventIdToHex (eventId: EventId) =
        EventId.toHex eventId

    let private tryParseAuthorId (hex: string) =
        AuthorId.parse hex |> Result.toOption

    let private tryParseEventId (hex: string) =
        EventId.parse hex |> Result.toOption

    /// Convert UserProfile to StoredProfile
    let profileToStored (profile: UserProfile) : StoredProfile =
        { Id = authorIdToHex profile.AuthorId
          Name = profile.Name |> Option.defaultValue ""
          DisplayName = profile.DisplayName |> Option.defaultValue ""
          About = profile.About |> Option.defaultValue ""
          Picture = profile.Picture |> Option.defaultValue ""
          PictureData = profile.PictureData |> Option.defaultValue [||]
          Nip05 = profile.Nip05 |> Option.defaultValue ""
          UpdatedAt = DateTime.UtcNow }

    /// Convert StoredProfile to UserProfile
    let storedToProfile (stored: StoredProfile) : UserProfile option =
        tryParseAuthorId stored.Id
        |> Option.map (fun authorId ->
            let pictureData =
                if stored.PictureData = null || stored.PictureData.Length = 0 then None
                else Some stored.PictureData
            printfn "[TRACE-CACHE] storedToProfile %s: PictureData=%s"
                (stored.Id.[..7])
                (if pictureData.IsSome then $"{pictureData.Value.Length} bytes" else "None")
            { AuthorId = authorId
              Name = if String.IsNullOrEmpty stored.Name then None else Some stored.Name
              DisplayName = if String.IsNullOrEmpty stored.DisplayName then None else Some stored.DisplayName
              About = if String.IsNullOrEmpty stored.About then None else Some stored.About
              Picture = if String.IsNullOrEmpty stored.Picture then None else Some stored.Picture
              PictureData = pictureData
              Nip05 = if String.IsNullOrEmpty stored.Nip05 then None else Some stored.Nip05 })

    /// Convert FeedEvent to StoredEvent
    let eventToStored (event: FeedEvent) : StoredEvent =
        { Id = eventIdToHex event.Id
          Author = authorIdToHex event.Author
          AuthorName = event.AuthorName |> Option.defaultValue ""
          Content = event.Content
          CreatedAt = event.CreatedAt
          Kind = int event.Kind
          ReplyTo = event.ReplyTo |> Option.map eventIdToHex |> Option.defaultValue ""
          Relays = event.Relays |> Array.ofList
          RawJson = event.RawJson }

    /// Convert StoredEvent to FeedEvent
    let storedToEvent (stored: StoredEvent) : FeedEvent option =
        match tryParseEventId stored.Id, tryParseAuthorId stored.Author with
        | Some eventId, Some authorId ->
            Some { Id = eventId
                   Author = authorId
                   AuthorName = if String.IsNullOrEmpty stored.AuthorName then None else Some stored.AuthorName
                   Content = stored.Content
                   CreatedAt = stored.CreatedAt
                   Kind = enum<Kind> stored.Kind
                   ReplyTo = if String.IsNullOrEmpty stored.ReplyTo then None else tryParseEventId stored.ReplyTo
                   Relays = stored.Relays |> List.ofArray
                   RawJson = stored.RawJson }
        | _ -> None

    /// Convert contacts to stored format
    let contactsToStored (ownerId: AuthorId) (following: AuthorId list) : StoredContacts =
        { Id = authorIdToHex ownerId
          Following = following |> List.map authorIdToHex |> Array.ofList
          UpdatedAt = DateTime.UtcNow }

    /// Convert stored contacts to AuthorId list
    let storedToContacts (stored: StoredContacts) : AuthorId list =
        stored.Following
        |> Array.choose tryParseAuthorId
        |> List.ofArray

    /// Convert relay list to stored format
    let relayListToStored (authorId: AuthorId) (relays: string list) : StoredRelayList =
        { Id = authorIdToHex authorId
          Relays = relays |> Array.ofList
          UpdatedAt = DateTime.UtcNow }

    /// Convert stored relay list to string list
    let storedToRelayList (stored: StoredRelayList) : string list =
        stored.Relays |> List.ofArray


/// Database handle - wraps LiteDatabase for resource management
type CacheDb = private CacheDb of LiteDatabase

/// Cache module with database operations
/// Uses a reader pattern - functions take db as first parameter
module CacheDb =
    open CacheTypes
    open Cache

    let private defaultDbPath () =
        let appData = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
        let nostraDir = Path.Combine(appData, "Nostra")
        Directory.CreateDirectory(nostraDir) |> ignore
        Path.Combine(nostraDir, "cache.db")

    /// Open the cache database
    let open' (path: string option) : CacheDb =
        let dbPath = path |> Option.defaultWith defaultDbPath
        let db = new LiteDatabase($"Filename={dbPath};Connection=shared")
        CacheDb db

    /// Close the database
    let close (CacheDb db) =
        db.Dispose()

    /// Use database with automatic cleanup
    let using (path: string option) (f: CacheDb -> 'a) : 'a =
        let db = open' path
        try
            f db
        finally
            close db

    // Profile operations

    let private profiles (CacheDb db) =
        db.GetCollection<StoredProfile>("profiles")

    /// Get a profile by author ID
    let getProfile (authorId: AuthorId) (db: CacheDb) : UserProfile option =
        let col = profiles db
        let id = AuthorId.toHex authorId
        col.FindById(BsonValue id)
        |> Option.ofObj
        |> Option.bind storedToProfile

    /// Get multiple profiles by author IDs
    let getProfiles (authorIds: AuthorId list) (db: CacheDb) : UserProfile list =
        authorIds |> List.choose (fun id -> getProfile id db)

    /// Save or update a profile
    let saveProfile (profile: UserProfile) (db: CacheDb) : unit =
        let col = profiles db
        let stored = profileToStored profile
        col.Upsert(stored) |> ignore

    /// Save multiple profiles
    let saveProfiles (profileList: UserProfile list) (db: CacheDb) : unit =
        let col = profiles db
        profileList
        |> List.map profileToStored
        |> Seq.ofList
        |> col.Upsert
        |> ignore

    /// Get all cached profiles
    let getAllProfiles (db: CacheDb) : UserProfile list =
        let col = profiles db
        col.FindAll()
        |> Seq.choose storedToProfile
        |> List.ofSeq

    /// Delete a profile
    let deleteProfile (authorId: AuthorId) (db: CacheDb) : bool =
        let col = profiles db
        let id = AuthorId.toHex authorId
        col.Delete(BsonValue id)

    // Event operations

    let private events (CacheDb db) =
        db.GetCollection<StoredEvent>("events")

    /// Get an event by ID
    let getEvent (eventId: EventId) (db: CacheDb) : FeedEvent option =
        let col = events db
        let id = EventId.toHex eventId
        col.FindById(BsonValue id)
        |> Option.ofObj
        |> Option.bind storedToEvent

    /// Get events by author
    let getEventsByAuthor (authorId: AuthorId) (db: CacheDb) : FeedEvent list =
        let col = events db
        let authorHex = AuthorId.toHex authorId
        col.Find(fun e -> e.Author = authorHex)
        |> Seq.choose storedToEvent
        |> List.ofSeq

    /// Get events by kind
    let getEventsByKind (kind: Kind) (db: CacheDb) : FeedEvent list =
        let col = events db
        let kindInt = int kind
        col.Find(fun e -> e.Kind = kindInt)
        |> Seq.choose storedToEvent
        |> List.ofSeq

    /// Get recent events (ordered by created date descending)
    let getRecentEvents (limit: int) (db: CacheDb) : FeedEvent list =
        let col = events db
        col.Query()
           .OrderByDescending(fun e -> e.CreatedAt)
           .Limit(limit)
           .ToEnumerable()
        |> Seq.choose storedToEvent
        |> List.ofSeq

    /// Save or update an event
    let saveEvent (event: FeedEvent) (db: CacheDb) : unit =
        let col = events db
        let stored = eventToStored event
        col.Upsert(stored) |> ignore

    /// Save multiple events
    let saveEvents (eventList: FeedEvent list) (db: CacheDb) : unit =
        let col = events db
        eventList
        |> List.map eventToStored
        |> Seq.ofList
        |> col.Upsert
        |> ignore

    /// Delete an event
    let deleteEvent (eventId: EventId) (db: CacheDb) : bool =
        let col = events db
        let id = EventId.toHex eventId
        col.Delete(BsonValue id)

    /// Delete events older than a date
    let deleteEventsOlderThan (date: DateTime) (db: CacheDb) : int =
        let col = events db
        col.DeleteMany(fun e -> e.CreatedAt < date)

    // Contacts operations

    let private contacts (CacheDb db) =
        db.GetCollection<StoredContacts>("contacts")

    /// Get contacts (following list) for an author
    let getContacts (authorId: AuthorId) (db: CacheDb) : AuthorId list =
        let col = contacts db
        let id = AuthorId.toHex authorId
        col.FindById(BsonValue id)
        |> Option.ofObj
        |> Option.map storedToContacts
        |> Option.defaultValue []

    /// Save contacts for an author
    let saveContacts (authorId: AuthorId) (following: AuthorId list) (db: CacheDb) : unit =
        let col = contacts db
        let stored = contactsToStored authorId following
        col.Upsert(stored) |> ignore

    /// Delete contacts for an author
    let deleteContacts (authorId: AuthorId) (db: CacheDb) : bool =
        let col = contacts db
        let id = AuthorId.toHex authorId
        col.Delete(BsonValue id)

    // Relay list operations

    let private relayLists (CacheDb db) =
        db.GetCollection<StoredRelayList>("relay_lists")

    /// Get relay list for an author (NIP-65)
    let getRelayList (authorId: AuthorId) (db: CacheDb) : string list =
        let col = relayLists db
        let id = AuthorId.toHex authorId
        col.FindById(BsonValue id)
        |> Option.ofObj
        |> Option.map storedToRelayList
        |> Option.defaultValue []

    /// Save relay list for an author
    let saveRelayList (authorId: AuthorId) (relays: string list) (db: CacheDb) : unit =
        let col = relayLists db
        let stored = relayListToStored authorId relays
        col.Upsert(stored) |> ignore

    /// Delete relay list for an author
    let deleteRelayList (authorId: AuthorId) (db: CacheDb) : bool =
        let col = relayLists db
        let id = AuthorId.toHex authorId
        col.Delete(BsonValue id)

    // Utility operations

    /// Clear all cached data
    let clearAll (db: CacheDb) : unit =
        let (CacheDb liteDb) = db
        liteDb.DropCollection("profiles") |> ignore
        liteDb.DropCollection("events") |> ignore
        liteDb.DropCollection("contacts") |> ignore
        liteDb.DropCollection("relay_lists") |> ignore

    /// Get cache statistics
    let getStats (db: CacheDb) : {| Profiles: int; Events: int; Contacts: int; RelayLists: int |} =
        {| Profiles = (profiles db).Count()
           Events = (events db).Count()
           Contacts = (contacts db).Count()
           RelayLists = (relayLists db).Count() |}


/// Functional cache API using computation expressions
/// Provides a monadic interface for cache operations
[<RequireQualifiedAccess>]
module CacheOp =

    /// Cache operation that reads from the database
    type CacheReader<'a> = CacheDb -> 'a

    /// Run a cache operation
    let run (db: CacheDb) (op: CacheReader<'a>) : 'a = op db

    /// Map over a cache operation
    let map (f: 'a -> 'b) (op: CacheReader<'a>) : CacheReader<'b> =
        fun db -> f (op db)

    /// Bind cache operations
    let bind (f: 'a -> CacheReader<'b>) (op: CacheReader<'a>) : CacheReader<'b> =
        fun db -> f (op db) db

    /// Return a value as a cache operation
    let return' (x: 'a) : CacheReader<'a> = fun _ -> x

    /// Combine multiple cache reads
    let map2 (f: 'a -> 'b -> 'c) (op1: CacheReader<'a>) (op2: CacheReader<'b>) : CacheReader<'c> =
        fun db -> f (op1 db) (op2 db)

    /// Sequence cache operations
    let sequence (ops: CacheReader<'a> list) : CacheReader<'a list> =
        fun db -> ops |> List.map (fun op -> op db)

    /// Traverse with cache operations
    let traverse (f: 'a -> CacheReader<'b>) (xs: 'a list) : CacheReader<'b list> =
        xs |> List.map f |> sequence

    // Lift CacheDb functions into CacheReader

    let getProfile authorId : CacheReader<UserProfile option> =
        CacheDb.getProfile authorId

    let getProfiles authorIds : CacheReader<UserProfile list> =
        CacheDb.getProfiles authorIds

    let saveProfile profile : CacheReader<unit> =
        CacheDb.saveProfile profile

    let getEvent eventId : CacheReader<FeedEvent option> =
        CacheDb.getEvent eventId

    let getEventsByAuthor authorId : CacheReader<FeedEvent list> =
        CacheDb.getEventsByAuthor authorId

    let getRecentEvents limit : CacheReader<FeedEvent list> =
        CacheDb.getRecentEvents limit

    let saveEvent event : CacheReader<unit> =
        CacheDb.saveEvent event

    let getContacts authorId : CacheReader<AuthorId list> =
        CacheDb.getContacts authorId

    let saveContacts authorId following : CacheReader<unit> =
        CacheDb.saveContacts authorId following

    let getRelayList authorId : CacheReader<string list> =
        CacheDb.getRelayList authorId

    let saveRelayList authorId relays : CacheReader<unit> =
        CacheDb.saveRelayList authorId relays


/// Computation expression builder for cache operations
type CacheBuilder() =
    member _.Return(x) = CacheOp.return' x
    member _.ReturnFrom(op) = op
    member _.Bind(op, f) = CacheOp.bind f op
    member _.Zero() = CacheOp.return' ()
    member _.Combine(op1, op2) = CacheOp.bind (fun () -> op2) op1
    member _.Delay(f) = fun db -> f () db
    member _.Run(op) = op


[<AutoOpen>]
module CacheBuilderModule =
    /// Cache computation expression
    let cache = CacheBuilder()
