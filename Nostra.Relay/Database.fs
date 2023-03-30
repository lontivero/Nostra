module Database

open System
open Fumble
open Microsoft.Data.Sqlite
open Nostra
open Nostra.Relay

let connection connectionString =
    Sqlite.existingConnection (new SqliteConnection(connectionString))

let createTables connection =
       
    connection
    |> Sqlite.command
        """
        PRAGMA foreign_keys = ON;
        CREATE TABLE IF NOT EXISTS Events (
            eventId TEXT NOT NULL PRIMARY KEY,
            pubkey TEXT NOT NULL,
            content TEXT NOT NULL,
            kind INTEGER NOT NULL,
            createdAt INTEGER NOT NULL,
            rawEvent TEXT NOT NULL,
            deleted BOOLEAN NOT NULL
            );

        CREATE TABLE IF NOT EXISTS Tags (
            eventId TEXT NOT NULL, 
            tag TEXT NOT NULL,
            value TEXT NOT NULL,
            FOREIGN KEY (eventId)
                REFERENCES Events (eventId) 
            );            
        """
    |> Sqlite.executeCommand
    |> (fun result ->
        match result with
        | Ok rows -> printfn "Created tables"
        | Error exn -> failwith exn.Message)

let saveEvent connection (event: Event.Event) =
    let (EventId eventId) = event.Id 
    let (XOnlyPubKey xOnlyPubkey) = event.PubKey
    let pubkey = xOnlyPubkey.ToBytes()

    let tags =
        event.Tags
        |> List.collect (fun (k, vs) -> vs |> List.map (fun v -> k, v))
        
    connection
    |> Sqlite.executeTransaction [
        "INSERT INTO Events(eventId, pubkey, content, kind, createdAt, rawEvent, deleted)
            VALUES (@eventId, @pubkey, @content, @kind, @createdAt, @rawEvent, @deleted)",
        [ [ "@eventId", Sqlite.bytes eventId
            "@pubkey", Sqlite.bytes pubkey
            "@content", Sqlite.string event.Content
            "@kind", Sqlite.int (int event.Kind)
            "@createdAt", Sqlite.dateTime event.CreatedAt
            "@rawEvent", Sqlite.string ""
            "@deleted", Sqlite.bool false ] ]
        
        if tags.Length > 0 then
            "INSERT INTO Tags(eventId, tag, value)
                VALUES (@eventId, @tag, @value)",
            tags |> List.map (fun (key, value) -> [
                "@eventId", Sqlite.bytes eventId
                "@tag", Sqlite.string key
                "@value", Sqlite.string value]
        )
    ]
    |> Result.map (fun _ -> {
        Event = event
        Id = Utils.toHex eventId
        PubKey = Utils.toHex pubkey
        Serialized = "rawEvent"
        Seen = DateTime.UtcNow
        RefEvents =
            tags
            |> List.filter (fun (k,v) -> k = "e")
            |> List.map snd
        RefPubKeys =
            tags
            |> List.filter (fun (k,v) -> k = "p")
            |> List.map snd
    })
    
let deleteEvents connection (XOnlyPubKey author) eventIds =
    let eventIdsParameters = String.Join(",", eventIds |> List.mapi (fun i _ -> $"@eventId{i}"))
    connection
    |> Sqlite.executeTransaction [
        $"UPDATE Events SET deleted = TRUE WHERE kind != 5 AND pubkey = @author AND eventId IN ({eventIdsParameters})",
        [[ "@author", Sqlite.bytes (author.ToBytes()) ]
         @
         (eventIds |> List.mapi (fun i eventId -> $"@eventId{i}", Sqlite.string eventId))
        ]
    ]
    
let buildQuery (filter: Request.Filter) =
    let orConditions f lst =
        lst
        |> List.map f
        |> String.concat " OR "
        |> function
            | "" -> None
            | condition -> Some condition
        
    let authCondition =
        filter.Authors
        |> orConditions (fun auth -> $"pubkey = '{auth}'")

    let kindCondition =
        filter.Kinds
        |> orConditions (fun kind -> $"kind = {int kind}")

    let idCondition =
        filter.Ids
        |> orConditions (fun eventId -> $"eventId = {eventId}")
        
    let sinceCondition =
        filter.Since
        |> Option.map (fun since -> $"createdAt > {Utils.toUnixTime since}")
    
    let untilCondition =
        filter.Until
        |> Option.map (fun until -> $"createdAt < {Utils.toUnixTime until}")
    
    let eventTagCondition =
        filter.Events
        |> orConditions (fun e -> $"@value = {e}")
        |> Option.map (fun cond -> $"SELECT eventId WHERE @tag = 'e' AND ({cond})")

    let pubkeyTagCondition =
        filter.PubKeys
        |> orConditions (fun e -> $"@value = {e}")
        |> Option.map (fun cond -> $"SELECT eventId WHERE @tag = 'p' AND ({cond})")

    let allConditions =
        [authCondition; kindCondition; idCondition; eventTagCondition; pubkeyTagCondition; sinceCondition; untilCondition ]
        |> List.choose id
        |> String.concat " AND "
        
    $"SELECT * FROM Events WHERE {allConditions}"
    
let fetchEvents connection filter =
    let query = buildQuery filter
    connection
    |> Sqlite.query query
    |> Sqlite.executeAsync (fun read -> read.string "rawEvent")
   