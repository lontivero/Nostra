module Database

open System
open FsToolkit.ErrorHandling
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
        
        CREATE TABLE IF NOT EXISTS events (
            id INTEGER PRIMARY KEY,
            event_hash BLOB NOT NULL,
            author BLOB NOT NULL,
            kind INTEGER NOT NULL,
            created_at INTEGER NOT NULL,
            serialized_event TEXT NOT NULL,
            deleted BOOLEAN NOT NULL
            );

        CREATE UNIQUE INDEX IF NOT EXISTS event_hash_index ON events(event_hash);
        CREATE INDEX IF NOT EXISTS author_index ON events(author);
        CREATE INDEX IF NOT EXISTS kind_index ON events(kind);
        CREATE INDEX IF NOT EXISTS created_at_index ON events(created_at);
        CREATE INDEX IF NOT EXISTS event_composite_index ON events(kind,created_at);
        CREATE INDEX IF NOT EXISTS kind_author_index ON events(kind,author);
        CREATE INDEX IF NOT EXISTS kind_created_at_index ON events(kind,created_at);
        CREATE INDEX IF NOT EXISTS author_created_at_index ON events(author,created_at);
        CREATE INDEX IF NOT EXISTS author_kind_index ON events(author,kind);           

        CREATE TABLE IF NOT EXISTS tags (
            id INTEGER PRIMARY KEY,
            event_hash BLOB NOT NULL, 
            name TEXT NOT NULL,
            value TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            kind INTEGER NOT NULL,
            FOREIGN KEY (event_hash)
                REFERENCES events(id) ON UPDATE CASCADE ON DELETE CASCADE
            );

        CREATE INDEX IF NOT EXISTS tag_value_index ON tags(value);
        CREATE INDEX IF NOT EXISTS tag_composite_index ON tags(event_hash,name,value);
        CREATE INDEX IF NOT EXISTS tag_name_eid_index ON tags(name,event_hash,value);
        CREATE INDEX IF NOT EXISTS tag_covering_index ON tags(name,kind,value,created_at,event_hash);        
        """
    |> Sqlite.executeCommand
    |> (fun result ->
        match result with
        | Ok rows -> ()
        | Error exn -> failwith exn.Message)

let areNewerReplacements connection author kind createdAt =
    connection
    |> Sqlite.query "
        SELECT e.id
        FROM events e INDEXED BY author_index
        WHERE e.author=@author AND e.kind=@kind AND e.created_at >= @created_at LIMIT 1;"
    |> Sqlite.parameters [
        "@author", Sqlite.bytes author
        "@kind", Sqlite.int kind
        "@created_at", Sqlite.dateTime createdAt ]
    |> Sqlite.executeNonQueryAsync
    |> AsyncResult.map (fun x -> x > 0)
    
let saveEvent connection (preprocessedEvent: StoredEvent) = asyncResult {
    let event = preprocessedEvent.Event
    let (EventId eventId) = event.Id 
    let (XOnlyPubKey xOnlyPubkey) = event.PubKey
    let author = xOnlyPubkey.ToBytes()
    let kind = int event.Kind

        
    let! alreadySaved = areNewerReplacements connection author kind event.CreatedAt 

    if not alreadySaved then
        let tags =
            event.Tags
            |> List.collect (fun (k, vs) -> vs |> List.map (fun v -> k, v))

        let! _ =    
            connection
            |> Sqlite.executeTransactionAsync [
                "INSERT OR IGNORE INTO events(event_hash, author, kind, created_at, serialized_event, deleted)
                    VALUES (@event_hash, @author, @kind, @created_at, @serialized_event, @deleted)",
                [ [ "@event_hash", Sqlite.bytes eventId
                    "@author", Sqlite.bytes author
                    "@kind", Sqlite.int (int event.Kind)
                    "@created_at", Sqlite.dateTime event.CreatedAt
                    "@serialized_event", Sqlite.string preprocessedEvent.Serialized
                    "@deleted", Sqlite.bool false ] ]
                
                if tags.Length > 0 then
                    "INSERT OR IGNORE INTO tags(event_hash, name, value, created_at, kind)
                        VALUES (@event_hash, @name, @value, @created_at, @kind)",
                    tags |> List.map (fun (key, value) -> [
                        "@event_hash", Sqlite.bytes eventId
                        "@name", Sqlite.string key
                        "@value", Sqlite.string value
                        "@created_at", Sqlite.dateTime event.CreatedAt
                        "@kind", Sqlite.int (int event.Kind)]
                )
            ]
        return ()
    return ()
}

let deleteEvents connection (XOnlyPubKey author) eventIds =
    let eventIdsParameters = String.Join(",", eventIds |> List.mapi (fun i _ -> $"@event_hash{i}"))
    connection
    |> Sqlite.executeTransactionAsync [
        $"UPDATE events SET deleted = TRUE WHERE kind != 5 AND author = @author AND event_hash IN ({eventIdsParameters})",
        [[ "@author", Sqlite.bytes (author.ToBytes()) ]
         @
         (eventIds |> List.mapi (fun i eventId -> $"@event_hash{i}", Sqlite.bytes (Utils.fromHex eventId)))
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
        |> orConditions (fun auth -> $"author = '{auth}'")

    let kindCondition =
        filter.Kinds
        |> orConditions (fun kind -> $"kind = {int kind}")

    let idCondition =
        filter.Ids
        |> orConditions (fun eventId -> $"event_hash = {eventId}")
        
    let sinceCondition =
        filter.Since
        |> Option.map (fun since -> $"created_at > {Utils.toUnixTime since}")
    
    let untilCondition =
        filter.Until
        |> Option.map (fun until -> $"created_at < {Utils.toUnixTime until}")
    
    let eventTagCondition =
        filter.Events
        |> orConditions (fun e -> $"@value = {e}")
        |> Option.map (fun cond -> $"SELECT event_hash WHERE @name = 'e' AND ({cond})")

    let pubkeyTagCondition =
        filter.PubKeys
        |> orConditions (fun e -> $"@value = {e}")
        |> Option.map (fun cond -> $"SELECT event_hash WHERE @name = 'p' AND ({cond})")

    let nothidden = Some "deleted = false"
    
    let allConditions =
        [nothidden; authCondition; kindCondition; idCondition; eventTagCondition; pubkeyTagCondition; sinceCondition; untilCondition ]
        |> List.choose id
        |> String.concat " AND "
        
    $"SELECT * FROM events WHERE {allConditions}"
    
let fetchEvents connection filter =
    let query = buildQuery filter
    connection
    |> Sqlite.query query
    |> Sqlite.executeAsync (fun read -> read.string "serialized_event")
   