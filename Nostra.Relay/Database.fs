module Database

open System
open FsToolkit.ErrorHandling
open Fumble
open Microsoft.Data.SqlClient
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
            event_id INTEGER NOT NULL,
            event_hash BLOB NOT NULL, 
            name TEXT NOT NULL,
            value TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            kind INTEGER NOT NULL,
            FOREIGN KEY(event_id)
                REFERENCES event(id) ON UPDATE CASCADE ON DELETE CASCADE
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
    let combineConditions (conditions : (string * (string * SqliteParameter) list) list)  =
        match conditions with
        | [] -> None
        | cs -> 
            let expressions =
                cs
                |> List.map fst
                |> String.concat " AND "
                
            let values =
                cs
                |> List.collect snd
            
            Some (expressions, values)
    
    let inConditions field sqltype values =
        match values with
        | [] -> None
        | _ -> 
            let pn = values |> List.mapi (fun i v -> (String.replace "." "_" field) + string i, v)
            let fieldNames = pn |> List.map (fun (x,_) -> "@"+x) |> String.concat ","
            let fieldValues = pn |> List.map (fun (name, value) -> name, sqltype value )
            Some ($"{field} IN ({fieldNames})", fieldValues)
        
    let notHidden = Some ("e.deleted = @e_deleted", ["@e_deleted", Sqlite.bool false])

    let authCondition =
        filter.Authors
        |> List.map Utils.fromHex
        |> inConditions "e.author" Sqlite.bytes 

    let idCondition =
        filter.Ids
        |> List.map Utils.fromHex
        |> inConditions "e.event_hash" Sqlite.bytes

    let kindCondition table =
        filter.Kinds
        |> List.map int
        |> inConditions $"{table}.kind" Sqlite.int

    let sinceCondition table =
        filter.Since
        |> Option.map Utils.toUnixTime
        |> Option.map (fun since -> $"{table}.created_at > @{table}_created_at", [$"@{table}_created_at", Sqlite.int (int since)])
    
    let untilCondition table =
        filter.Until
        |> Option.map Utils.toUnixTime
        |> Option.map (fun until -> $"{table}.created_at < @{table}_created_at", [$"@{table}_created_at", Sqlite.int (int until)])
            
    let tagsCondition =
        let tagCondition tag values =
            values
            |> inConditions "t.value" Sqlite.string
            |> Option.map (fun x -> x :: List.choose id [kindCondition "t"; sinceCondition "t"; untilCondition "t"])
            |> Option.bind combineConditions
            |> Option.map (fun (s, v) -> $"e.id IN (SELECT t.event_id FROM tags t WHERE t.name = '{tag}' AND {s})", v)

        filter.Tags
        |> List.map (fun (tag, values) -> tagCondition tag[1..2] values)
        |> List.choose id
        |> combineConditions
  
    [notHidden; authCondition; kindCondition "e"; idCondition; tagsCondition; sinceCondition "e"; untilCondition "e"]
    |> List.choose id 
    |> combineConditions
    |> Option.map (fun (s,v) -> $"SELECT e.serialized_event FROM events e WHERE {s}", v)
    |> Option.defaultValue ("SELECT e.serialized_event FROM events e WHERE e.deleted = false", [])
    
        
let fetchEvents connection filter =
    let query, parameters = buildQuery filter
    connection
    |> Sqlite.query query
    |> Sqlite.parameters parameters
    |> Sqlite.executeAsync (fun read -> read.string "serialized_event")
   