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

type Column = | Colum of string * string
type Query =
    | Projection of string * Expression
and MultiValue =
     | SimpleList of SqliteParameter list
     | SelectList of Query
and Expression =
    | EqualTo of Column * SqliteParameter
    | GreaterThan of Column * SqliteParameter
    | LessThan of Column * SqliteParameter
    | In of Column * MultiValue
    | And of Expression * Expression

let buildQueryForFilter (filter: Request.Filter) =

    let inConditions field sqltype values =
        match values with
        | [] -> None
        | _ -> 
            let fieldValues = values |> List.map sqltype
            Some (In (field, SimpleList fieldValues))
        
    let notHidden = EqualTo (Colum ("e", "deleted"), Sqlite.bool false) |> Some

    let authCondition =
        filter.Authors
        |> List.map Utils.fromHex
        |> inConditions (Colum ("e", "author")) Sqlite.bytes 

    let idCondition =
        filter.Ids
        |> List.map Utils.fromHex
        |> inConditions (Colum ("e", "event_hash")) Sqlite.bytes

    let kindCondition table =
        filter.Kinds
        |> List.map int
        |> inConditions (Colum (table, "kind")) Sqlite.int

    let sinceCondition table =
        filter.Since
        |> Option.map Utils.toUnixTime
        |> Option.map (fun since -> GreaterThan (Colum (table, "created_at"), Sqlite.int (int since)))
    
    let untilCondition table =
        filter.Until
        |> Option.map Utils.toUnixTime
        |> Option.map (fun until -> LessThan (Colum (table, "created_at"), Sqlite.int (int until)))
            
    let tagsCondition =
        let tagCondition tag values =
            values
            |> inConditions (Colum ("t", "value")) Sqlite.string
            |> Option.map (fun x -> x :: List.choose id [kindCondition "t"; sinceCondition "t"; untilCondition "t"])
            |> Option.map (fun conditions ->
                let andExpr = List.reduce (fun acc expr -> And(acc, expr)) conditions 
                let select = Projection ("SELECT t.event_id FROM tags t", And (EqualTo(Colum ("t", "name"), Sqlite.string tag), andExpr))
                In (Colum ("e", "id"), SelectList select))

        filter.Tags
        |> List.map (fun (tag, values) -> tagCondition tag[1..2] values)

    ([notHidden; authCondition; kindCondition "e"; idCondition; sinceCondition "e"; untilCondition "e"] @ tagsCondition)
    |> List.choose id
    |> List.reduce (fun acc expr -> And(acc, expr))
    |> fun es -> Projection("SELECT e.serialized_event FROM events e", es)
    
let rec materializeExpression expression scope =
    let paramName (Colum(table, name)) = $"@s{scope}_{table}_{name}"
    let columnName (Colum(table, name)) = $"{table}.{name}"
    
    match expression with
    | EqualTo (column, value) -> $"{columnName column} = {paramName column}", [paramName column, value], scope 
    | GreaterThan (column, value) -> $"{columnName column} > {paramName column}", [paramName column, value], scope
    | LessThan (column, value) -> $"{columnName column} < {paramName column}", [paramName column, value], scope
    | In (column, values) ->
        match values with
        | SimpleList values ->
            let parameterValues =
                values
                |> List.mapi (fun i v -> (paramName column) + string i, v)
            let parameterNames = String.concat "," (parameterValues |> List.map fst)
            $"{columnName column} IN ({parameterNames})", parameterValues, scope
        | SelectList query ->
            let select, parameterValues, scope = materializeQuery query (scope + 1)
            $"{columnName column} IN ({select})", parameterValues, scope
    | And (expr1, expr2) ->
        let s1, params1, scope = materializeExpression expr1 scope
        let s2, params2, scope = materializeExpression expr2 scope
        $"{s1} AND {s2}", params1 @ params2, scope      
             
and
    materializeQuery (x: Query) (scope:int) =
    match x with
    | Projection(select, where) ->
        let whereStr, expr, scope = materializeExpression where scope
        $"{select} WHERE {whereStr}", expr, scope

let buildQueryForFilters (filters: Request.Filter list) =
    filters
    |> List.map buildQueryForFilter
    |> List.fold (fun (i, qs) query -> let s, p, j = materializeQuery query i in (j + 1, (s, p) :: qs) )  (0, [])
    |> snd
    |> List.rev
    |> List.reduce (fun (select1, parms1) (select2, parms2) -> ($"{select1} UNION {select2}", parms1 @ parms2))  
    
let fetchEvents connection filters =
    let query, parameters = buildQueryForFilters filters
    connection
    |> Sqlite.query query
    |> Sqlite.parameters parameters
    |> Sqlite.executeAsync (fun read -> read.string "serialized_event")
   