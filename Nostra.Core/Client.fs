module Nostra.Core.Client

    open System
    open System.IO
    open System.Text
    open System.Threading
    open Microsoft.FSharp.Control
    open System.Buffers
    open System.Net.WebSockets
    open Chiron

    module Query =
        type Filter = {
            Kinds : Kind list
            Authors: XOnlyPubKey list
            Limit: int
            Since: DateTime
            Until: DateTime
            Events: EventId list
            PubKeys: XOnlyPubKey list
        } with
            static member ToJson(filter : Filter) = json {
                let kindsToJson (kinds : Kind list) =
                    Array <| (kinds |> List.map (decimal >> Number))
                do! Json.writeWith kindsToJson "kinds" filter.Kinds
                do! Json.write "authors" filter.Authors
                do! Json.write "limit" filter.Limit
                do! Json.writeWith dateTimeToJson "since" filter.Since
                do! Json.writeWith dateTimeToJson "until" filter.Until
                do! Json.write "#e" filter.Events
                do! Json.write "#p" filter.PubKeys
            }
            static member FromJson(_: Filter) = json {
                let jsonToKing = function
                    | Number n -> enum<Kind>(int n)
                    | _ -> failwith "error"
                    
                let jsonToKinds (json: Json) =  
                    match json with
                    | Array kinds -> Value (kinds |> List.map jsonToKing)
                    | _ -> Error ("An array of kinds was expected")

                let! kinds = Json.readWith jsonToKinds "kinds"
                let! authors = Json.read "authors"
                let! limit = Json.read "limit"
                let! since = Json.readWith jsonToDateTime "since"
                let! until = Json.readWith jsonToDateTime "until"
                let! events = Json.read "#e"
                let! pubkeys = Json.read "#p"
                return {
                    Kinds = kinds
                    Authors = authors
                    Limit = limit
                    Since = since
                    Until = until
                    Events = events
                    PubKeys = pubkeys
                }
            }

        type CommonClientFilter =
             | MetadataFilter of XOnlyPubKey list * DateTime
             | ContactsFilter of XOnlyPubKey list * DateTime
             | TextNoteFilter of XOnlyPubKey list * DateTime
             | LinkedEvents of EventId list * DateTime
             | AllNotes of DateTime
             | AllMetadata of DateTime
             | DirectMessageFilter of XOnlyPubKey

        let singleton : Filter = {
             Kinds = []
             Authors = []
             Limit = 500
             Since = DateTime.UnixEpoch
             Until = DateTime.UtcNow.AddYears 1
             Events = []
             PubKeys = [] 
             }
        
        let toFilter = function
            | MetadataFilter (authors, until) ->
                { singleton with
                   Kinds = [Kind.Metadata]
                   Authors = authors
                   Until = until }
            | ContactsFilter (authors, until) ->
                { singleton with
                    Kinds = [Kind.Contacts]
                    Authors = authors
                    Until = until }
            | TextNoteFilter (authors, until) ->
                { singleton with
                    Kinds = [Kind.Text]
                    Authors = authors
                    Until = until }
            | LinkedEvents (eventIds, until) -> 
                { singleton with
                    Kinds = [Kind.Text]
                    Events = eventIds
                    Until = until }
            | AllNotes since ->
                { singleton with
                    Kinds = [Kind.Text]
                    Since = since }
            | AllMetadata until ->
                { singleton with
                    Kinds = [Kind.Metadata]
                    Until = until }
            | DirectMessageFilter from ->
                { singleton with
                    Kinds = [Kind.Encrypted]
                    Authors = [from] }

    type SubscriptionId = string

    module Request =
        type ClientMessage =
            | CMEvent of NostrEvent
            | CMSubscribe of SubscriptionId * Query.Filter list
            | CMUnsubscribe of SubscriptionId
            | CMUnknown
            
            with
            static member ToJson(msg : ClientMessage) =
                match msg with
                | CMEvent event -> ToJsonDefaults.ToJson (("EVENT", event))
                | CMSubscribe (subscriptionId, filters) -> ToJsonDefaults.ToJson  (([String "REQ"; String subscriptionId ] @ [for filter in filters do filter |> Json.serialize]))
                | CMUnsubscribe subscriptionId -> ToJsonDefaults.ToJson (("CLOSE", subscriptionId))
                | CMUnknown _ -> ToJsonDefaults.ToJson (()) 
                
            static member FromJson(_: ClientMessage) = fun json ->
                let serialized =
                    match json with
                    | Array [String "EVENT"; event] -> CMEvent(event |> Json.deserialize)
                    | Array ((String "REQ")::(String subscriptionId)::(filter)::filters) -> CMSubscribe(subscriptionId, [filter |> Json.deserialize] @ [for filter in filters do filter |> Json.deserialize])
                    | Array [String "CLOSE"; String subscriptionId] -> CMUnsubscribe(subscriptionId)
                    | _ -> CMUnknown 
                Value serialized, json 
        
        let serialize (msg: ClientMessage) =
            msg |> Json.serialize |> Json.format
            
        let deserialize (str: string) : ClientMessage =
            str |> Json.parse |> Json.deserialize
        
    module Response =
        type RelayMessage =
            | RMEvent of SubscriptionId * NostrEvent
            | RMNotice of string
            | RMACK of EventId * bool * string
            | RMEOSE of string
            with

            static member ToJson(msg : RelayMessage) =
                match msg with
                | RMEvent (subscriptionId, event) -> ToJsonDefaults.ToJson (("EVENT", subscriptionId, event))
                | RMNotice message -> ToJsonDefaults.ToJson (("NOTICE", message))
                | RMACK (eventId, success, message) -> ToJsonDefaults.ToJson (("OK", eventId, success, message))
                | RMEOSE message -> ToJsonDefaults.ToJson (("EOSE", message))

            static member FromJson(_: RelayMessage) = fun json ->
                match json with
                | Array [String "EVENT"; String subscriptionId; event] -> Value <| RMEvent (subscriptionId, event |> Json.deserialize), json
                | Array [String "NOTICE"; String message] -> Value <| RMNotice (message), json
                | Array [String "OK";  String64 eventId; Bool success; String message] -> Value <| RMACK (EventId eventId, success, message), json
                | Array [String "EOSE"; String subscriptionId] -> Value <| RMEOSE (subscriptionId), json
                | _ -> Error "Unexpected message format from the relay", json

        let serialize (msg: RelayMessage) =
            msg |> Json.serialize |> Json.format  
        
        let toPayload (msg: RelayMessage) =
            msg
            |> serialize
            |> Encoding.UTF8.GetBytes
            |> ArraySegment
            
        let deserialize (str: string) : RelayMessage =
            str |> Json.parse |> Json.deserialize
            
            
    open Response
    
    type Reader<'environment,'a> = Reader of ('environment -> 'a)
    let run environment (Reader action) =  
        let resultOfAction = action environment 
        resultOfAction
    
    let readWebSocketMessage (ws:WebSocket) =
        let rec readMessage (mem:MemoryStream) = async {
            let buffer = ArraySegment (ArrayPool.Shared.Rent(1024))
            let! result = ws.ReceiveAsync(buffer, CancellationToken.None) |> Async.AwaitTask
            mem.Write (buffer.Array, buffer.Offset, result.Count)
            ArrayPool.Shared.Return buffer.Array
            if result.EndOfMessage then
                return mem.ToArray()
            else
                return! readMessage mem
        }
        readMessage (new MemoryStream (4 * 1024))

    let startReceiving (fn: NostrEvent * bool -> unit) = 
        let rec loop (ws) = async {
            let! payload = (readWebSocketMessage ws)
            let relayMsg : RelayMessage = Encoding.UTF8.GetString payload |> Json.parse |> Json.deserialize
            match relayMsg with
            | RMEvent (subscriptionId, event) ->
                fn (event, (verify event)) |> ignore
                Console.WriteLine ("received:")
                Console.WriteLine (Encoding.UTF8.GetString payload)
            | RMNotice notice -> 1 |> ignore
            | RMACK (eid, s, ack) -> 1 |> ignore
            | RMEOSE s -> 1 |> ignore
            do! loop (ws)
        }
        Reader (fun (ws:WebSocket) -> loop ws)

    let sender () =
        let createPusher (ws:WebSocket) =
            MailboxProcessor<Request.ClientMessage>.Start (fun inbox ->
                let rec loop () = async { 
                    let! msg = inbox.Receive()
                    let serializedMessage = msg |> Request.serialize
                    Console.WriteLine("sent:")
                    Console.WriteLine(serializedMessage)
                    let payload = serializedMessage |> Encoding.UTF8.GetBytes
                    do! ws.SendAsync( ArraySegment(payload), WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
                    return! loop() }
                loop () )
            
        Reader(fun (ws:WebSocket) ->
            let pusher = createPusher ws
            let pushToRelay x =
                pusher.Post x
            
            pushToRelay)
