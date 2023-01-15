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
        let kindsToJson (kinds : Kind list) =
            Array <| (kinds |> List.map (decimal >> Number))
            
        type Filter =
             | MetadataFilter of XOnlyPubKey list * DateTime
             | ContactsFilter of XOnlyPubKey list * DateTime
             | TextNoteFilter of XOnlyPubKey list * DateTime
             | LinkedEvents of EventId list * DateTime
             | AllNotes of DateTime
             | AllMetadata of DateTime
             | DirectMessageFilter of XOnlyPubKey
             with
             static member ToJson (filter: Filter) = json {
                match filter with
                | MetadataFilter (pubKeys, dateTime) ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Metadata ]
                    do! Json.write "authors" pubKeys
                    do! Json.write "limit" 1
                    do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
                | ContactsFilter (pubKeys, dateTime) ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Contacts ]
                    do! Json.write "authors" pubKeys
                    do! Json.write "limit" 500
                    do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
                | DirectMessageFilter (pubKey) ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Encrypted ]
                    do! Json.write "authors" pubKey
                    do! Json.write "limit" 1
                    do! Json.writeWith dateTimeToJson "since" (DateTime.UtcNow.AddSeconds -180.0)
                | TextNoteFilter (pubKeys, dateTime) ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Text; Kind.Deleted ]
                    do! Json.write "authors" pubKeys
                    do! Json.write "limit" 500
                    do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
                | LinkedEvents (eventIds, dateTime) ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Text ]
                    do! Json.write "limit" 500
                    do! Json.write "#e" (Array <| (List.map (Json.serialize) eventIds))
                    do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
                | AllNotes dateTime ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Text ]
                    do! Json.write "limit" 500
                    do! Json.writeWith dateTimeToJson "since" (dateTime.AddSeconds -180.0)
                | AllMetadata dateTime ->
                    do! Json.writeWith kindsToJson "kinds" [ Kind.Metadata ]
                    do! Json.write "limit" 500
                    do! Json.writeWith dateTimeToJson "until" (dateTime.AddSeconds 60.0)
            }

    type SubscriptionId = string

    type ClientMessage =
        | CMEvent of Event
        | CMSubscribe of SubscriptionId * Query.Filter list
        | CMUnsubscribe of SubscriptionId

    type RelayMessage =
        | RMEvent of SubscriptionId * Event.Event
        | RMNotice of string
        | RMACK of EventId * bool * string
        | RMEOSE of string
    
    let serialize (msg: ClientMessage) =
        let json =
            match msg with
            | CMEvent event -> [ String "EVENT"; event |> Json.serialize ]
            | CMSubscribe (subscriptionId, filters) -> [ String "REQ"; String subscriptionId ] @ [for filter in filters do Json.serialize filter]
            | CMUnsubscribe subscriptionId -> [ String "CLOSE"; subscriptionId |> Json.serialize ]
        json |> Json.serialize |> Json.format 

    let deserialize (payload: string) =
        let json = Json.parse payload
        match json with
        | Array [String "EVENT"; String subscriptionId; event] -> Ok <| RMEvent (subscriptionId, event |> Json.deserialize)
        | Array [String "NOTICE"; String message] -> Ok <| RMNotice (message)
        | Array [String "OK";  String64 eventId; Bool success; String message] -> Ok <| RMACK (EventId eventId, success, message)
        | Array [String "EOSE"; String subscriptionId] -> Ok <| RMEOSE (subscriptionId)
        | _ -> Result.Error "Unexpected message format from the relay"

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
        
        
    let startReceiving (fn: Event * bool -> unit) = 
        let rec loop (ws) = async {
            let! payload = (readWebSocketMessage ws)
            let relayMsg = deserialize (Encoding.UTF8.GetString payload)
            match relayMsg with
            | Ok (RMEvent (subscriptionId, event)) ->
                fn (event, (verify event)) |> ignore
                //Console.WriteLine (Encoding.UTF8.GetString payload)
            | Ok (RMNotice notice) -> 1 |> ignore
            | Ok (RMACK (eid, s, ack)) -> 1 |> ignore
            | Ok (RMEOSE s) -> 1 |> ignore
            | Result.Error error -> 1 |> ignore
            do! loop (ws)
        }
        Reader (fun (ws:WebSocket) -> loop ws)
    
    let sender () =
        let createPusher (ws:WebSocket) =
            MailboxProcessor<ClientMessage>.Start (fun inbox ->
                let rec loop () = async { 
                    let! msg = inbox.Receive()
                    let payload = msg |> serialize |> Encoding.UTF8.GetBytes
                    do! ws.SendAsync( ArraySegment(payload), WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
                    return! loop() }
                loop () )
            
        Reader(fun (ws:WebSocket) ->
            let pusher = createPusher ws
            let pushToRelay x =
                pusher.Post x
            
            pushToRelay)
