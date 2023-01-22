namespace Nostra.Core

open System
open System.IO
open System.Text
open System.Threading
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open System.Buffers
open System.Net.WebSockets
open Thoth.Json.Net

module Query =
    type Filter = {
        Ids: EventId list
        Kinds: Kind list 
        Authors: XOnlyPubKey list 
        Limit: int option 
        Since: DateTime option 
        Until: DateTime option 
        Events: EventId list 
        PubKeys: XOnlyPubKey list 
    }
    module Filter =
        let decoder : Decoder<Filter> =
            Decode.object (fun get -> {
                Ids = get.Optional.Field "ids" (Decode.list Decode.eventId) |> Option.defaultValue [] 
                Kinds = get.Optional.Field "kinds" (Decode.list Decode.Enum.int) |> Option.defaultValue []
                Authors = get.Optional.Field "authors" (Decode.list Decode.xOnlyPubkey) |> Option.defaultValue []
                Limit = get.Optional.Field "limit" Decode.int
                Since = get.Optional.Field "since" Decode.unixDateTime
                Until = get.Optional.Field "until" Decode.unixDateTime
                Events = get.Optional.Field "#e" (Decode.list Decode.eventId) |> Option.defaultValue []
                PubKeys = get.Optional.Field "#p" (Decode.list Decode.xOnlyPubkey) |> Option.defaultValue []
            })

        let encodeList field list encoder map : (string * JsonValue) list =
            match list with
            | [] -> map
            | _ -> (field, Encode.list (list |> List.map encoder)) :: map
            
        let encodeOption field opt encoder map : (string * JsonValue) list =
            match opt with
            | None -> map
            | Some v -> (field, encoder v) :: map
            
        let encoder (filter : Filter) =
            []
            |> encodeList "ids" filter.Ids Encode.eventId
            |> encodeList "kinds" filter.Kinds Encode.Enum.int
            |> encodeList "authors" filter.Authors Encode.xOnlyPubkey
            |> encodeList "#e" filter.Events Encode.eventId
            |> encodeList "#p" filter.PubKeys Encode.xOnlyPubkey
            |> encodeOption "limit" filter.Limit Encode.int
            |> encodeOption "since" filter.Since Encode.unixDateTime 
            |> encodeOption "until" filter.Until Encode.unixDateTime 
            |> Encode.object

    type CommonClientFilter =
         | MetadataFilter of XOnlyPubKey list * DateTime
         | ContactsFilter of XOnlyPubKey list * DateTime
         | TextNoteFilter of XOnlyPubKey list * DateTime
         | LinkedEvents of EventId list * DateTime
         | AllNotes of DateTime
         | AllMetadata of DateTime
         | DirectMessageFilter of XOnlyPubKey

    let singleton : Filter = {
         Ids = []
         Kinds = []
         Authors = []
         Limit = None
         Since = None
         Until = None
         Events = []
         PubKeys = [] 
         }
    
    let toFilter = function
        | MetadataFilter (authors, until) ->
            { singleton with
               Kinds = [Kind.Metadata]
               Authors = authors
               Until = Some until }
        | ContactsFilter (authors, until) ->
            { singleton with
                Kinds = [Kind.Contacts]
                Authors = authors
                Until = Some until }
        | TextNoteFilter (authors, until) ->
            { singleton with
                Kinds = [Kind.Text]
                Authors = authors
                Until = Some until }
        | LinkedEvents (eventIds, until) -> 
            { singleton with
                Kinds = [Kind.Text]
                Events = eventIds
                Until = Some until }
        | AllNotes since ->
            { singleton with
                Kinds = [Kind.Text]
                Since = Some since }
        | AllMetadata until ->
            { singleton with
                Kinds = [Kind.Metadata]
                Until = Some until }
        | DirectMessageFilter from ->
            { singleton with
                Kinds = [Kind.Encrypted]
                Authors = [from] }

type SubscriptionId = string

module Request =
    type ClientMessage =
        | CMEvent of Event
        | CMSubscribe of SubscriptionId * Query.Filter list
        | CMUnsubscribe of SubscriptionId
    module ClientMessage =
        module Decode =
            let cmEvent : Decoder<ClientMessage> =
                Decode.tuple2 (Decode.expect "EVENT") Decode.event
                |> Decode.andThen (fun (_, event) -> Decode.succeed (CMEvent event))

            let cmUnsubscribe : Decoder<ClientMessage> =
                Decode.tuple2 (Decode.expect "CLOSE") Decode.string
                |> Decode.andThen (fun (_, subscriptionId) -> Decode.succeed (CMUnsubscribe subscriptionId))
        
            let cmSubscribe : Decoder<ClientMessage> =
                fun path value -> 
                    if Decode.Helpers.isArray value then
                        let items = Decode.Helpers.asArray value
                        let len = items.Length
                        if len >= 3 then
                            let commandResult = Decode.index 0 (Decode.string) path value 
                            let subscriptionIdResult = Decode.index 1 (Decode.string) path value
                            let filtersResult =
                                (Ok [], items |> Array.mapi (fun i x -> i, x) |> Array.skip 2 )
                                ||> Array.fold (fun acc values ->
                                        match acc with
                                        | Error _ -> acc
                                        | Ok acc ->
                                            match Decode.index (fst values) (Query.Filter.decoder) path value with
                                            | Error er -> Error er
                                            | Ok value -> Ok (value::acc))
                            match commandResult, subscriptionIdResult, filtersResult with
                            | Ok ("REQ"), Ok(subscriptionId), Ok(filters) -> Ok (CMSubscribe(subscriptionId, filters))
                            | _ -> Error (path, BadType("REQ", value))
                        else
                            Error (path, BadType ("REQ array is too short", value))
                    else
                        Error (path, BadType ("REQ is not and array", value))

            let clientMessage : Decoder<ClientMessage> =
                Decode.oneOf [
                    cmEvent
                    cmUnsubscribe
                    cmSubscribe
                ]

        module Encode =
            let clientMessage = function
                | CMEvent event -> Encode.tuple2 (Encode.string) (Encode.event) ("EVENT", event)
                | CMUnsubscribe subscriptionId -> Encode.tuple2 (Encode.string) (Encode.string) ("CLOSE", subscriptionId)
                | CMSubscribe (subscriptionId, filter) ->
                    Encode.list ([
                        Encode.string "REQ"
                        Encode.string subscriptionId
                        ] @ (filter |> List.map Query.Filter.encoder))
                    
    
        let serialize (msg: ClientMessage) =
            msg |> Encode.clientMessage |> Encode.toCompactString
            
        let deserialize str  =
            Decode.fromString (Decode.clientMessage) str
    
open Request

module Response =
    type RelayMessage =
        | RMEvent of SubscriptionId * Event
        | RMNotice of string
        | RMACK of EventId * bool * string
        | RMEOSE of string

    module Decode =
        let rmEvent : Decoder<RelayMessage> =
            Decode.tuple3 (Decode.expect "EVENT") Decode.string Decode.event
            |> Decode.andThen (fun (_, subscriptionId, event) -> Decode.succeed (RMEvent (subscriptionId, event)))

        let rmNotice : Decoder<RelayMessage> =
            Decode.tuple2 (Decode.expect "NOTICE") Decode.string
            |> Decode.andThen (fun (_, message) -> Decode.succeed (RMNotice message))

        let rmEose : Decoder<RelayMessage> =
            Decode.tuple2 (Decode.expect "EOSE") Decode.string 
            |> Decode.andThen (fun (_, message) -> Decode.succeed (RMNotice message))

        let rmAck : Decoder<RelayMessage> =
            Decode.tuple4 (Decode.expect "OK") Decode.eventId Decode.bool Decode.string
            |> Decode.andThen (fun (_, eventId, success, message) -> Decode.succeed (RMACK (eventId, success, message)))

        let relayMessage : Decoder<RelayMessage> =
            Decode.oneOf [
                rmEvent
                rmNotice
                rmEose
                rmAck
            ]

    module Encode =
        let relayMessage = function
            | RMEvent (subscriptionId, event) ->
                Encode.tuple3
                    Encode.string
                    Encode.string
                    Encode.event
                    ("EVENT", subscriptionId, event)
            | RMNotice message ->
                Encode.tuple2
                    Encode.string
                    Encode.string
                    ("NOTICE", message)
            | RMACK (eventId, success, message) ->
                Encode.tuple4
                    Encode.string
                    Encode.eventId
                    Encode.bool
                    Encode.string
                    ("OK", eventId, success, message)
            | RMEOSE subscriptionId ->
                Encode.tuple2
                    Encode.string
                    Encode.string
                    ("EOSE", subscriptionId)

    let serialize (msg: RelayMessage) =
        msg |> Encode.relayMessage |> Encode.toCompactString

    let toPayload (msg: RelayMessage) =
        msg
        |> serialize
        |> Encoding.UTF8.GetBytes
        |> ArraySegment
        
    let deserialize (str: string)  =
        Decode.fromString (Decode.relayMessage) str
        
        
open Response

module Client =
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
        let rec loop (ws: WebSocket) = async {
            let! payload = (readWebSocketMessage ws)
            let relayMsgResult = Encoding.UTF8.GetString payload |> deserialize
            match relayMsgResult with
            | Error e -> 0 |> ignore
            | Ok relayMsg ->
                match relayMsg with
                | RMEvent (subscriptionId, event) ->
                    fn (event, (verify event)) |> ignore
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
                    let serializedMessage = msg |> ClientMessage.serialize
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
