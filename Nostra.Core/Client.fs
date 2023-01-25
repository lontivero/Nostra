namespace Nostra.Core

open System
open System.IO
open System.Text
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open System.Buffers
open Thoth.Json.Net

module Client =
    open Nostra.Core.Event
    
    module Request =
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
            
            module Encode =
                let private encodeList field list encoder map : (string * JsonValue) list =
                    match list with
                    | [] -> map
                    | _ -> (field, Encode.list (list |> List.map encoder)) :: map
                    
                let private encodeOption field opt encoder map : (string * JsonValue) list =
                    match opt with
                    | None -> map
                    | Some v -> (field, encoder v) :: map
                    
                let filter (filter : Filter) =
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

            module FilterUtils =
                type ClientFilter =
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

        type ClientMessage =
            | CMEvent of Event
            | CMSubscribe of SubscriptionId * Filter list
            | CMUnsubscribe of SubscriptionId

        module Encode =
            open Filter
            
            let clientMessage = function
                | CMEvent event -> Encode.tuple2 Encode.string Encode.event ("EVENT", event)
                | CMUnsubscribe subscriptionId -> Encode.tuple2 Encode.string Encode.string ("CLOSE", subscriptionId)
                | CMSubscribe (subscriptionId, filter) ->
                    Encode.list ([
                        Encode.string "REQ"
                        Encode.string subscriptionId
                        ] @ (filter |> List.map Encode.filter))

        let serialize (msg: ClientMessage) =
            msg |> Encode.clientMessage |> Encode.toCompactString

    module Response =
        type RelayMessage =
            | RMEvent of SubscriptionId * Event
            | RMNotice of string
            | RMACK of EventId * bool * string
            | RMEOSE of string

        module Decode =
            let relayMessage : Decoder<RelayMessage> =
                Decode.index 0 Decode.string
                |> Decode.andThen ( function
                    | "EVENT" ->
                        Decode.map2
                            (fun subscriptionId event -> RMEvent(subscriptionId, event))
                            (Decode.index 1 Decode.string)
                            (Decode.index 2 Decode.event)
                    | "NOTICE" ->
                        Decode.map
                            (fun message -> RMNotice message)
                            (Decode.index 1 Decode.string)
                    | "EOSE" ->
                        Decode.map
                            (fun message -> RMEOSE message)
                            (Decode.index 1 Decode.string)
                    | "OK" ->
                        Decode.map3
                            (fun eventId success message -> RMACK(eventId, success, message))
                            (Decode.index 1 Decode.eventId)
                            (Decode.index 2 Decode.bool)
                            (Decode.index 3 Decode.string)
                    | _ -> Decode.fail "Unknown message from the relay")

        let deserialize (str: string)  =
            Decode.fromString Decode.relayMessage str

    module Communication =
        open WebSocket
        let readWebSocketMessage (ws:WebSocket) =
            let rec readMessage (mem:MemoryStream) = async {
                let buffer = ArrayPool.Shared.Rent(1024)
                let! result = ws.read(buffer)
                mem.Write (buffer, 0, result.Count)
                ArrayPool.Shared.Return buffer
                if result.EndOfMessage then
                    return mem.ToArray()
                else
                    return! readMessage mem
            }
            readMessage (new MemoryStream (4 * 1024))

        let startReceiving callback = 
            let rec loop (ws: WebSocket) = async {
                let! payload = (readWebSocketMessage ws)
                payload
                |> Encoding.UTF8.GetString
                |> Response.deserialize
                |> Result.bind (fun relayMsg ->
                    match relayMsg with
                    | Response.RMEvent (subscriptionId, event) ->
                        if (verify event) then
                            Ok relayMsg
                        else
                            Error "Invalid message received"
                    | _ -> Ok relayMsg
                    )
                |> callback
                do! loop (ws)
            }
            Reader.Reader (fun (ws:WebSocket) -> loop ws)

        let sender () =
            let createPusher (ws:WebSocket) =
                MailboxProcessor<Request.ClientMessage>.Start (fun inbox ->
                    let rec loop () = async { 
                        let! msg = inbox.Receive()
                        let serializedMessage = msg |> Request.serialize
                        Console.WriteLine("sent:")
                        Console.WriteLine(serializedMessage)
                        let payload = serializedMessage |> Encoding.UTF8.GetBytes
                        do! ws.write(payload)
                        return! loop() }
                    loop () )
                
            Reader.Reader(fun (ws:WebSocket) ->
                let pusher = createPusher ws
                let pushToRelay x =
                    pusher.Post x
                
                pushToRelay)
