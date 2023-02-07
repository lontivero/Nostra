namespace Nostra

open System
open System.IO
open System.Net.WebSockets
open System.Text
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open System.Buffers
open Nostra.ClientContext
open Thoth.Json.Net

module Client =
    open Nostra.Event
    
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
            msg |> Encode.clientMessage |> Encode.toCanonicalForm

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

    open Response
    
    module Communication =
        let readWebSocketMessage (ctx: Context) =
            let rec readMessage (mem:MemoryStream) = async {
                let buffer = ArrayPool.Shared.Rent(1024)
                let! result = ctx.WebSocket.read buffer
                mem.Write (buffer, 0, result.Count)
                ArrayPool.Shared.Return buffer
                if result.EndOfMessage then
                    return mem.ToArray()
                else
                    return! readMessage mem
            }
            readMessage (new MemoryStream (4 * 1024))

        let receiveMessage =
            Monad.Reader (fun (ctx: Context) -> async {
                let! payload = (readWebSocketMessage ctx)
                return payload
                |> Encoding.UTF8.GetString
                |> deserialize
                |> Result.bind (fun relayMsg ->
                   match relayMsg with
                   | RMEvent (_, event) ->
                        if (verify event) then
                            Ok relayMsg
                        else
                            Error "Invalid message received"
                   | _ -> Ok relayMsg )
            })
        
        let rec startReceiving callback = 
            let rec loop (ctx: Context) = async {
                let (Monad.Reader r ) = receiveMessage 
                let! message = r ctx
                callback message
                do! loop ctx
            }
            Monad.Reader (fun (ctx: Context) -> loop ctx)

        let sender () =
            let createPusher (ctx: Context) =
                MailboxProcessor<Request.ClientMessage>.Start (fun inbox ->
                    let rec loop () = async { 
                        let! msg = inbox.Receive()
                        let serializedMessage = msg |> Request.serialize
                        let payload = serializedMessage |> Encoding.UTF8.GetBytes
                        do! ctx.WebSocket.write payload
                        return! loop() }
                    loop () )
                
            Monad.Reader(fun (ctx: Context) ->
                let pusher = createPusher ctx 
                pusher.Post)

        let buildContext (ws: WebSocket) (log: TextWriter) = {
                WebSocket = {
                    write = 
                        fun arr -> async {
                            let! ct = Async.CancellationToken
                            do! ws.SendAsync( ArraySegment(arr), WebSocketMessageType.Text, true, ct ) |> Async.AwaitTask
                        }
                    read =
                        fun buffer -> async {
                            let! ct = Async.CancellationToken
                            let! result = ws.ReceiveAsync(ArraySegment(buffer), ct) |> Async.AwaitTask
                            return { Count = result.Count; EndOfMessage = result.EndOfMessage }
                        }
                }
                Logger = {
                    logInfo = log.WriteLine
                    logDebug = log.WriteLine
                    logError = log.WriteLine
                }
            }
