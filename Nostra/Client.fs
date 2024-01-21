namespace Nostra

open System
open System.IO
open System.Net.WebSockets
open System.Text
open System.Threading
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control
open System.Buffers
open Nostra.ClientContext
open Thoth.Json.Net

module Client =

    type SubscriptionFilter =
        { Ids: EventId list
          Kinds: Kind list
          Authors: AuthorId list
          Limit: int option
          Since: DateTime option
          Until: DateTime option
          Events: EventId list
          PubKeys: AuthorId list }

    module Request =
        [<RequireQualifiedAccess>]
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

                let filter (filter: SubscriptionFilter) =
                    []
                    |> encodeList "ids" filter.Ids Encode.eventId
                    |> encodeList "kinds" filter.Kinds Encode.Enum.int
                    |> encodeList "authors" filter.Authors Encode.author
                    |> encodeList "#e" filter.Events Encode.eventId
                    |> encodeList "#p" filter.PubKeys Encode.author
                    |> encodeOption "limit" filter.Limit Encode.int
                    |> encodeOption "since" filter.Since Encode.unixDateTime
                    |> encodeOption "until" filter.Until Encode.unixDateTime
                    |> Encode.object

            let all: SubscriptionFilter =
                { Ids = []
                  Kinds = []
                  Authors = []
                  Limit = None
                  Since = None
                  Until = None
                  Events = []
                  PubKeys = [] }

            let notes filter = { filter with Kinds = [Kind.Text] }
            let metadata filter = { filter with Kinds = [Kind.Metadata] }
            let contacts filter = { filter with Kinds = [Kind.Contacts] }
            let encryptedMessages filter = { filter with Kinds = [Kind.Encrypted] }
            let channelCreation channels filter = { filter with Ids = channels }

            let events evnts filter =
                { filter with
                    Events =
                       evnts
                       |> List.append filter.Events
                       |> List.distinct }

            let authors authors (filter : SubscriptionFilter) =
                { filter with
                    Authors =
                       authors
                       |> List.append filter.Authors
                       |> List.distinct }

            let channels channels filter =
                let filter' = events channels filter
                { filter' with
                    Kinds = Kind.ChannelMessage :: filter'.Kinds }

            let since dateTime filter =
                { filter with Since = Some dateTime }

            let until dateTime filter =
                { filter with Until = Some dateTime }

            let limit count filter =
                { filter with Limit = Some count }

        type ClientMessage =
            | CMEvent of Event
            | CMSubscribe of SubscriptionId * SubscriptionFilter list
            | CMUnsubscribe of SubscriptionId

        module Encode =

            let clientMessage =
                function
                | CMEvent event -> Encode.tuple2 Encode.string Event.Encode.event ("EVENT", event)
                | CMUnsubscribe subscriptionId -> Encode.tuple2 Encode.string Encode.string ("CLOSE", subscriptionId)
                | CMSubscribe(subscriptionId, filter) ->
                    Encode.list (
                        [ Encode.string "REQ"; Encode.string subscriptionId ]
                        @ (filter |> List.map Filter.Encode.filter)
                    )

        let serialize (msg: ClientMessage) =
            msg |> Encode.clientMessage |> Encode.toCanonicalForm

    open Request

    module Response =
        type RelayMessage =
            | RMEvent of SubscriptionId * Event
            | RMNotice of string
            | RMACK of EventId * bool * string
            | RMEOSE of string

        module Decode =
            let relayMessage: Decoder<RelayMessage> =
                Decode.index 0 Decode.string
                |> Decode.andThen (function
                    | "EVENT" ->
                        Decode.map2
                            (fun subscriptionId event -> RMEvent(subscriptionId, event))
                            (Decode.index 1 Decode.string)
                            (Decode.index 2 Event.Decode.event)
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

        let deserialize (str: string) =
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
                   | RMEvent (subscriptionId, event) ->
                        if (Event.verify event) then
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

    let connectToRelay uri =
        let ws = new ClientWebSocket()
        let ctx = Communication.buildContext ws Console.Out
        let pushToRelay = Monad.injectedWith ctx (Communication.sender ())
        let receiveLoop onReceiving = Monad.injectedWith ctx (Communication.startReceiving onReceiving)
        async {
            use connectCancellationToken = new CancellationTokenSource(TimeSpan.FromSeconds(3))
            let cts = CancellationTokenSource.CreateLinkedTokenSource(Async.DefaultCancellationToken, connectCancellationToken.Token)
            do! ws.ConnectAsync (uri, cts.Token) |> Async.AwaitTask

            let subscribe sid filters = pushToRelay (ClientMessage.CMSubscribe (sid, filters))
            let publish event = pushToRelay (ClientMessage.CMEvent event)

            return {|
                publish = publish
                subscribe = subscribe
                startListening = receiveLoop
            |}
        }


    [<CompiledName("ConnectToRelayAsync")>]
    let connectToRelayAsync uri =
        connectToRelay uri
        |> Async.StartAsTask

