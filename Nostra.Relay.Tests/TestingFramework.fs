module TestingFramework

open System
open System.Collections.Generic
open System.Threading
open Nostra
open Nostra.Client
open Nostra.Client.Request
open Nostra.Client.Response
open Nostra.Relay.Tests

open FsUnit.Xunit

type ReceivedMessage =
    | MsgEvent of subscriptionId: string * event: Event
    | MsgEose of subscriptionId: string
    | MsgOk of eventId: string * success: bool * message: string
    | MsgNotice of message: string
    | MsgCount of subscriptionId: string * count: int

type AsyncWriter = string -> Async<unit>
type AsyncReader = Async<Result<RelayMessage, string>>

type Connection = {
    Sender: AsyncWriter
    Receiver: AsyncReader
}

type User = {
    Name: string
    Secret: SecretKey
    Connection: Connection option
    SentEvents: Event ResizeArray
    ReceivedMessages: ReceivedMessage ResizeArray
}

module User =
    let create name secret = {
        Name = name
        Secret = secret
        Connection = None
        SentEvents = ResizeArray<Event>()
        ReceivedMessages = ResizeArray<ReceivedMessage>()
    }

    let createWithRandomKey name =
        create name (SecretKey.createNewRandom())

    let createWithKey name privateKeyHex =
        match SecretKey.fromHex privateKeyHex with
        | Some secret -> create name secret
        | None -> failwith $"Invalid private key hex: {privateKeyHex}"

type TestContext = {
    Users: Dictionary<string, User>
    CurrentUser: string
    Port: int
    Cts: CancellationTokenSource
}

type TestStep = TestContext -> Async<TestContext>
type FilterFactory = TestContext -> string
type EventSpec = {
    Id: string option
    Content: string
    Kind: int
    CreatedAt: int64
    Tags: Tag list
}

let ($) prev next = async.Bind(prev, next)

let currentUser ctx = ctx.Users[ctx.CurrentUser]

let ``start relay`` () = async {
    let cts = new CancellationTokenSource()
    let port = Relay.startRelay cts.Token
    return { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port; Cts = cts }
}

let ``start relay with limitations`` (limitations: Nostra.Relay.InfoDocument.Limitation) = async {
    let cts = new CancellationTokenSource()
    let port = Relay.startRelayWithLimitations cts.Token limitations
    return { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port; Cts = cts }
}

let ``given`` userName privateKeyHex : TestStep =
    fun ctx ->
        let exists, _ = ctx.Users.TryGetValue(userName)
        if not exists then
            let user = User.createWithKey userName privateKeyHex
            ctx.Users.Add(userName, user)
        async { return { ctx with CurrentUser = userName } }

let ``connect to relay`` : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | None ->
            let! sender, receiver = Client.createClient ctx.Port
            let connection = { Sender = sender; Receiver = receiver }
            ctx.Users[ctx.CurrentUser] <- { user with Connection = Some connection }
        | Some _ -> ()
        return ctx
    }

let receiveOne (conn: Connection) = async {
    let! response = conn.Receiver
    return
        match response with
        | Ok (RMEvent(subId, event)) ->
            MsgEvent(subId, event)
        | Ok (RMEOSE subId) ->
            MsgEose subId
        | Ok (RMACK(eventId, success, msg)) ->
            MsgOk(EventId.toHex eventId, success, msg)
        | Ok (RMNotice notice) ->
            MsgNotice notice
        | Ok (RMCount(subId, count)) ->
            MsgCount(subId, count)
        | Error e ->
            failwith $"Error receiving message: {e}"
}

type ExpectedMessage =
    | ExpectEvent of subscriptionId: string * predicate: (Event -> bool)
    | ExpectEose of subscriptionId: string
    | ExpectOk of success: bool
    | ExpectNotice
    | ExpectCount of subscriptionId: string * count: int

let onSub subId : string * (Event -> bool) = (subId, fun _ -> true)
let withEventId subId (eventId: EventId) : string * (Event -> bool) =
    (subId, fun e -> e.Id = eventId)
let withContent subId content : string * (Event -> bool) =
    (subId, fun e -> e.Content = content)
let matching subId (predicate: Event -> bool) : string * (Event -> bool) =
    (subId, predicate)

let ``receive messages`` (expected: ExpectedMessage list) : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            for exp in expected do
                let! msg = receiveOne conn
                user.ReceivedMessages.Add(msg)
                match exp, msg with
                | ExpectEvent(subId, _), MsgEvent(actualSubId, _) when subId <> actualSubId ->
                    failwith $"Expected EVENT on '{subId}' but got EVENT on '{actualSubId}'"
                | ExpectEvent(_, predicate), MsgEvent(_, event) ->
                    if not (predicate event) then
                        failwith $"EVENT predicate failed for event {EventId.toHex event.Id} (content: '{event.Content}')"
                | ExpectEose subId, MsgEose actualSubId ->
                    if subId <> actualSubId then
                        failwith $"Expected EOSE on '{subId}' but got EOSE on '{actualSubId}'"
                | ExpectOk success, MsgOk(_, actualSuccess, reason) ->
                    if success <> actualSuccess then
                        failwith $"Expected OK success={success} but got success={actualSuccess}: {reason}"
                | ExpectNotice, MsgNotice _ -> ()
                | ExpectCount(subId, count), MsgCount(actualSubId, actualCount) ->
                    if subId <> actualSubId then
                        failwith $"Expected COUNT on '{subId}' but got COUNT on '{actualSubId}'"
                    if count <> actualCount then
                        failwith $"Expected COUNT={count} but got COUNT={actualCount}"
                | _ ->
                    failwith $"Expected {exp} but got {msg}"
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let ``subscribe`` subscriptionId (filters: SubscriptionFilter list) : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            let filterJson =
                filters
                |> List.map (Filter.Encode.filter >> Encode.toCanonicalForm)
                |> String.concat ","
            do! conn.Sender $"""["REQ","{subscriptionId}",{filterJson}]"""
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let ``close subscription`` subscriptionId : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            do! conn.Sender $"""["CLOSE","{subscriptionId}"]"""
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let createEventWithSpec (user: User) (spec: EventSpec) : Event =
    let createdAt = Utils.fromUnixTime (int spec.CreatedAt)
    let unsigned : Event.UnsignedEvent = {
        CreatedAt = createdAt
        Kind = enum<Kind> spec.Kind
        Tags = spec.Tags
        Content = spec.Content
    }
    Event.sign user.Secret unsigned

let ``publish event`` (spec: EventSpec) : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        let event = createEventWithSpec user spec
        let serialized = Event.serialize event
        match user.Connection with
        | Some conn ->
            do! conn.Sender $"""["EVENT",{serialized}]"""
            user.SentEvents.Add(event)
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let ``publish events`` (specs: EventSpec list) : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            for spec in specs do
                let event = createEventWithSpec user spec
                let serialized = Event.serialize event
                do! conn.Sender $"""["EVENT",{serialized}]"""
                user.SentEvents.Add(event)
                let! msg = receiveOne conn
                user.ReceivedMessages.Add(msg)
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let verify f ctx =
    let result = ctx |> Async.RunSynchronously
    f result
    result.Cts.Cancel()

let event = {
    Id = None
    Content = ""
    Kind = 1
    CreatedAt = 0L
    Tags = []
}

let id (value: int) (spec: EventSpec) = { spec with Id = Some $"id{value}" }
let content value (spec: EventSpec) = { spec with Content = value }
let kind (k: Kind) (spec: EventSpec) = { spec with Kind = int k }
let created value (spec: EventSpec) = { spec with CreatedAt = value }
let tags value (spec: EventSpec) = { spec with Tags = value }

let note text createdAt = event |> content text |> kind Kind.Text |> created createdAt

let kindFilter kinds =
    Filter.all |> Filter.kinds kinds

let authorFilter authors =
    Filter.all |> Filter.authors authors

[<Literal>]
let Alice = "Alice"
[<Literal>]
let Bob = "Bob"
[<Literal>]
let Charlie = "Charlie"

let AlicePrivateKey = "512a14752ed58380496920da432f1c0cdad952cd4afda3d9bfa51c2051f91b02"
let AlicePublicKey = AuthorId.parse "5758137ec7f38f3d6c3ef103e28cd9312652285dab3497fe5e5f6c5c0ef45e75" |> Result.requiresOk
let BobPrivateKey = "3551fc7617f76632e4542992c0bc01fecb224de639c4b6a1e0956946e8bb8a29"
let BobPublicKey = AuthorId.parse "5bc683a5d12133a96ac5502c15fe1c2287986cff7baf6283600360e6bb01f627" |> Result.requiresOk
let CharliePrivateKey = "f77f81a6a223eb15f81fee569161a4f729401a9cbc31bb69fef6a949b9d3c23a"
let CharliePublicKey = AuthorId.parse "fe8d7a5726ea97ce6140f9fb06b1fe7d3259bcbf8de42c2a5d2ec9f8f0e2f614" |> Result.requiresOk

// Deletion event (Kind 5) with e-tags referencing events to delete
let deleteEvent eventIds createdAt =
    let eTags = eventIds |> List.map (fun evtId -> Tag("e", [evtId]))
    { Id = None; Content = ""; Kind = 5; CreatedAt = createdAt; Tags = eTags }

// Deletion event with a-tags for addressable events
let deleteAddressable aTags createdAt =
    let addressTags = aTags |> List.map (fun a -> Tag("a", [a]))
    { Id = None; Content = ""; Kind = 5; CreatedAt = createdAt; Tags = addressTags }

// COUNT request
let ``count`` subscriptionId (filters: SubscriptionFilter list) : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            let filterJson =
                filters
                |> List.map (Filter.Encode.filter >> Encode.toCanonicalForm)
                |> String.concat ","
            do! conn.Sender $"""["COUNT","{subscriptionId}",{filterJson}]"""
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

// Expiration event
let expirableEvent eventContent eventKind eventCreatedAt expirationUnix =
    { Id = None
      Content = eventContent
      Kind = eventKind
      CreatedAt = eventCreatedAt
      Tags = [Tag("expiration", [string expirationUnix])] }

// Event kind helper
let withKind eventKind (spec: EventSpec) = { spec with Kind = eventKind }

// Common test setup functions
let startWithAlice () =
    ``start relay`` ()
    $ ``given`` Alice AlicePrivateKey
    $ ``connect to relay``

let startWithAliceAndBob () =
    startWithAlice ()
    $ ``given`` Bob BobPrivateKey
    $ ``connect to relay``

let startWithAliceBobAndCharlie () =
    startWithAliceAndBob ()
    $ ``given`` Charlie CharliePrivateKey
    $ ``connect to relay``
