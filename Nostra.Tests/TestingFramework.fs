module TestingFramework

open System.Collections.Generic
open System.Threading
open Nostra
open Nostra.Client.Response
open Nostra.Relay.Configuration
open Nostra.Relay.InfoDocument
open Nostra.Tests
open FsUnit.Xunit

type AsyncWriter = string -> Async<unit>
type AsyncReader = Async<Result<RelayMessage,string>>
type Connection = {
    Sender : AsyncWriter
    Receiver : AsyncReader
}
type User = {
    SentEvents : Event ResizeArray
    ReceivedEvents : Event ResizeArray
    Secret : SecretKey
    Connection : Connection option
    Errors : string ResizeArray
}

module User =
    let createDefault () = {
        Secret = SecretKey.createNewRandom()
        SentEvents = ResizeArray<Event>()
        ReceivedEvents = ResizeArray<Event>()
        Connection = None
        Errors = ResizeArray<string>()
    }

type TestContext = {
    Users: Dictionary<string, User>
    CurrentUser: string
    Port: int
}

type TestStep = TestContext -> Async<TestContext>
type FilterFactory = TestContext -> string
type EventFactory = TestContext -> Event.UnsignedEvent

let ($) prev next = prev |> Async.bind next

let currentUser ctx = ctx.Users[ctx.CurrentUser]

let ``start relay`` () = async {
    use cts = new CancellationTokenSource()
    let port = Relay.startRelay cts.Token
    let testContext = { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port }
    return testContext
}

let ``start relay with limits`` (limits : Limitation) = async {
    use cts = new CancellationTokenSource()
    let port = Relay.startRelayWithLimitations cts.Token limits
    let testContext = { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port }
    return testContext
}

let ``start relay with write policy`` (writePolicy : WritePolicy) = async {
    use cts = new CancellationTokenSource()
    let port = Relay.startRelayWithWritePolicy cts.Token writePolicy
    let testContext = { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port }
    return testContext
}

let ``given`` user : TestStep =
    fun ctx ->
        let alreadyExists, knownUser = ctx.Users.TryGetValue(user)
        if not alreadyExists then
            ctx.Users.Add (user, User.createDefault ())
        async { return { ctx with CurrentUser = user } }

let ``connect to relay`` : TestStep =
    fun ctx -> async {
        let curUser = currentUser(ctx)
        match curUser.Connection with
        | None ->
            let! sender, receiver = Client.createClient ctx.Port
            let connection = { Sender = sender; Receiver = receiver }
            ctx.Users[ctx.CurrentUser]  <- { curUser with Connection = Some connection }
        | Some _ ->
            ()
        return ctx
    }

let ``wait for event`` subscriptionId : TestStep =
    let rec receiveEvents receiver (events: Event ResizeArray) = async {
        let! response = receiver
        match response with
        | Ok (RMEvent(subscriptionId, event)) ->
            events.Add event
        | _ -> failwith "Unexpected message"
    }

    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            do! receiveEvents conn.Receiver user.ReceivedEvents
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let ``subscribe to`` subscriptionId (filterFactory: FilterFactory): TestStep =
    let rec receiveEvents subscriptionId receiver (user: User) = async {
        let! response = receiver
        match response with
        | Ok (RMEvent(subscriptionId, event)) ->
            user.ReceivedEvents.Add event
            do! receiveEvents subscriptionId receiver user
        | Ok (RMEOSE subscriptionId) ->
            ()
        | Ok (RMNotice(notice)) ->
            user.Errors.Add notice
            ()
        | _ -> failwith "Unexpected message"
    }

    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            do! conn.Sender $"""["REQ","{subscriptionId}",{filterFactory ctx}]"""
            do! receiveEvents subscriptionId conn.Receiver user
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let ``subscribe to all events`` : TestStep =
    ``subscribe to`` "all" (fun _ -> "{}")

let notes : FilterFactory =
    fun ctx -> """{"kinds": [1]}"""

let events : FilterFactory =
    fun ctx -> """{}"""

let latest n : FilterFactory =
    fun ctx -> $"""{{"limit": {n}}}"""

let eventsFrom who : FilterFactory =
    fun ctx ->
        let user = ctx.Users[who]
        let author = user.Secret |> SecretKey.getPubKey |> fun x -> AuthorId.toBytes x |> Utils.toHex
        $"""{{"authors": ["{author}"]}}"""

let ``send raw`` messageFactory : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            do! conn.Sender (messageFactory ctx)
            let! response = conn.Receiver
            match response with
            | Ok (RMACK(_, true, _)) -> should equal true true
            | Ok (RMACK(_, false, reason)) -> user.Errors.Add reason
            | Ok (RMNotice(notice)) -> user.Errors.Add notice
            | Result.Error e -> user.Errors.Add (e.ToString())
            | _ -> failwith "error"
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }
let ``send event`` eventFactory : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        let signedEvent = eventFactory ctx |> Event.sign user.Secret
        let serializedEvent = Event.serialize signedEvent
        let! ctx' = ``send raw`` (fun _ -> $"""["EVENT",{serializedEvent}]""") ctx
        user.SentEvents.Add signedEvent
        return ctx'
    }

let verify f ctx =
    ctx
    |> Async.RunSynchronously
    |> f

let note content ctx =
    Event.createNote content

let noteWithTags content tags ctx =
    Event.create Kind.Text tags content

let replaceableNote content : EventFactory =
    fun ctx -> Event.create Kind.ReplaceableStart [] content

let parameterizedNote content dtag : EventFactory =
    fun ctx -> Event.create Kind.ParameterizableReplaceableStart [("d", [dtag])] content

let ephemeralNote content : EventFactory =
    fun ctx -> Event.create Kind.EphemeralStart [] content

let expirableNote content expirationDate : EventFactory =
    fun ctx -> Event.create Kind.Text [("expiration", [string expirationDate])] content

let deleteNote evnts : EventFactory =
    fun ctx ->
        let allEvents =
            ctx.Users
            |> Seq.map (fun x -> x.Key, x.Value)
            |> Seq.map (fun (_, u) -> u.SentEvents)
            |> Seq.concat
            |> Seq.toList

        let ids =
            allEvents
            |> List.filter (fun e -> List.contains (e.Content) evnts)
            |> List.map (_.Id)

        Event.createDeleteEvent ids "nothing"

let reaction content : EventFactory =
    fun ctx -> Event.create Kind.Reaction [] content

let repost content : EventFactory =
    fun ctx -> Event.create Kind.Repost [] content

[<Literal>]
let Alice = "Alice"
[<Literal>]
let Bob = "Bob"
