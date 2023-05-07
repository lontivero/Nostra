module TestingFramework

open System.Collections.Generic
open System.Threading
open NBitcoin.Secp256k1
open Nostra
open Nostra.Event
open Nostra.Client.Response
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
    Secret : ECPrivKey
    Connection : Connection option
}
        
type TestContext = {
    Users: Dictionary<string, User>
    CurrentUser: string
    Port: int
}

type TestStep = TestContext -> Async<TestContext>
type FilterFactory = TestContext -> string
type EventFactory = TestContext -> UnsignedEvent

let ($) prev next = prev |> Async.bind next

let currentUser ctx = ctx.Users[ctx.CurrentUser]

let ``start relay`` () = async {
    use cts = new CancellationTokenSource()
    let port = Relay.startRelay cts.Token
    let testContext = { CurrentUser = ""; Users = Dictionary<string, User>(); Port = port }
    return testContext
}
    
let ``given`` user : TestStep =
    fun ctx -> 
        let alreadyExists, knownUser = ctx.Users.TryGetValue(user)
        if not alreadyExists then
            let secret = Key.createNewRandom ()
            ctx.Users.Add (user, { Secret = secret; SentEvents = ResizeArray<Event>(); ReceivedEvents = ResizeArray<Event>(); Connection = None })
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
        | None _ ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx            
    }
    
let ``subscribe to`` subscriptionId (filterFactory: FilterFactory): TestStep =
    let rec receiveEvents subscriptionId receiver (events: Event ResizeArray) = async {
        let! response = receiver
        match response with
        | Ok (RMEvent(subscriptionId, event)) ->
            events.Add event
            do! receiveEvents subscriptionId receiver events
        | Ok (RMEOSE subscriptionId) ->
            ()
        | _ -> failwith "Unexpected message"                
    }
        
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            do! conn.Sender $"""["REQ","{subscriptionId}",{filterFactory ctx}]"""
            do! receiveEvents subscriptionId conn.Receiver user.ReceivedEvents
        | None _ ->
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
        let pubkey = user.Secret |> Key.getPubKey |> fun x -> x.ToBytes() |> Utils.toHex
        $"""{{"authors": ["{pubkey}"]}}"""    
    
let ``send event`` eventFactory : TestStep =
    fun ctx -> async {
        let user = currentUser ctx
        match user.Connection with
        | Some conn ->
            let signedEvent = eventFactory ctx |> Event.sign user.Secret
            let serializedEvent = Event.serialize signedEvent
            do! conn.Sender $"""["EVENT",{serializedEvent}]"""                 
            let! response = conn.Receiver
            match response with
            | Ok (RMACK(_, true, _)) -> should equal true true
            | Ok (RMACK(_, false, reason)) -> failwith reason
            | _ -> failwith "error"
            user.SentEvents.Add signedEvent
        | None ->
            failwith $"User '{ctx.CurrentUser}' is not connected."
        return ctx
    }

let verify f ctx =
    ctx
    |> Async.RunSynchronously
    |> f
    
let note content ctx =
    Event.createNoteEvent content
    
let replaceableNote content : EventFactory =
    fun ctx -> Event.createEvent Kind.ReplaceableStart [] content

let parameterizedNote content dtag : EventFactory =
    fun ctx -> Event.createEvent Kind.ParameterizableReplaceableStart [("d", [dtag])] content

let ephemeralNote content : EventFactory =
    fun ctx -> Event.createEvent Kind.EphemeralStart [] content
        
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
            |> List.map (fun e -> e.Id)
            
        Event.createDeleteEvent ids "nothing" 
    
[<Literal>]
let Alice = "Alice"
[<Literal>]
let Bob = "Bob"
