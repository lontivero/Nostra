module RelayTests

open System
open System.Threading
open Nostra
open Nostra.Client.Response
open Nostra.Tests
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions

let expectEvent msg asserts =
    match msg with
    | Ok (RMEvent(subscriptionId, event)) ->
        asserts subscriptionId event
    | Ok _ -> failwith "An relay message containing an event was expected but something different came!"
    | Error error -> error |> should equal "Invalid message received"

let createRelayMessage susbcriptionId event =
    Relay.Response.RMEvent (susbcriptionId, event)
    |> Relay.Response.serialize

type RelayFixture() =
    let cts = new CancellationTokenSource()
    let port = Relay.startRelay cts.Token
    member x.Port = port 

    interface IDisposable with
        member this.Dispose() = cts.Cancel()

type ``Relay Accept Events``(output:ITestOutputHelper, fixture:RelayFixture) =

    [<Fact>]
    let ``Can receive a valid event from relay`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let event = Event.createNoteEvent "Welcome" |> Event.sign (Key.createNewRandom())
        do! send $"""["EVENT",{Event.serialize event}]"""

        let! msg = receive
        match msg with
        | Ok (RMACK(_, success, _)) -> should equal true success
        | _ -> failwith "error"
    } 

    [<Fact>]
    let ``Can detect invalid (non-authentic) events`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let event = Event.createNoteEvent "Welcome" |> Event.sign (Key.createNewRandom())
        let modifiedEvent = { event with Content = event.Content.Replace("Welcome","Bienvenido") }
        let serializedModifiedEvent = modifiedEvent |> Event.serialize
        do! send ("""["EVENT",""" + serializedModifiedEvent + "]")

        let! msg = receive
        match msg with
        | Ok (RMACK(_, true, _)) -> failwith "It should have detected the event is invalid"
        | Ok (RMACK(_, false, error)) -> should equal "invalid: the signature is incorrect" error 
        | Ok _ -> failwith "error" 
        | Error error -> failwith error
    }

    [<Fact>]
    let ``Fails with malformed relay messages`` () = async {
        let! send, receive = Client.createClient fixture.Port
        do! send """["EVENT","14846312514583623"]"""

        let! msg = receive
        match msg with
        | Ok (RMNotice message) -> should equal "Invalid message received" message  
        | Ok _ -> failwith "error" 
        | Error error -> failwith error
    }
     
    [<Fact>]
    let ``Fails with malformed relay messages (eventId)`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let message = """["OK","",true,"a message]""" 
        do! send message

        let! msg = receive
        match msg with
        | Ok (RMNotice message) -> should equal "Invalid message received" message  
        | Ok _ -> failwith "error" 
        | Error error -> failwith error
    }
    
    interface IClassFixture<RelayFixture> 


type ``Relay Accept Queries``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can receive encrypted messages`` () = async {
        use cts = new CancellationTokenSource()
        let port = Relay.startRelay cts.Token

        let! send, receive = Client.createClient port
        do! send """["REQ","all",{"kinds":[1]}]"""
        let! notice = receive

        let secret = Key.createNewRandom ()
        let event = Event.createNoteEvent "hello" |> Event.sign secret
        let serializedEvent = Event.serialize event
        do! send $"""["EVENT",{serializedEvent}]"""       
        let! ok = receive
        
        let! eventMsg = receive
        
        expectEvent eventMsg (fun sid receivedEvent ->
            should equal "all" sid
            should equal serializedEvent (Event.serialize receivedEvent))
    }

    [<Fact>]
    let ``Can receive immediate event subscription`` () = async {
        use cts = new CancellationTokenSource()
        let port = Relay.startRelay cts.Token

        let! send, receive = Client.createClient port
        do! send """["REQ","all",{"kinds":[1]}]"""
        let! notice = receive

        let! sendOther, receiveOther = Client.createClient port
        
        let secret = Key.createNewRandom ()
        let event = Event.createNoteEvent "hello" |> Event.sign secret
        let serializedEvent = Event.serialize event
        do! sendOther $"""["EVENT",{serializedEvent}]"""       
        let! ok = receiveOther
        
        let! eventMsg = receive
        
        expectEvent eventMsg (fun sid receivedEvent ->
            should equal "all" sid
            should equal serializedEvent (Event.serialize receivedEvent))       
    }

    [<Fact>]
    let ``Can receive stored event subscription`` () = async {
        use cts = new CancellationTokenSource()
        let port = Relay.startRelay cts.Token
        
        let! sendOther, receiveOther = Client.createClient port
        
        let secret = Key.createNewRandom ()
        let event = Event.createNoteEvent "hello" |> Event.sign secret
        let serializedEvent = Event.serialize event
        do! sendOther $"""["EVENT",{serializedEvent}]"""       
        let! ok = receiveOther

        let! send, receive = Client.createClient port
        do! send """["REQ","all",{"kinds":[1]}]"""
        let! eventMsg = receive
                       
        expectEvent eventMsg (fun sid receivedEvent ->
            should equal "all" sid
            should equal serializedEvent (Event.serialize receivedEvent))
        
        let! eose = receive
        match eose with
        | Ok (RMEOSE "all") -> ()
        | _ -> failwith "error"
    }
    
