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


type EchoServerFixture() =
    let cts = new CancellationTokenSource()
    let port = EchoServer.startEchoServer cts.Token
    member x.Port = port 
        
    interface IDisposable with
        member this.Dispose() = cts.Cancel()

type ``Client tests``(output:ITestOutputHelper, fixture: EchoServerFixture) =
    let createRelayMessage susbcriptionId event =
        Relay.Response.RMEvent (susbcriptionId, event)
        |> Relay.Response.serialize

    [<Fact>]
    let ``Can receive encrypted messages`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let secret = Key.createNewRandom ()
        let pubkey = Key.getPubKey secret |> XOnlyPubKey 
        let event = Event.createEncryptedDirectMessage pubkey secret "hello" |> Event.sign secret
        do! send (createRelayMessage "sid" (Event.serialize event))
        
        event.Content |> should not' (equal "hello", "The note is not encrypted")
        event.Content |> should haveSubstring "?iv="

        let! msg = receive
        expectEvent msg (fun _ encryptedEvent ->
            let decryptedContent = Event.decryptDirectMessage secret encryptedEvent
            decryptedContent |> should equal "hello")
    } 
            
    [<Fact>]
    let ``Can receive a valid event from relay`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let event = Event.createNoteEvent "Welcome" |> Event.sign (Key.createNewRandom())
        do! send (createRelayMessage "sid" (Event.serialize event))

        let! msg = receive
        expectEvent msg (fun subscriptionId event ->
            subscriptionId |> should equal "sid"
            event.Content |> should equal "Welcome")
    } 

    [<Fact>]
    let ``Can detect invalid (non-authentic) events`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let event = Event.createNoteEvent "Welcome" |> Event.sign (Key.createNewRandom())
        let modifiedEvent = { event with Content = event.Content.Replace("Welcome","Bienvenido") }
        let serializedModifiedEvent = modifiedEvent |> Event.serialize
        do! send (createRelayMessage "sid" serializedModifiedEvent)

        let! msg = receive
        match msg with
        | Ok _ -> failwith "It should have detected the event is invalid"
        | Error error -> error |> should equal "Invalid message received"
    }

    [<Fact>]
    let ``Fails with malformed relay messages`` () = async {
        let! send, receive = Client.createClient fixture.Port
        do! send """["EVENT","14846312514583623"]"""

        let! msg = receive
        match msg with
        | Ok _ -> failwith "It should have detected the event is invalid"
        | Error error -> ()
    }
     
    [<Fact>]
    let ``Fails with malformed relay messages (eventId)`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let message = """["OK","",true,"a message"]""" 
        do! send message

        let! msg = receive
        match msg with
        | Ok _ -> failwith "It should have detected the event is invalid"
        | Error error -> error |> should haveSubstring "EventId is invalid"
    }
    
    [<Fact>]
    let ``Fails with garbage received from relay`` () = async {
        let! send, receive = Client.createClient fixture.Port
        let message = """["Invalid garbage}""" 
        do! send message

        let! msg = receive
        match msg with
        | Ok _ -> failwith "It should have detected the event is invalid"
        | Error error -> error |> should haveSubstring "invalid"
    }
    
    interface IClassFixture<EchoServerFixture> 
