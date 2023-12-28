module RelayIntegrationTests

open Microsoft.FSharp.Control
open Nostra
open Nostra.Event
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions
open TestingFramework

type ``Relay Accept Queries``(output:ITestOutputHelper) =
    
    [<Fact>]
    let ``Can receive immediate event subscription`` () =
        ``start relay`` ()
        $ given Alice 
        $ ``connect to relay`` 
        $ ``subscribe to all events``
        $ given Bob
        $ ``connect to relay``
        $ ``send event`` (note "hello")
        $ given Alice
        $ ``wait for event`` "All"
        |>  verify (fun test ->
            let alice = test.Users[Alice]
            let bob = test.Users[Bob]
            should equal
                (Event.serialize bob.SentEvents[0])
                (Event.serialize alice.ReceivedEvents[0]))
       
    [<Fact>]
    let ``Can receive stored event subscription`` () =
        ``start relay`` ()
        $ given Alice 
        $ ``connect to relay`` 
        $ ``send event`` (note "hello")
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let bob = test.Users[Bob]
            let contents = bob.ReceivedEvents |> Seq.map (fun x -> x.Content)
            should contain "hello" contents)

    [<Fact>]
    let ``Can receive limited results subscription`` () =
        ``start relay`` ()
        $ given Alice 
        $ ``connect to relay`` 
        $ ``send event`` (note "hello 1")
        $ ``send event`` (note "hello 2")
        $ ``send event`` (note "hello 3")
        $ ``subscribe to`` "sid" (latest 2)
        |>  verify (fun test ->
            let events = test.Users[Alice].ReceivedEvents            
            should equal 2 events.Count
            should equal "hello 2" events[0].Content
            should equal "hello 3" events[1].Content
            )
                    
type ``Relay Nip09``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can delete events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay`` 
        $ ``send event`` (note "hello 1")
        $ ``send event`` (note "hello 2")
        $ ``send event`` (deleteNote ["hello 1"; "hello 2"])
        $ given Bob
        $ ``connect to relay`` 
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedEvents.Count
            should equal Kind.Delete user.ReceivedEvents[0].Kind)
    
    
type ``Relay Nip16``(output:ITestOutputHelper) =
    
    [<Fact>]
    let ``Can Replace replaceable events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay`` 
        $ ``send event`` (replaceableNote "replaceable")
        $ ``send event`` (replaceableNote "replacement")
        $ given Bob
        $ ``connect to relay`` 
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedEvents.Count
            should equal "replacement" user.ReceivedEvents[0].Content)

    [<Fact>]
    let ``Can send ephemeral events`` () =
        ``start relay`` ()
        $ ``given`` Bob   $ ``connect to relay`` 
        $ ``given`` Alice $ ``connect to relay`` 
        $ ``subscribe to`` "Bob's events" (eventsFrom Bob)
        $ ``given`` Bob   $ ``send event`` (ephemeralNote "hi there!")
        $ ``given`` Alice $ ``wait for event`` "Bob's events" 
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedEvents.Count
            should equal "hi there!" user.ReceivedEvents[0].Content)

type ``Relay Nip33``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can Replace Dtag events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay`` 
        $ ``send event`` (parameterizedNote "replaceable" "dtag")
        $ ``send event`` (parameterizedNote "replacement" "dtag")
        $ given Bob
        $ ``connect to relay`` 
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedEvents.Count
            should equal "replacement" user.ReceivedEvents[0].Content)

type ``Relay Nip40``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can return non-expired events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (expirableNote "some text" (Utils.toUnixTime (System.DateTime.Now.AddSeconds 2)))
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedEvents.Count
            should equal "some text" user.ReceivedEvents[0].Content)

    [<Fact>]
    let ``Can not return expired events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (expirableNote "some text" (Utils.toUnixTime (System.DateTime.Now.AddSeconds -2)))
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 0 user.ReceivedEvents.Count)
