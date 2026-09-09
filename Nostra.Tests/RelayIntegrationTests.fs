module RelayIntegrationTests

open System.IO
open Nostra
open Nostra.Relay.Configuration
open Nostra.Relay.InfoDocument
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
            let contents = bob.ReceivedEvents |> Seq.map _.Content
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

    [<Fact>]
    let ``Can receive limited results subscription from limitation`` () =
        ``start relay with limits`` { Limitation.defaults with MaxLimit = 1 }
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "hello 1")
        $ ``send event`` (note "hello 2")
        $ ``send event`` (note "hello 3")
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let events = test.Users[Alice].ReceivedEvents
            should equal 1 events.Count
            should equal "hello 3" events[0].Content
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

type ``Relay Nip11``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can send respecting limitations`` () =
        ``start relay with limits`` { Limitation.defaults with MaxContentLength = 6; MaxEventTags = 2; MaxMessageLength = 400; MaxSubidLength = 3  }
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "hello")
        $ ``send event`` (note "hello again!") // must fail because MaxContentLength
        $ ``send event`` (noteWithTags "hola" [Tag.create "p" ["p1"]; Tag.create "q" ["q1"] ])
        $ ``send event`` (noteWithTags "hola" [Tag.create "p" ["p1"]; Tag.create "q" ["q1"]; Tag.create "r" ["r1"] ]) // must fail because MaxEventTags
        $ ``send raw``   (fun _ -> $"""["REQ","sub", {{ {System.String(' ', 500)} }}]""" ) // must fail because MaxMessageLngth
        $ ``subscribe to`` "larger-than-3" (latest 1) // must fail because MaxSubidLength
        |>  verify (fun test ->
            let user = currentUser test
            should equal 4 user.Errors.Count
            should equal "invalid: content too large" user.Errors[0]
            should equal "invalid: too many tags" user.Errors[1]
            should equal "message too large" user.Errors[2]
            should equal "too large subscription id" user.Errors[3]
            )

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
        $ ``send event`` (expirableNote "some text" (Utils.toUnixTime (System.DateTime.UtcNow.AddSeconds 60)))
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
        $ ``send event`` (expirableNote "some text" (Utils.toUnixTime (System.DateTime.UtcNow.AddSeconds -2)))
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |>  verify (fun test ->
            let user = currentUser test
            should equal 0 user.ReceivedEvents.Count)

type ``Relay Plugin``(output:ITestOutputHelper) =

    let createPluginScript (blockedKind: int) =
        let scriptPath = Path.GetTempFileName()
        let scriptContent = $"""
read line
id=$(echo "$line" | sed -n 's/.*"id":"\([^"]*\)".*/\1/p')
kind=$(echo "$line" | sed -n 's/.*"kind":\([0-9]*\).*/\1/p')
if [ "$kind" = "{blockedKind}" ]; then
    echo '{{"id":"'"$id"'","action":"reject","msg":"kind {blockedKind} is not allowed"}}'
else
    echo '{{"id":"'"$id"'","action":"accept"}}'
fi
"""
        File.WriteAllText(scriptPath, scriptContent)
        scriptPath

    [<Fact>]
    let ``Plugin can block events by kind`` () =
        let scriptPath = createPluginScript 7 // Block Kind.Reaction
        let writePolicy = { Plugin = Some $"bash {scriptPath}"; TimeoutSeconds = 5 }

        ``start relay with write policy`` writePolicy
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "hello")           // Kind.Text = 1, should be accepted
        $ ``send event`` (reaction "+1")          // Kind.Reaction = 7, should be blocked
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |> verify (fun test ->
            let alice = test.Users[Alice]
            let bob = test.Users[Bob]

            // Alice should have one error (blocked reaction)
            should equal 1 alice.Errors.Count
            should haveSubstring "event cannot be accepted" alice.Errors[0]

            // Bob should only receive the note, not the reaction
            should equal 1 bob.ReceivedEvents.Count
            should equal Kind.Text bob.ReceivedEvents[0].Kind
            should equal "hello" bob.ReceivedEvents[0].Content)

    [<Fact>]
    let ``Plugin allows events when kind is not blocked`` () =
        let scriptPath = createPluginScript 999 // Block a kind we won't use
        let writePolicy = { Plugin = Some $"bash {scriptPath}"; TimeoutSeconds = 5 }

        ``start relay with write policy`` writePolicy
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "first note")
        $ ``send event`` (note "second note")
        $ ``send event`` (reaction "like")
        $ given Bob
        $ ``connect to relay``
        $ ``subscribe to all events``
        |> verify (fun test ->
            let alice = test.Users[Alice]
            let bob = test.Users[Bob]

            // No errors - all events should be accepted
            should equal 0 alice.Errors.Count

            // Bob should receive all 3 events
            should equal 3 bob.ReceivedEvents.Count)

type ``Relay Nip45``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can count events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "hello 1")
        $ ``send event`` (note "hello 2")
        $ ``send event`` (note "hello 3")
        $ ``count`` "count1" notes
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedCounts.Count
            let (subscriptionId, count) = user.ReceivedCounts[0]
            should equal "count1" subscriptionId
            should equal 3 count)

    [<Fact>]
    let ``Can count events with filter`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay``
        $ ``send event`` (note "hello")
        $ ``send event`` (reaction "+1")
        $ ``send event`` (note "world")
        $ ``count`` "notes-only" notes
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedCounts.Count
            let (_, count) = user.ReceivedCounts[0]
            should equal 2 count)

    [<Fact>]
    let ``Can count zero events`` () =
        ``start relay`` ()
        $ given Alice
        $ ``connect to relay``
        $ ``count`` "empty" notes
        |>  verify (fun test ->
            let user = currentUser test
            should equal 1 user.ReceivedCounts.Count
            let (_, count) = user.ReceivedCounts[0]
            should equal 0 count)

type ``Relay WebSocket Fragmentation``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Can receive fragmented WebSocket message`` () = async {
        use cts = new System.Threading.CancellationTokenSource()
        let port = Nostra.Tests.Relay.startRelay cts.Token

        let! _, sendFragmented, receive = Nostra.Tests.Client.createClientWithFragmentedSend port

        // Create a valid event
        let event = Event.createNote "Hello from fragmented message" |> Event.sign (SecretKey.createNewRandom())
        let serializedEvent = Event.serialize event
        let fullMessage = $"""["EVENT",{serializedEvent}]"""

        // Split the message into 3 fragments
        let partSize = fullMessage.Length / 3
        let frag1 = fullMessage.Substring(0, partSize)
        let frag2 = fullMessage.Substring(partSize, partSize)
        let frag3 = fullMessage.Substring(partSize * 2)

        // Send as fragmented message
        do! sendFragmented [frag1; frag2; frag3]

        // Should receive OK response
        let! msg = receive
        match msg with
        | Ok (Nostra.Client.Response.RMACK(_, success, _)) -> should equal true success
        | Ok other -> failwith $"Expected RMACK but got {other}"
        | Result.Error e -> failwith $"Error: {e}"
    }

    [<Fact>]
    let ``Can receive two-fragment WebSocket message`` () = async {
        use cts = new System.Threading.CancellationTokenSource()
        let port = Nostra.Tests.Relay.startRelay cts.Token

        let! _, sendFragmented, receive = Nostra.Tests.Client.createClientWithFragmentedSend port

        // Create a valid event
        let event = Event.createNote "Two fragments" |> Event.sign (SecretKey.createNewRandom())
        let serializedEvent = Event.serialize event
        let fullMessage = $"""["EVENT",{serializedEvent}]"""

        // Split into 2 fragments
        let midpoint = fullMessage.Length / 2
        let frag1 = fullMessage.Substring(0, midpoint)
        let frag2 = fullMessage.Substring(midpoint)

        do! sendFragmented [frag1; frag2]

        let! msg = receive
        match msg with
        | Ok (Nostra.Client.Response.RMACK(_, success, _)) -> should equal true success
        | Ok other -> failwith $"Expected RMACK but got {other}"
        | Result.Error e -> failwith $"Error: {e}"
    }
