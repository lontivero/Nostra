module Nip01Tests

open Nostra.Client.Request
open Nostra.Utils
open Xunit
open FsUnit.Xunit
open Nostra
open TestingFramework

type ``NIP-01 Basic Protocol``() =

    [<Fact>]
    let ``Newly subscribed client receives matching events, EOSE and future events`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            note "Hello 1" 1722337838L
            event |> content "Hello MD" |> kind (enum 30023) |> created 1722337839L
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [kindFilter [Kind.Text]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Hello 1")
            ExpectEose "abcd"
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [note "Hello 2" 1722337840L]
        $ ``given`` Alice AlicePrivateKey
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Hello 2")
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Closed subscriptions should no longer receive events`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [kindFilter [Kind.Text]]
        $ ``receive messages`` [ExpectEose "abcd"]
        $ ``subscribe`` "efgh" [kindFilter [Kind.Text]]
        $ ``receive messages`` [ExpectEose "efgh"]
        $ ``close subscription`` "abcd"
        $ ``given`` Bob BobPrivateKey
        $ ``publish event`` (note "Hello" 1722337838L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``receive messages`` [ExpectEvent (withContent "efgh" "Hello")]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Resubscribing restarts subscription`` () =
        startWithAliceBobAndCharlie ()
        $ ``given`` Charlie CharliePrivateKey
        $ ``publish event`` (note "Hello" 1722337836L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [ExpectEose "abcd"]
        $ ``subscribe`` "abcd" [authorFilter [CharliePublicKey]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Hello")
            ExpectEose "abcd"
        ]
        $ ``given`` Charlie CharliePrivateKey
        $ ``publish event`` (note "Hello again" 1722337837L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``receive messages`` [ExpectEvent (withContent "abcd" "Hello again")]
        $ ``given`` Bob BobPrivateKey
        $ ``publish event`` (note "Bob says hi" 1722337838L)
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Zero limit returns EOSE and future events only`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [note "Before" 1722337838L]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [Filter.all |> Filter.authors [BobPublicKey] |> Filter.limit 0]
        $ ``receive messages`` [ExpectEose "abcd"]
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [note "After" 1722337850L]
        $ ``given`` Alice AlicePrivateKey
        $ ``receive messages`` [ExpectEvent (withContent "abcd" "After")]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Filter with multiple kinds`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            note "Note" 1722337838L
            event |> content "Metadata" |> kind Kind.Metadata |> created 1722337839L
            event |> content "Reaction" |> kind Kind.Reaction |> created 1722337840L
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [kindFilter [Kind.Text; Kind.Reaction]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Note")
            ExpectEvent (withContent "abcd" "Reaction")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Filter with limit`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            note "First" 1722337830L
            note "Second" 1722337840L
            note "Third" 1722337850L
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [Filter.all |> Filter.authors [BobPublicKey] |> Filter.notes |> Filter.limit 2]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Second")
            ExpectEvent (withContent "abcd" "Third")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Relay can handle complex filters with multiple filter objects`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            note "Hello 1" 1722337838L
            event |> content "" |> kind Kind.Metadata |> created 1722337850L
            event |> content "Hello MD" |> kind (enum 30023) |> created 1722337839L
            note "Tagged" 1722337839L |> tags [Tag("q", ["q1"]); Tag("q", ["q2"]); Tag("r", ["r1"])]
            note "Tagged2" 1722337839L |> tags [Tag("q", ["q1"]); Tag("q", ["q3"])]
        ]
        $ ``given`` Charlie CharliePrivateKey
        $ ``connect to relay``
        $ ``publish events`` [
            event |> content "Hello" |> kind (enum 30023) |> created 1722337835L
            note "Hello" 1722337836L
            note "Hello again" 1722337837L
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [
            Filter.all |> Filter.limit 1
            Filter.all
            |> Filter.authors [CharliePublicKey]
            |> Filter.kinds [Kind.Text; Kind.Recommend]
            |> Filter.since (fromUnixTime 1722337830L)
            |> Filter.until (fromUnixTime 1722337836L)
            Filter.all
            |> Filter.events [EventId.parse "a6d166e834e78827af0770f31f15b13a772f281ad880f43ce12c24d4e3d0e346" |> Result.requiresOk]
            Filter.all
            |> Filter.kinds [enum<Kind> 30023]
            Filter.all
            |> Filter.kinds [Kind.Text]
            |> Filter.authors [BobPublicKey]
            |> Filter.tags [Tag("q", ["q4"; "q1"]); Tag("r", ["r1"])]
        ]
        $ ``receive messages`` [
            ExpectEvent (onSub "abcd")
            ExpectEvent (onSub "abcd")
            ExpectEvent (onSub "abcd")
            ExpectEvent (onSub "abcd")
            ExpectEvent (onSub "abcd")
            ExpectEose "abcd"
        ]
        |> verify (fun ctx ->
            let alice = ctx.Users[Alice]
            let eventCount = alice.ReceivedMessages |> Seq.filter (function MsgEvent _ -> true | _ -> false) |> Seq.length
            eventCount |> should equal 5
        )