module Nip09Tests

open Nostra.Client.Request
open Xunit
open Nostra
open TestingFramework

type ``NIP-09 Event Deletion``() =

    [<Fact>]
    let ``Deletion event is accepted`` () =
        startWithAlice ()
        $ ``publish event`` (note "Hello" 1722337838L)
        $ ``publish event`` (deleteEvent ["8ed8cc390eaf6db9e0ae8f3bf720a80d81ae49f95f953a9a4e26a72dc7f4a2c5"] 1722337850L)
        $ ``receive messages`` [
            ExpectOk true
            ExpectOk true
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Deletion event for unknown events is accepted`` () =
        startWithAlice ()
        $ ``publish event`` (deleteEvent ["ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"] 1722337845L)
        $ ``receive messages`` [ ExpectOk true ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Deleted event is not returned in subscription`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Hello" 1722337838L
            note "Later" 1722337848L
            deleteEvent ["8ed8cc390eaf6db9e0ae8f3bf720a80d81ae49f95f953a9a4e26a72dc7f4a2c5"] 1722337845L
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``subscribe`` "abcd" [authorFilter [AlicePublicKey]]
        $ ``receive messages`` [
            ExpectEvent (matching "abcd" (fun e -> e.Kind = Kind.Delete))
            ExpectEvent (withContent "abcd" "Later")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Deleting a deletion has no effect`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Hello" 1722337838L
            note "Later" 1722337848L
            deleteEvent ["8ed8cc390eaf6db9e0ae8f3bf720a80d81ae49f95f953a9a4e26a72dc7f4a2c5"] 1722337845L
            deleteEvent ["367ca4fcb31777b20fffc7057ca10e3f251322022b57fc4c123ecbf423f3b529"] 1722337850L
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``subscribe`` "abcd" [authorFilter [AlicePublicKey]]
        $ ``receive messages`` [
            ExpectEvent (matching "abcd" (fun e -> e.Kind = Kind.Delete))
            ExpectEvent (withContent "abcd" "Later")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact(Skip = "Nostra accepts resubmission of deleted events - NIP-09 compliance needed")>]
    let ``Resubmission of deleted event is rejected`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Hello" 1722337838L
            deleteEvent ["8ed8cc390eaf6db9e0ae8f3bf720a80d81ae49f95f953a9a4e26a72dc7f4a2c5"] 1722337845L
        ]
        $ ``publish event`` (note "Hello" 1722337838L)
        $ ``receive messages`` [ExpectOk false]
        $ ``given`` Bob BobPrivateKey
        $ ``subscribe`` "abcd" [Filter.all |> Filter.notes |> Filter.authors [AlicePublicKey]]
        $ ``receive messages`` [ExpectEose "abcd"]
        |> verify (fun _ -> ())

    [<Fact(Skip = "Nostra doesn't implement deletion via a-tags")>]
    let ``Deletion with a-tag removes addressable event`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            event |> content "Note A" |> kind (enum 30000) |> created 1722337835L |> tags [Tag("d", ["a"])]
            event |> content "Note B" |> kind (enum 30000) |> created 1722337840L |> tags [Tag("d", ["b"])]
        ]
        $ ``publish event`` (deleteAddressable [$"30000:{BobPublicKey}:a"] 1722337845L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [
            ExpectEvent (matching "abcd" (fun e -> e.Kind = Kind.Delete))
            ExpectEvent (withContent "abcd" "Note B")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Cannot delete someone else's events`` () =
        startWithAliceBobAndCharlie ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Alice's event" 1722337838L
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            deleteEvent ["fd701a4504d688d72526323b24ebd63478171e376910ada8e36e1b29eea463e7"] 1722337845L
        ]
        $ ``given`` Charlie CharliePrivateKey
        $ ``subscribe`` "abcd" [Filter.all |> Filter.notes |> Filter.authors [AlicePublicKey]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Alice's event")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())
