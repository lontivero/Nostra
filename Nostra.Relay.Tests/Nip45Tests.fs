module Nip45Tests

open Xunit
open Nostra
open TestingFramework

type ``NIP-45 Counting``() =

    [<Fact>]
    let ``Can count events`` () =
        startWithAlice ()
        $ ``publish events`` [
            note "Hello 1" 1722337838L
            note "Hello 2" 1722337839L
            note "Hello 3" 1722337840L
        ]
        $ ``count`` "count1" [kindFilter [Kind.Text]]
        $ ``receive messages`` [ExpectCount("count1", 3)]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Can count events with kind filter`` () =
        startWithAlice ()
        $ ``publish events`` [
            note "Hello" 1722337838L
            event |> content "Reaction" |> kind Kind.Reaction |> created 1722337839L
            note "World" 1722337840L
        ]
        $ ``count`` "notes-only" [kindFilter [Kind.Text]]
        $ ``receive messages`` [ExpectCount("notes-only", 2)]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Can count zero events`` () =
        startWithAlice ()
        $ ``count`` "empty" [kindFilter [Kind.Text]]
        $ ``receive messages`` [ExpectCount("empty", 0)]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Can count by author`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish events`` [
            note "Alice 1" 1722337838L
            note "Alice 2" 1722337839L
        ]
        $ ``given`` Bob BobPrivateKey
        $ ``publish event`` (note "Bob 1" 1722337840L)
        $ ``receive messages`` [ExpectOk true]
        $ ``count`` "alice-count" [authorFilter [AlicePublicKey]]
        $ ``receive messages`` [ExpectCount("alice-count", 2)]
        |> verify (fun _ -> ())
