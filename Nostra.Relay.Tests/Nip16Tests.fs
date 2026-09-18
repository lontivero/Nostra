module Nip16Tests

open Xunit
open Nostra
open TestingFramework

type ``NIP-16 Replaceable Events``() =

    [<Fact>]
    let ``Only newest replaceable event is returned`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish events`` [
            event |> content "First" |> kind Kind.Metadata |> created 1722337838L
            event |> content "Second" |> kind Kind.Metadata |> created 1722337840L
            event |> content "Third" |> kind Kind.Metadata |> created 1722337830L
        ]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Second")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())

type ``NIP-16 Ephemeral Events``() =

    [<Fact>]
    let ``Ephemeral events are not stored`` () =
        startWithAliceAndBob ()
        $ ``given`` Bob BobPrivateKey
        $ ``publish event`` (event |> content "Ephemeral" |> kind (enum 20000) |> created 1722337838L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [ExpectEose "abcd"]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Ephemeral events are broadcast live`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``subscribe`` "abcd" [authorFilter [BobPublicKey]]
        $ ``receive messages`` [ExpectEose "abcd"]
        $ ``given`` Bob BobPrivateKey
        $ ``publish event`` (event |> content "Ephemeral" |> kind (enum 20000) |> created 1722337838L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Alice AlicePrivateKey
        $ ``receive messages`` [ExpectEvent (withContent "abcd" "Ephemeral")]
        |> verify (fun _ -> ())
