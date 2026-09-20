module Nip40Tests

open Xunit
open Nostra
open TestingFramework

type ``NIP-40 Expiration``() =

    [<Fact>]
    let ``Unparsable expiration tag is ignored`` () =
        startWithAlice ()
        $ ``publish event`` (note "Test" 1722337838L |> tags [Tag("expiration", ["blah"])])
        $ ``receive messages`` [
           ExpectOk true
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Already expired event is rejected`` () =
        startWithAlice ()
        $ ``publish event`` (expirableEvent "Test" 1 1722337838L 1231002905L)
        $ ``receive messages`` [
           ExpectOk false
        ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Non-expired event is accepted and returned`` () =
        startWithAliceAndBob ()
        $ ``given`` Alice AlicePrivateKey
        $ ``publish event`` (expirableEvent "Valid" 1 1722337838L 9999999999L)
        $ ``receive messages`` [ExpectOk true]
        $ ``given`` Bob BobPrivateKey
        $ ``subscribe`` "abcd" [kindFilter [Kind.Text]]
        $ ``receive messages`` [
            ExpectEvent (withContent "abcd" "Valid")
            ExpectEose "abcd"
        ]
        |> verify (fun _ -> ())
