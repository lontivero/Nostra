module Nip11Tests

open System.Net.Http
open System.Text.Json
open Xunit
open FsUnit.Xunit
open Nostra
open Nostra.Relay.InfoDocument
open TestingFramework

type ``NIP-11 Relay Information``() =

    [<Fact>]
    let ``Relay returns info document with Accept header`` () =
        ``start relay`` ()
        |> verify (fun ctx ->
            use client = new HttpClient()
            let request = new HttpRequestMessage(HttpMethod.Get, $"http://127.0.0.1:{ctx.Port}/")
            request.Headers.Add("Accept", "application/nostr+json")
            let response = client.SendAsync(request) |> Async.AwaitTask |> Async.RunSynchronously

            response.StatusCode |> should equal System.Net.HttpStatusCode.OK

            response.Headers.Contains("Access-Control-Allow-Origin") |> should equal true

            let content = response.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously
            let doc = JsonDocument.Parse(content)
            let root = doc.RootElement

            root.TryGetProperty("name", ref Unchecked.defaultof<JsonElement>) |> should equal true
            root.TryGetProperty("supported_nips", ref Unchecked.defaultof<JsonElement>) |> should equal true
        )

type ``NIP-11 Created At Limits``() =

    [<Fact>]
    let ``Event too old is rejected when lower limit is set`` () =
        let limitations = { Limitation.defaults with CreatedAtLowerLimit = Some 3600 }
        ``start relay with limitations`` limitations
        $ ``given`` Alice AlicePrivateKey
        $ ``connect to relay``
        $ ``publish event`` (note "Old event" 1231002905L)
        $ ``receive messages`` [ ExpectOk false ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Event in future is rejected when upper limit is set`` () =
        let limitations = { Limitation.defaults with CreatedAtUpperLimit = Some 900 }
        let futureTimestamp = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds() + 3600L
        ``start relay with limitations`` limitations
        $ ``given`` Alice AlicePrivateKey
        $ ``connect to relay``
        $ ``publish event`` (note "Future event" futureTimestamp)
        $ ``receive messages`` [ ExpectOk false ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``Recent event is accepted when lower limit is set`` () =
        let limitations = { Limitation.defaults with CreatedAtLowerLimit = Some 31536000 }
        let nowTimestamp = System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()
        ``start relay with limitations`` limitations
        $ ``given`` Alice AlicePrivateKey
        $ ``connect to relay``
        $ ``publish event`` (note "Recent event" nowTimestamp)
        $ ``receive messages`` [ ExpectOk true ]
        |> verify (fun _ -> ())

    [<Fact>]
    let ``No limits by default`` () =
        startWithAlice ()
        $ ``publish event`` (note "Old event" 1231002905L)
        $ ``receive messages`` [ ExpectOk true ]
        |> verify (fun _ -> ())
