namespace Nostra.Client

open System
open System.Threading
open NBitcoin.Secp256k1
open Nostra.Core
open Nostra.Core.Client.Request
open Thoth.Json.Net
open WabiSabi.CredentialRequesting
open WabiSabi.Crypto
open WabiSabi.Crypto.Randomness
open WabiSabi.Crypto.ZeroKnowledge

module ECashMinter =
    open Nostra.Core.Event
    open Nostra.Core.Client
    open Nostra.Core.Client.Request.Filter.FilterUtils
    open Nostra.Client.Common
    open WabiSabi

    [<Literal>]
    let DIRECT_MESSAGE_SUBSCRIPTION = "direct messages to the minter"

    let maxAmount = (2L <<< 32) - 1L 
    let rnd = SecureRandom()
    let issuerKey = CredentialIssuerSecretKey(rnd)
    let issuer = CredentialIssuer(issuerKey, rnd, maxAmount)

    let subscribeToDirectMessagesToMinter (pushToRelay) =
        subscribeTo DIRECT_MESSAGE_SUBSCRIPTION  (DirectMessageToFilter minterPubKey) pushToRelay

    let announceParameters send secret =
        let parameters = issuerKey.ComputeCredentialIssuerParameters()
        let announcementEvent =
            parameters
            |> Encode.issuerParameters
            |> Encode.toCompactString
            |> createNoteEvent
            |> sign secret
            |> CMEvent
            
        send announcementEvent

    let dispatchProtocolHandler relay secret =
        commonProtocolHandler 
            (fun subscriptionId event ->
                match subscriptionId with
                | DIRECT_MESSAGE_SUBSCRIPTION ->
                    let issuanceRequestResult =
                        event
                        |> decryptDirectMessage secret
                        |> Decode.fromString Decode.credentialsRequest
                    match issuanceRequestResult with
                    | Ok credentialsRequest ->
                        Console.WriteLine "Credential request received"
                        let credentialsResponseJson =
                            issuer.HandleRequest credentialsRequest
                            |> Encode.credentialsResponse
                            |> Encode.toCompactString
                        sayTo relay secret event.PubKey credentialsResponseJson
                        Console.WriteLine "New issued credentials sent back to requester"
                    | Error e -> Console.WriteLine "Announcement was invalid"
                | _ -> Console.WriteLine "The relay is crazy or what!?")
