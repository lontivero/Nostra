namespace Nostra.Client

open System
open Nostra.Core
open Nostra.Core.Event
open Nostra.Core.Client
open Nostra.Core.Client.Request
open Nostra.Core.Client.Request.Filter.FilterUtils
open Thoth.Json.Net
open NBitcoin.Secp256k1
open WabiSabi.CredentialRequesting
open WabiSabi.Crypto
open WabiSabi.Crypto.ZeroKnowledge

module Common =

    type PrivateSender = ECPrivKey -> string -> unit

    type Money = int64
    type MinterMessage =
         | MintParametersAnnouncement of CredentialIssuerParameters
         | NewCredentialsMinted of CredentialsResponse

    type PayerMessage =
         | NewPaymentReceived of Credential
         
    type UserMessage =
         | ConsolidateCredential
         | SendTo of Money * XOnlyPubKey
         | Buy of Money
         | GetNullCredentials
         
    type EchashProtocolMessage =
         | MinterMessage of MinterMessage
         | UserMessage of UserMessage
         | PayerMessage of PayerMessage
    
    let secretFromHex =
        Utils.fromHex >> ECPrivKey.Create
        
    let pubKeyFromHex =
        Decode.fromString Decode.xOnlyPubkey

    let sayTo send secret pubKey msg =
        createEncryptedDirectMessage pubKey secret msg
        |> sign secret
        |> CMEvent
        |> send

    let subscribeTo name (filter: ClientFilter) pushToRelay =
        CMSubscribe (name, [toFilter filter])
        |> pushToRelay

    let minterPubKey =
        "'448d8a020ef05ffd37e2e2044b8e2f791b2b6ea38f5cbde4c58f09f992065c81'" 
        |> pubKeyFromHex
        |> function
            | Ok pubkey -> pubkey
            | _ -> failwith "That's not valid xonlypubkey"

    let commonProtocolHandler eventsHandler = function
        | Ok (Response.RMEvent (subscriptionId, event)) ->
            eventsHandler subscriptionId event
        | Ok (Response.RMACK(eventId, success, message)) ->
            Console.ForegroundColor <- ConsoleColor.Green
            let (EventId eid) = eventId 
            Console.WriteLine $"Event: {eid |> Utils.toHex} Success: {success} = {message}"
        | Ok (Response.RMNotice message) ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.WriteLine message
        | Ok (Response.RMEOSE subscriptionId) ->
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.WriteLine $">>> {subscriptionId} Done"
        | Error e ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine (e.ToString())

