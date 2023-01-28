namespace Nostra.Client

open System
open System.Threading
open Nostra.Client.Common
open Nostra.Core
open Nostra.Core.Client.Request
open Thoth.Json.Net
open WabiSabi.CredentialRequesting
open WabiSabi.Crypto
open WabiSabi.Crypto.Randomness
open WabiSabi.Crypto.ZeroKnowledge

module ECashClient =
    open Nostra.Core.Event
    open Nostra.Core.Client
    open Nostra.Core.Client.Request.Filter.FilterUtils
    open Nostra.Client.Common
    open WabiSabi
    
    [<Literal>]
    let MINT_ANNOUNCEMENTS_SUBSCRIPTION = "minter's announcements"
    [<Literal>]
    let MINT_DIRECT_MESSAGE_SUBSCRIPTION = "minter's direct messages"
    [<Literal>]
    let PAYMENT_DIRECT_MESSAGE_SUBSCRIPTION = "new payments"
    
    let subscribeToAnnouncements (pushToRelay) =
        let filter = Custom { singleton with Kinds = [Kind.Text]; Authors = [minterPubKey]; Limit = Some 1 }
        subscribeTo MINT_ANNOUNCEMENTS_SUBSCRIPTION filter pushToRelay

    let subscribeToDirectMessages pushToRelay me =
        subscribeTo MINT_DIRECT_MESSAGE_SUBSCRIPTION  (DirectMessageFromFilter (minterPubKey, me) ) pushToRelay

    let subscribeToPayments pushToRelay me =
        let filter = Custom { singleton with Kinds = [Kind.Encrypted]; PubKeys = [me]; Since = Some DateTime.UtcNow; Limit = Some 0 }
        subscribeTo PAYMENT_DIRECT_MESSAGE_SUBSCRIPTION filter pushToRelay
    
    type ReadyState = {
        Credentials: Credential list
        Client: WabiSabiClient
    }

    type WaitingMoney = {
        Credentials: Credential list
        Client: WabiSabiClient
        ValidationData: CredentialsResponseValidation
    }

    type WaitingMoneyForPayment = {
        Credentials: Credential list
        Client: WabiSabiClient
        ValidationData: CredentialsResponseValidation
        Destination: XOnlyPubKey
    }
    
    type State =
        | Uninitialized
        | Ready of ReadyState
        | WaitingMoney of WaitingMoney
        | WaitingMoneyForPayment of WaitingMoneyForPayment

    let protocolHandlerLoop secret relay =
        let sendTo dst =
            sayTo relay secret dst

        let sendToMinter =
            sendTo minterPubKey

        let getUsableCredentials (credentials: Credential list) =
            credentials
            |> List.sortByDescending(fun x -> x.Value)
            |> List.mapi (fun i credential -> (i, credential))
            |> List.partition (fun (i, credential) -> i < ProtocolConstants.CredentialNumber)
            |> fun (toUse, rest) ->
                (List.map snd toUse, List.map snd rest) 

        let worker =
            MailboxProcessor<EchashProtocolMessage>.Start (fun inbox ->
                let rec loop (state: State) = async { 
                    let! msg = inbox.Receive()
                    let newState =
                        match msg with
                        | MinterMessage (MintParametersAnnouncement parameters) ->
                            match state with
                            | Uninitialized ->
                                let maxAmount = (2L <<< 32) - 1L
                                Ready {
                                    Client = WabiSabiClient(parameters, SecureRandom(), maxAmount)
                                    Credentials = []
                                }
                            | _ -> state
                        | MinterMessage (NewCredentialsMinted credentialResponse) ->
                            match state with
                            | WaitingMoney waitingMoneyState ->
                                let newCredentials =
                                    waitingMoneyState.Client.HandleResponse (credentialResponse, waitingMoneyState.ValidationData)
                                    |> Array.ofSeq
                                let c1, c2 = newCredentials[0], newCredentials[1]
                                let receivedAmount = c1.Value + c2.Value
                                let curBalance = waitingMoneyState.Credentials |>  Seq.sumBy (fun x -> x.Value)
                                Console.ForegroundColor <- ConsoleColor.Green
                                Console.WriteLine $"Received {receivedAmount}. New balance: {curBalance + receivedAmount}"
                                Ready {
                                    Client = waitingMoneyState.Client
                                    Credentials = c1 :: c2 :: waitingMoneyState.Credentials
                                }
                            | WaitingMoneyForPayment waitingMoneyState ->
                                let newCredentials =
                                    waitingMoneyState.Client.HandleResponse (credentialResponse, waitingMoneyState.ValidationData)
                                    |> Array.ofSeq
                                let credFormPayment, credChange = newCredentials[0], newCredentials[1]
                                let paymentJson =
                                    credFormPayment
                                    |> Encode.credential
                                    |> Encode.toCompactString
                                sendTo (waitingMoneyState.Destination) paymentJson 
                                let credentials = credChange :: waitingMoneyState.Credentials

                                Console.WriteLine $"Sent {credFormPayment.Value}. New balance: {credentials |> List.sumBy (fun x -> x.Value)}"                                
                                
                                Ready {
                                    Client = waitingMoneyState.Client
                                    Credentials = credentials
                                }
                            | _ -> state
                        | PayerMessage (NewPaymentReceived credential) ->
                            match state with
                            | Ready readyState ->
                                let receivedAmount = credential.Value
                                let curBalance = readyState.Credentials |>  Seq.sumBy (fun x -> x.Value)
                                Console.ForegroundColor <- ConsoleColor.Green
                                Console.WriteLine $"Received {receivedAmount}. New balance: {curBalance + receivedAmount}"
                                Ready {
                                    Client = readyState.Client
                                    Credentials = credential :: readyState.Credentials
                                }
                            | _ -> state
                        | UserMessage (ConsolidateCredential) ->
                            match state with
                            | Ready readyState ->
                                let credentialsToUse, unSelectedCredential = getUsableCredentials (readyState.Credentials)
                                let amount = credentialsToUse |> Seq.sumBy (fun x -> x.Value)
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; 0], credentialsToUse, CancellationToken.None)
                                let requestJson =
                                    credentialsRequest.CredentialsRequest
                                    |> Encode.credentialsRequest
                                    |> Encode.toCompactString
                                Console.WriteLine "Consolidating..."
                                sendToMinter requestJson
                                WaitingMoney {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredential
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                }
                            | _ -> state
                        | UserMessage (SendTo (amount, destination)) ->
                            match state with
                            | Ready readyState ->
                                let credentialsToUse, unSelectedCredential = getUsableCredentials (readyState.Credentials)
                                let availableAmount = credentialsToUse |> Seq.sumBy (fun x -> x.Value)
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; availableAmount - amount], credentialsToUse, CancellationToken.None)
                                let requestJson =
                                    credentialsRequest.CredentialsRequest
                                    |> Encode.credentialsRequest
                                    |> Encode.toCompactString
                                sendToMinter requestJson
                                WaitingMoneyForPayment {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredential
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                    Destination = destination
                                }
                            | _ -> state
                        | UserMessage (Buy amount) ->
                            match state with
                            | Ready readyState ->
                                let credentialsToUse = readyState.Credentials |> List.filter (fun x -> x.Value = 0) |> List.take ProtocolConstants.CredentialNumber
                                let unSelectedCredentials = readyState.Credentials |> List.except credentialsToUse
                                let credentialsRequest = readyState.Client.CreateRequest ([amount; 0], credentialsToUse, CancellationToken.None)
                                let requestJson =
                                    credentialsRequest.CredentialsRequest
                                    |> Encode.credentialsRequest
                                    |> Encode.toCompactString
                                sendToMinter requestJson
                                WaitingMoney {
                                    Client = readyState.Client
                                    Credentials = unSelectedCredentials
                                    ValidationData = credentialsRequest.CredentialsResponseValidation
                                }
                            | _ -> state
                        | UserMessage (GetNullCredentials) ->
                            match state with
                            | Ready readyState ->
                                let zeroCredentials = readyState.Credentials |> List.filter (fun x -> x.Value = 0)
                                if List.length zeroCredentials <= 2 then
                                    let nullCredentialsRequest = readyState.Client.CreateRequestForZeroAmount ()
                                    let requestJson =
                                        nullCredentialsRequest.CredentialsRequest
                                        |> Encode.credentialsRequest
                                        |> Encode.toCompactString
                                    sendToMinter requestJson
                                    WaitingMoney {
                                        Client = readyState.Client
                                        Credentials = readyState.Credentials
                                        ValidationData = nullCredentialsRequest.CredentialsResponseValidation
                                    }
                                else
                                    state
                            | _ -> state
                    return! loop(newState) }
                loop (Uninitialized) )
        worker.Post

    let processMinterAnnouncement (event: Event) =
        event.Content
        |> Decode.fromString Decode.issuerParameters

    let  processMinterDirectMessage secret (event: Event) =
        event
        |> decryptDirectMessage secret
        |> Decode.fromString Decode.credentialsResponse

    let processPaymentDirectMessage secret (event: Event) =
        event
        |> decryptDirectMessage secret
        |> Decode.fromString Decode.credential
        
        
    let dispatchProtocolHandler secret pushToProtocolHandler = 
        commonProtocolHandler  
            (fun subscriptionId event ->
                match subscriptionId with
                | MINT_ANNOUNCEMENTS_SUBSCRIPTION ->
                    match processMinterAnnouncement event with
                    | Ok minterParameters ->
                        pushToProtocolHandler (MinterMessage (MintParametersAnnouncement minterParameters))
                        pushToProtocolHandler (UserMessage GetNullCredentials)
                    | Error e -> Console.WriteLine "Announcement was invalid"
                | MINT_DIRECT_MESSAGE_SUBSCRIPTION ->
                    match processMinterDirectMessage secret event with
                    | Ok issuanceResponse ->
                        pushToProtocolHandler (MinterMessage (NewCredentialsMinted issuanceResponse))
                        pushToProtocolHandler (UserMessage GetNullCredentials)
                    | Error e -> Console.WriteLine e
                | PAYMENT_DIRECT_MESSAGE_SUBSCRIPTION ->
                    let (XOnlyPubKey from) = event.PubKey
                    let (XOnlyPubKey minter) = minterPubKey
                    let fromMinter = Utils.toHex(from.ToBytes()) = Utils.toHex(minter.ToBytes())
                    if not fromMinter then
                        match processPaymentDirectMessage secret event with
                        | Ok credential ->
                            pushToProtocolHandler (PayerMessage (NewPaymentReceived credential))
                            pushToProtocolHandler (UserMessage (ConsolidateCredential))
                            pushToProtocolHandler (UserMessage GetNullCredentials)
                        | Error e -> Console.WriteLine e
                | _ -> Console.WriteLine "The relay is crazy or what!?")
    
    let processUserCommandLoop pushToProtocolHandler = 
        let rec loop () = async {
            Console.Write "# "
            let line = Console.ReadLine ()
            match line.Split [| ' ' |] with
            | [| "send"; amountStr; destinationStr |] ->
                let pubkeyResult = pubKeyFromHex $"'{destinationStr}'"
                let success, amount = Int32.TryParse amountStr
                match success, amount, pubkeyResult with
                | true, amount, Ok pubkey ->
                    pushToProtocolHandler (UserMessage (SendTo (amount, pubkey)))
                    Console.WriteLine "Done"
                | false, _, _ ->
                    Console.WriteLine "The amount is not a valid integer"
                | _ ->
                    Console.WriteLine "public key is invalid"
            | [| "buy"; amountStr |] ->
                match Int32.TryParse amountStr with
                | true, amount ->
                    pushToProtocolHandler (UserMessage (Buy amount))
                    Console.WriteLine $"Bought amount sabis from minter"
                | _ ->
                    Console.WriteLine "The amount is not a valid integer"
            | [||] ->
                Console.WriteLine ()
            | _ ->
                Console.WriteLine "?"
            do! loop ()
        }
        loop ()
