namespace Nostra.Client

open System
open System.Globalization
open System.Reflection
open System.Threading
open NBitcoin.Secp256k1
open Nostra.Core
open Nostra.Core.Client.Response
open Nostra.Core.Relay.Request
open WabiSabi.Crypto
open WabiSabi.Crypto.Groups
open WabiSabi.Crypto.Randomness
open WabiSabi.Crypto.ZeroKnowledge
open Thoth.Json.Net

module WabiSabi =
    open WabiSabi
    open WabiSabi.CredentialRequesting

    module Utils =
        let createInstance<'a> (args : obj[])=
                Activator.CreateInstance(
                    typeof<'a>,
                    BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.CreateInstance,
                    Type.DefaultBinder,
                    args,
                    CultureInfo.InvariantCulture) :?> 'a
        
    module Decode =

        let ge : Decoder<GroupElement> =
            Decode.string
            |> Decode.andThen (fun s ->
                Decode.succeed (s |> Utils.fromHex |> GroupElement.FromBytes))

        let scalar : Decoder<Scalar> =
            Decode.string
            |> Decode.andThen (fun s ->
                Decode.succeed (s |> Utils.fromHex |> Scalar))

        let issuanceRequest : Decoder<IssuanceRequest> =
            Decode.object( fun get ->
                let ma = get.Required.Field "ma" ge 
                let bitCommitments = get.Required.Field "bitcomm" (Decode.list ge)
                Utils.createInstance<IssuanceRequest> ( [|ma; bitCommitments|] : obj[] )
            )

        let credentialPresentation : Decoder<CredentialPresentation> =
            Decode.object ( fun get ->
                let ca = get.Required.Field "ca" ge
                let cx0 = get.Required.Field "cx0" ge
                let cx1 = get.Required.Field "cx1" ge
                let s = get.Required.Field "s" ge
                let cv = get.Required.Field "cv" ge
                Utils.createInstance<CredentialPresentation> [|ca; cx0; cx1; cv; s|]
            )

        let proof : Decoder<Proof> =
            Decode.object (fun get ->
                let responses = get.Required.Field "responses" (Decode.array scalar)
                let nonces = get.Required.Field "nonces" (Decode.array ge)
                Utils.createInstance<Proof> ([|
                    Utils.createInstance<GroupElementVector> (nonces |> Array.map (fun x -> x : obj))
                    Utils.createInstance<ScalarVector> ( responses |> Array.map (fun x -> x : obj))
                |] : obj[])
            )

        let credentialsRequest : Decoder<RealCredentialsRequest> =
            Decode.object (fun get ->
                let delta = get.Required.Field "delta" Decode.int64
                let requested = get.Required.Field "requested" (Decode.list issuanceRequest)
                let presented = get.Required.Field "presented" (Decode.list credentialPresentation)
                let proofs = get.Required.Field "proofs" (Decode.list proof)
                Utils.createInstance<RealCredentialsRequest> ([|delta; presented; requested; proofs|] : obj[])
            )
        
        let issuanceValidationData :  Decoder<IssuanceValidationData> =
            Decode.object (fun get ->
                let ma = get.Required.Field "ma" ge
                let r = get.Required.Field "r" scalar
                let v = get.Required.Field "v" Decode.int64
                Utils.createInstance<IssuanceValidationData> ([|v; r; ma|] : obj[])
            )

        let mac : Decoder<MAC> =
            Decode.object (fun get ->
                let t = get.Required.Field "t" scalar
                let v = get.Required.Field "v" ge
                Utils.createInstance<MAC> ([|t; v|] : obj[])
            )
        
        let credentialsResponse : Decoder<CredentialsResponse> =
            Decode.object (fun get ->
                let proofs = get.Required.Field "proofs" (Decode.list proof)
                let issuedCredentials = get.Required.Field "issuedCredentials" (Decode.list mac)
                Utils.createInstance<CredentialsResponse> [|issuedCredentials; proofs|]
            )

        let issuerParameters : Decoder<CredentialIssuerParameters> =
            Decode.object (fun get ->
                let cw = get.Required.Field "cw" ge
                let i = get.Required.Field "i" ge
                CredentialIssuerParameters(cw, i)
            )

        let credential : Decoder<Credential> =
            Decode.object (fun get ->
                let mac = get.Required.Field "mac" mac
                let r = get.Required.Field "r" scalar
                let v = get.Required.Field "v" Decode.int64
                Credential(v, r, mac)
            )

    module Encode =
        let ge (ge: GroupElement) =
            ge.ToBytes() |> Utils.toHex |> Encode.string

        let scalar (s: Scalar) =
            s.ToBytes() |> Utils.toHex |> Encode.string

        let issuanceRequest (x: IssuanceRequest) =
            Encode.object [
                "ma", ge x.Ma
                "bitcomm", x.BitCommitments |> Seq.map ge |> Encode.seq 
            ]

        let credentialPresentation (x: CredentialPresentation) =
            Encode.object [
                "ca", ge x.Ca
                "cx0", ge x.Cx0
                "cx1", ge x.Cx1
                "s", ge x.S
                "cv", ge x.CV
            ]

        let proof (x: Proof) =
            Encode.object [
                "responses", x.Responses |> Seq.map scalar |> Encode.seq
                "nonces", x.PublicNonces |> Seq.map ge |> Encode.seq
            ]

        let credentialsRequest (x: ICredentialsRequest) =
            Encode.object [
                "delta", Encode.int64 x.Delta
                "requested", x.Requested |> Seq.map issuanceRequest |> Encode.seq
                "presented", x.Presented |> Seq.map credentialPresentation |> Encode.seq
                "proofs", x.Proofs |> Seq.map proof |> Encode.seq
            ]

        let issuanceValidationData (x: IssuanceValidationData) =
            Encode.object [
                "ma", ge x.Ma
                "r", scalar x.Randomness
                "v", Encode.int64 x.Value
            ]

        let mac (x: MAC) =
            Encode.object [
                "t", scalar x.T
                "v", ge x.V
            ]
            
        let credentialsResponse (x: CredentialsResponse) =
            Encode.object [
                "proofs", x.Proofs |> Seq.map proof |> Encode.seq 
                "issuedCredentials", x.IssuedCredentials |> Seq.map mac |> Encode.seq 
            ]

        let issuerParameters (x: CredentialIssuerParameters) =
            Encode.object [
                "cw", ge x.Cw
                "i", ge x.I
            ]

        let credential (x: Credential) =
            Encode.object [
                "mac", mac x.Mac
                "r", scalar x.Randomness
                "v", Encode.int64 x.Value
            ]
            
    module Test =
        let testIt () =
            let maxAmount = (2L <<< 32) - 1L 
            let rnd = SecureRandom()
            let issuerKey = CredentialIssuerSecretKey(rnd)
            let issuer = CredentialIssuer(issuerKey, rnd, maxAmount)
    
            let client = WabiSabiClient(issuerKey.ComputeCredentialIssuerParameters(), rnd, maxAmount)
            let nullValueCredentialRequest = client.CreateRequestForZeroAmount()
            let nullValueResponse = issuer.HandleRequest (nullValueCredentialRequest.CredentialsRequest)
            let nullCredentials = client.HandleResponse (nullValueResponse, nullValueCredentialRequest.CredentialsResponseValidation)
            
            let requestData = client.CreateRequest ([5000L; 6000L], nullCredentials, CancellationToken.None)
            let credentialRequestJson =
                Encode.credentialsRequest requestData.CredentialsRequest |> Encode.toCompactString

            let credentialsResult =
                credentialRequestJson
                |> (Decode.fromString Decode.credentialsRequest)
                |> Result.bind (fun credentialRequest -> Ok <| issuer.HandleRequest(credentialRequest) )
                |> Result.bind (fun credentialResponse ->
                    Encode.credentialsResponse credentialResponse
                    |> Encode.toCompactString
                    |> (Decode.fromString Decode.credentialsResponse)
                    |> Result.bind (fun credentialResponse -> Ok <| client.HandleResponse (credentialResponse, requestData.CredentialsResponseValidation)))
                
            match credentialsResult with
            | Ok credentials -> Console.WriteLine "si"
            | Error e -> Console.WriteLine e
                
            0