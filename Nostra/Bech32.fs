namespace Nostra

module Bech32 =
    type HRP = string

    let toWord5 (x:int) = byte (x &&& 31)

    let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
    let charsetRev =
        [|
            -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
            -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
            -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
            15; -1; 10; 17; 21; 20; 26; 30;  7;  5; -1; -1; -1; -1; -1; -1;
            -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
             1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1;
            -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
             1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1;
        |]
        |> Array.map (fun x -> if x = -1 then None else Some (toWord5 x))

    let generator: int list =
        [ 0x3b6a57b2; 0x26508e6d; 0x1ea119fa; 0x3d4233dd; 0x2a1462b3 ]

    let polymod values =
        (1, values)
        ||> List.fold (fun chk v ->
            let b = chk >>> 25
            let chk = ((chk &&& 0x1ffffff) <<< 5) ^^^ int v

            let chk =
                (chk, [ 0..4 ])
                ||> List.fold (fun chk i -> if (b >>> i) &&& 1 = 1 then chk ^^^ generator[i] else chk)

            chk)

    let hrpExpand (hrp: HRP) =
        [ for x in hrp do
              byte x >>> 5 ]
        @ [ 0uy ]
        @ [ for x in hrp do
                byte x &&& 31uy ]

    let verifyChecksum hrp data =
        let hrpExpanded = hrp |> hrpExpand
        polymod (hrpExpanded @ data) = 1

    type Pad = int -> int -> int list list -> int list list

    let yesPadding bits padValue result =
        match (bits, padValue, result) with
        | 0, _, result -> result
        | _, padValue, result -> [ padValue ] :: result

    let noPadding _ _ result = result

    let convertBits (data: byte list) fromBits toBits (pad: Pad) =
        let maxValue = (1 <<< toBits) - 1

        let result, acc, bits =
            data
            |> List.fold (fun (result, acc, bits) value ->
                let acc' = (acc <<< fromBits) ||| int value
                let bits' = bits + fromBits
                let result' = [for b in [(bits' - toBits) .. -toBits .. 0] do (acc' >>> b) &&& maxValue]
                result' :: result, acc', bits' % toBits) ([], 0, 0)
        let padValue = (acc <<< (toBits - bits)) &&& maxValue
        pad bits padValue result |> List.rev |> List.concat

    let toBase32 data : byte list =
        convertBits data 8 5 yesPadding |> List.map toWord5

    let toBase256 data : byte list =
        convertBits data 5 8 noPadding |> List.map byte

    let createChecksum hrp data =
        let values = hrpExpand hrp @ data
        let padx = List.replicate 6 (toWord5 0)
        let polymod = (polymod (values @ padx)) ^^^ 1

        [ for i in [ 0..5 ] do
              toWord5 (polymod >>> 5 * (5 - i)) ]

    let encode hrp data : string =
        let data = toBase32 data

        let encoded =
            (data @ createChecksum hrp data)
            |> List.map (fun i -> charset[int i])
            |> List.toArray
            |> System.String

        hrp + "1" + encoded

    let decode (str: string) =
        let lift l =
            if List.contains None l then
                None
            else
                Some(List.map Option.get l)

        let lastOneIndex = str.IndexOf('1')
        let hrp = HRP str[0 .. lastOneIndex - 1]
        let data = str[lastOneIndex + 1 ..]

        data
        |> Seq.map (fun x -> charsetRev[int x])
        |> Seq.toList
        |> lift
        |> Option.bind (fun d ->
            if verifyChecksum hrp d then
                Some(d[.. d.Length - 7])
            else
                None)
        |> Option.map (fun data -> (hrp, toBase256 data |> List.toArray))

module Shareable =
    open System.Text
    open Microsoft.FSharp.Collections
    open NBitcoin.Secp256k1

    type Relay = string
    type ShareableEntity =
    | NSec of SecretKey
    | NPub of AuthorId
    | Note of EventId
    | NProfile of AuthorId * Relay list
    | NEvent of EventId * Relay list * AuthorId option * Kind option
    | NRelay of Relay

    let private _encode hrp bytesArr =
        Bech32.encode hrp (bytesArr |> Array.toList)

    let encode = function
        | NSec (SecretKey ecPrivKey) ->
            let bytes = Array.create 32 0uy
            ecPrivKey.WriteToSpan bytes
            bytes |> _encode "nsec"
        | NPub author ->
            AuthorId.toBytes author |> _encode "npub"
        | Note(EventId eventId) ->
            eventId |> _encode "note"
        | NProfile(author, relays) ->
            let author = AuthorId.toBytes author |> Array.toList
            let encodedPubKey = 0uy :: 32uy :: author
            let encodedRelays =
                relays
                |> List.map (Encoding.ASCII.GetBytes >> Array.toList)
                |> List.map (fun encodedRelay -> 1uy :: byte (encodedRelay.Length) :: encodedRelay)
                |> List.concat
            encodedPubKey @ encodedRelays
            |> Bech32.encode "nprofile"
        | NEvent(EventId eventId, relays, author, kind) ->
            let encodedRelays =
                relays
                |> List.map (Encoding.ASCII.GetBytes >> Array.toList)
                |> List.map (fun encodedRelay -> 1uy :: byte (encodedRelay.Length) :: encodedRelay)
                |> List.concat
            let encodedAuthor =
                author
                |> Option.map (fun author -> 2uy :: 32uy :: List.ofArray (AuthorId.toBytes author))
                |> Option.defaultValue []
            let encodedKind =
                kind
                |> Option.map (fun kind -> 3uy :: 4uy :: List.ofArray (Utils.toBE (int kind)))
                |> Option.defaultValue []

            [
                0uy :: 32uy :: (List.ofArray eventId)
                encodedRelays
                encodedAuthor
                encodedKind
            ]
            |> List.concat
            |> Bech32.encode "nevent"
        | NRelay relayUrl ->
            relayUrl
            |> Encoding.ASCII.GetBytes
            |> List.ofArray
            |> fun encodedRelay -> 0uy :: byte (encodedRelay.Length) :: encodedRelay
            |> Bech32.encode "nrelay"

    let parseTLV (bytes : byte[]) =
        let rec parse = function
            | typ :: len :: rest -> (typ, rest[..(int len) - 1]) :: (parse rest[int len..])
            | _ -> []

        let elemByType = parse (bytes |> List.ofArray)
        [0uy..3uy]
        |> List.map (fun typ0 -> elemByType |> List.filter (fun (typ1, _) -> typ0 = typ1) |> List.map snd)

    let decode str =
        Bech32.decode str
        |> Option.bind (fun (hrp, byteArray) ->
            match hrp with
            | "nsec" ->
                ECPrivKey.TryCreate byteArray
                |> Option.ofTuple
                |> Option.map SecretKey
                |> Option.map NSec
            | "npub" ->
                ECXOnlyPubKey.TryCreate byteArray
                |> Option.ofTuple
                |> Option.map AuthorId
                |> Option.map NPub
            | "note" ->
                Some (Note (EventId byteArray))
            | "nprofile" ->
                match parseTLV byteArray with
                | [[secKey]; relays; _; _] ->
                    ECXOnlyPubKey.TryCreate (secKey |> List.toArray)
                    |> Option.ofTuple
                    |>  Option.map AuthorId
                    |> Option.map (fun key -> NProfile(key, relays |> List.map (List.toArray >> Encoding.ASCII.GetString)))
                | _ -> None
            | "nevent" ->
                match parseTLV byteArray with
                | [[eventId]; relays; authors; kinds]  ->
                    Some (NEvent(
                            EventId (eventId |> List.toArray),
                            relays |> List.map (List.toArray >> Encoding.ASCII.GetString),
                            authors
                            |> List.tryHead
                            |> Option.bind (fun author ->
                                ECXOnlyPubKey.TryCreate (author |> List.toArray)
                                |> Option.ofTuple
                                |> Option.map AuthorId),
                            kinds
                            |> List.tryHead
                            |> Option.map (List.toArray >> Utils.fromBE >> LanguagePrimitives.EnumOfValue)
                        ))
                | _ -> None
            | "nrelay" ->
                match parseTLV byteArray with
                | [[relayUrl]; _; _; _]  ->
                    Some (NRelay(Encoding.ASCII.GetString (List.toArray relayUrl)))
                | _ -> None
            | _ -> None
            )

    [<CompiledName("ToNPub")>]
    let encodeNpub author =
        encode (NPub author)

    let decodeNpub str =
        decode str
        |> Option.bind (function
            | NPub pk -> Some pk
            | _ ->  None)

    [<CompiledName("FromNPub")>]
    let _decodeNpub = decodeNpub >> Option.get

    [<CompiledName("ToNSec")>]
    let encodeNsec secret =
        encode (NSec secret)

    let decodeNsec str =
        decode str
        |> Option.bind (function
            | NSec sk -> Some sk
            | _ ->  None)

    [<CompiledName("FromNSec")>]
    let _decodeNsec = decodeNsec >> Option.get

    [<CompiledName("ToNote")>]
    let encodeNote note =
        encode (Note note)

    let decodeNote str =
        decode str
        |> Option.bind (function
            | Note eventId -> Some eventId
            | _ ->  None)

    [<CompiledName("FromNote")>]
    let _decodeNote = decodeNote >> Option.get

    [<CompiledName("ToNProfile")>]
    let encodeNprofile profile =
        encode (NProfile profile)

    let decodeNprofile str =
        decode str
        |> Option.bind (function
            | NProfile (pk, relays) -> Some (pk, relays)
            | _ ->  None)

    [<CompiledName("FromNProfile")>]
    let _decodeNprofile = decodeNprofile >> Option.get

    [<CompiledName("ToNEvent")>]
    let encodeNevent event =
        encode (NEvent event)

    let decodeNevent str =
        decode str
        |> Option.bind (function
            | NEvent (eventId, relays, author, kind) -> Some (eventId, relays, author, kind)
            | _ ->  None)

    [<CompiledName("FromNEvent")>]
    let _decodeNevent = decodeNevent >> Option.get

    [<CompiledName("ToNRelay")>]
    let encodeNrelay relay =
        encode (NRelay relay)

    let decodeNrelay str =
        decode str
        |> Option.bind (function
            | NRelay relays -> Some relays
            | _ ->  None)

    [<CompiledName("FromNRelay")>]
    let _decodeNrelay = decodeNrelay >> Option.get