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
        pad toBits padValue result |> List.rev |> List.concat

    let toBase32 data : byte list =
        convertBits data 8 5 yesPadding |> List.map toWord5

    let toBase256 data : byte list =
        convertBits data 5 8 noPadding |> List.map byte

    let createChecksum hrp data =
        let values = hrpExpand (hrp) @ data
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
        |> Option.map (fun data -> (hrp, toBase256 data))
