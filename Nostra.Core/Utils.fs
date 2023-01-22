namespace Nostra.Core

module Utils =
    open System

    let toHex (bytes:byte[]) =
       bytes |> Convert.ToHexString |> (fun s -> s.ToLower()) 

    let fromHex (str: string) =
        Convert.FromHexString str

    let (|Base64|_|) (len: int) (str:string) =
        let ishexa c = (c >= 'a' && c <= 'f') || (c >= '0' && c <= 'f')
        match str with
        | s when s.Length = len && s.ToCharArray() |> Array.forall ishexa ->
            Some (fromHex s)
        | _ -> None
        
module Monad =
    type Reader<'environment,'a> = Reader of ('environment -> 'a)

    let run environment (Reader action) =  
        let resultOfAction = action environment 
        resultOfAction
    