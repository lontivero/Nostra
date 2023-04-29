module QueryBuildingTests

open Fumble
open Nostra
open Nostra.Client.Request
open Nostra.Relay.Request
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions


type ``Filters``(output:ITestOutputHelper) =

    let createFilter s =
        let filter = s |> Thoth.Json.Net.Decode.fromString Filter.Decode.filter
        
        match filter with
        | Error e -> failwith e
        | Ok filter -> filter 
        
    [<Fact>]
    let ``Query Empty`` () =   
        let filter = createFilter "{ }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted" query

    [<Fact>]
    let ``Query Kinds`` () =   
        let filter = createFilter "{ \"kinds\" : [1,2] }"
        let query, parameters = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.kind IN (@e_kind0,@e_kind1)" query
        should equal [
           "@e_deleted", false :> obj
           "e_kind0", 1
           "e_kind1", 2
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors`` () =   
        let filter = createFilter "{ \"authors\" : [\"aabbcc\", \"332211\"] }"
        let query, parameters = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.author IN (@e_author0,@e_author1)" query
        should equal [
           "@e_deleted", false :> obj
           "e_author0", Utils.fromHex "aabbcc" :> obj
           "e_author1", Utils.fromHex "332211" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Events`` () =   
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"] }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.event_hash IN (@e_event_hash0,@e_event_hash1)" query
        should equal [
           "@e_deleted", false :> obj
           "e_event_hash0", Utils.fromHex "bbccaa" :> obj
           "e_event_hash1", Utils.fromHex "ddeeff" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors, Events and Kinds`` () =   
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"], \"authors\" : [\"aabbcc\", \"332211\"], \"kinds\" : [1,2] }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.author IN (@e_author0,@e_author1) AND e.kind IN (@e_kind0,@e_kind1) AND e.event_hash IN (@e_event_hash0,@e_event_hash1)" query
        should equal [
           "@e_deleted", false :> obj
           "e_author0", Utils.fromHex "aabbcc" :> obj
           "e_author1", Utils.fromHex "332211" :> obj
           "e_kind0", 1
           "e_kind1", 2
           "e_event_hash0", Utils.fromHex "bbccaa" :> obj
           "e_event_hash1", Utils.fromHex "ddeeff" :> obj           
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Since`` () =   
        let filter = createFilter "{ \"since\" : 12345678 }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.created_at > @e_created_at" query
        should equal [
           "@e_deleted", false :> obj
           "@e_created_at", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Until`` () =   
        let filter = createFilter "{ \"until\" : 12345678 }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.created_at < @e_created_at" query
        should equal [
           "@e_deleted", false :> obj
           "@e_created_at", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))
        
    [<Fact>]
    let ``Query Simple Tags`` () =   
        let filter = createFilter "{ \"#e\" : [\"223344\", \"443322\"], \"#p\": [\"888888\"] }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = 'e' AND t.value IN (@t_value0,@t_value1)) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = 'p' AND t.value IN (@t_value0))" query

    [<Fact>]
    let ``Query Tags with Kinds`` () =   
        let filter = createFilter "{ \"kinds\" : [1,2], \"#e\": [\"888888\"] }"
        let query, parameters  = Database.buildQuery filter       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @e_deleted AND e.kind IN (@e_kind0,@e_kind1) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = 'e' AND t.value IN (@t_value0) AND t.kind IN (@t_kind0,@t_kind1))" query
