module QueryBuildingTests

open Nostra
open Nostra.Client.Request
open Nostra.Relay.Request
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions

let createFilter s =
    let filter = s |> Thoth.Json.Net.Decode.fromString Filter.Decode.filter
    
    match filter with
    | Error e -> failwith e
    | Ok filter -> filter 

let materializeSingleQuery (query: Database.Query) =
    let s, e, _ = Database.materializeQuery query 0
    s, e    

type ``Single Filters``(output:ITestOutputHelper) =
        
    [<Fact>]
    let ``Query Empty`` () =   
        let filter = createFilter "{ }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted" query

    [<Fact>]
    let ``Query Kinds`` () =   
        let filter = createFilter "{ \"kinds\" : [1,2] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.kind IN (@s0_e_kind0,@s0_e_kind1)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_kind0", 1
           "@s0_e_kind1", 2
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors`` () =   
        let filter = createFilter "{ \"authors\" : [\"aabbcc\", \"332211\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.author IN (@s0_e_author0,@s0_e_author1)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_author0", Utils.fromHex "aabbcc" :> obj
           "@s0_e_author1", Utils.fromHex "332211" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Events`` () =   
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.event_hash IN (@s0_e_event_hash0,@s0_e_event_hash1)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_event_hash0", Utils.fromHex "bbccaa" :> obj
           "@s0_e_event_hash1", Utils.fromHex "ddeeff" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors, Events and Kinds`` () =   
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"], \"authors\" : [\"aabbcc\", \"332211\"], \"kinds\" : [1,2] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.author IN (@s0_e_author0,@s0_e_author1) AND e.kind IN (@s0_e_kind0,@s0_e_kind1) AND e.event_hash IN (@s0_e_event_hash0,@s0_e_event_hash1)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_author0", Utils.fromHex "aabbcc" :> obj
           "@s0_e_author1", Utils.fromHex "332211" :> obj
           "@s0_e_kind0", 1
           "@s0_e_kind1", 2
           "@s0_e_event_hash0", Utils.fromHex "bbccaa" :> obj
           "@s0_e_event_hash1", Utils.fromHex "ddeeff" :> obj           
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Since`` () =   
        let filter = createFilter "{ \"since\" : 12345678 }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.created_at > @s0_e_created_at" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_created_at", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Until`` () =   
        let filter = createFilter "{ \"until\" : 12345678 }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.created_at < @s0_e_created_at" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_created_at", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))
        
    [<Fact>]
    let ``Query Simple Tags`` () =   
        let filter = createFilter "{ \"#e\" : [\"223344\", \"443322\"], \"#p\": [\"888888\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name AND t.value IN (@s1_t_value0,@s1_t_value1)) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s2_t_name AND t.value IN (@s2_t_value0))" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s1_t_name", "e"
           "@s1_t_value0", "223344"
           "@s1_t_value1", "443322"
           "@s2_t_name", "p"
           "@s2_t_value0", "888888"
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Tags with Kinds`` () =   
        let filter = createFilter "{ \"kinds\" : [1,2], \"#e\": [\"888888\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter filter)       
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.kind IN (@s0_e_kind0,@s0_e_kind1) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name AND t.value IN (@s1_t_value0) AND t.kind IN (@s1_t_kind0,@s1_t_kind1))" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_kind0", 1
           "@s0_e_kind1", 2
           "@s1_t_name", "e"
           "@s1_t_value0", "888888"
           "@s1_t_kind0", 1
           "@s1_t_kind1", 2
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

type ``Subscriptions (multiple Filters)``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Query event table only`` () =   
        let filter1 = createFilter "{ \"kinds\" : [1,2] }"
        let filter2 = createFilter "{ \"kinds\" : [3] }"
        let query, parameters = Database.buildQueryForFilters [filter1; filter2]
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.kind IN (@s0_e_kind0,@s0_e_kind1) UNION SELECT e.serialized_event FROM events e WHERE e.deleted = @s1_e_deleted AND e.kind IN (@s1_e_kind0)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_kind0", 1
           "@s0_e_kind1", 2
           "@s1_e_deleted", false
           "@s1_e_kind0", 3
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Tags with Kinds`` () =   
        let filter1 = createFilter "{ \"kinds\" : [1,2], \"#e\": [\"888888\"] }"
        let filter2 = createFilter "{ \"kinds\" : [3] }"
        let query, parameters = Database.buildQueryForFilters [filter1; filter2]
        should equal "SELECT e.serialized_event FROM events e WHERE e.deleted = @s0_e_deleted AND e.kind IN (@s0_e_kind0,@s0_e_kind1) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name AND t.value IN (@s1_t_value0) AND t.kind IN (@s1_t_kind0,@s1_t_kind1)) UNION SELECT e.serialized_event FROM events e WHERE e.deleted = @s2_e_deleted AND e.kind IN (@s2_e_kind0)" query
        should equal [
           "@s0_e_deleted", false :> obj
           "@s0_e_kind0", 1
           "@s0_e_kind1", 2
           "@s1_t_name", "e"
           "@s1_t_value0", "888888"
           "@s1_t_kind0", 1
           "@s1_t_kind1", 2
           "@s2_e_deleted", false
           "@s2_e_kind0", 3
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))
