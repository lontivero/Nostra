module QueryBuildingTests

open System
open Nostra
open Nostra.Relay.Request
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions

let now = DateTime.Now
let defaultLimit = 50
let maxLimit = 5000

let createFilter s =
    let filter = s |> Thoth.Json.Net.Decode.fromString Filter.Decode.filter

    match filter with
    | Result.Error e -> failwith e
    | Ok filter -> filter

let materializeSingleQuery (query: Database.Query) =
    let baseQuery, limitStr, e, _ = Database.materializeQuery defaultLimit maxLimit query 0
    baseQuery + limitStr, e

type ``Single Filters``(output:ITestOutputHelper) =
        
    [<Fact>]
    let ``Query Empty`` () =
        let filter = createFilter "{ }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query

    [<Fact>]
    let ``Query Limit`` () =
        let filter = createFilter """{"limit": 10}"""
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt ORDER BY e.created_at DESC, e.id DESC LIMIT 10" query

    [<Fact>]
    let ``Query Kinds`` () =
        let filter = createFilter "{ \"kinds\" : [1,2] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.kind IN (@s0_e_kind_in0,@s0_e_kind_in1) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_kind_in0", 1
           "@s0_e_kind_in1", 2
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors`` () =
        let filter = createFilter "{ \"authors\" : [\"aabbcc\", \"332211\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.author IN (@s0_e_author_in0,@s0_e_author_in1) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_author_in0", Utils.fromHex "aabbcc" :> obj
           "@s0_e_author_in1", Utils.fromHex "332211" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Events`` () =
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.event_hash IN (@s0_e_event_hash_in0,@s0_e_event_hash_in1) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_event_hash_in0", Utils.fromHex "bbccaa" :> obj
           "@s0_e_event_hash_in1", Utils.fromHex "ddeeff" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Authors, Events and Kinds`` () =
        let filter = createFilter "{ \"ids\" : [\"bbccaa\", \"ddeeff\"], \"authors\" : [\"aabbcc\", \"332211\"], \"kinds\" : [1,2] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.author IN (@s0_e_author_in0,@s0_e_author_in1) AND e.kind IN (@s0_e_kind_in0,@s0_e_kind_in1) AND e.event_hash IN (@s0_e_event_hash_in0,@s0_e_event_hash_in1) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_author_in0", Utils.fromHex "aabbcc" :> obj
           "@s0_e_author_in1", Utils.fromHex "332211" :> obj
           "@s0_e_kind_in0", 1
           "@s0_e_kind_in1", 2
           "@s0_e_event_hash_in0", Utils.fromHex "bbccaa" :> obj
           "@s0_e_event_hash_in1", Utils.fromHex "ddeeff" :> obj
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Since`` () =
        let filter = createFilter "{ \"since\" : 12345678 }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.created_at > @s0_e_created_at_gt ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_created_at_gt", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Until`` () =
        let filter = createFilter "{ \"until\" : 12345678 }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.created_at < @s0_e_created_at_lt ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_created_at_lt", 12345678
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Simple Tags`` () =
        let filter = createFilter "{ \"#e\" : [\"223344\", \"443322\"], \"#p\": [\"888888\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name_eq AND t.value IN (@s1_t_value_in0,@s1_t_value_in1)) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s2_t_name_eq AND t.value IN (@s2_t_value_in0)) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s1_t_name_eq", "e"
           "@s1_t_value_in0", "223344"
           "@s1_t_value_in1", "443322"
           "@s2_t_name_eq", "p"
           "@s2_t_value_in0", "888888"
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Tags with Kinds`` () =
        let filter = createFilter "{ \"kinds\" : [1,2], \"#e\": [\"888888\"] }"
        let query, parameters  = materializeSingleQuery (Database.buildQueryForFilter now filter)
        should equal "SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.kind IN (@s0_e_kind_in0,@s0_e_kind_in1) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name_eq AND t.value IN (@s1_t_value_in0) AND t.kind IN (@s1_t_kind_in0,@s1_t_kind_in1)) ORDER BY e.created_at DESC, e.id DESC LIMIT 50" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_kind_in0", 1
           "@s0_e_kind_in1", 2
           "@s1_t_name_eq", "e"
           "@s1_t_value_in0", "888888"
           "@s1_t_kind_in0", 1
           "@s1_t_kind_in1", 2
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

type ``Subscriptions (multiple Filters)``(output:ITestOutputHelper) =

    [<Fact>]
    let ``Query event table only`` () =
        let filter1 = createFilter "{ \"kinds\" : [1,2] }"
        let filter2 = createFilter "{ \"kinds\" : [3] }"
        let query, parameters = Database.buildQueryForFilters [filter1; filter2] defaultLimit maxLimit now
        should equal "SELECT * FROM (SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.kind IN (@s0_e_kind_in0,@s0_e_kind_in1) ORDER BY e.created_at DESC, e.id DESC LIMIT 50) UNION SELECT * FROM (SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s1_e_deleted_eq AND e.expires_at > @s1_e_expires_at_gt AND e.kind IN (@s1_e_kind_in0) ORDER BY e.created_at DESC, e.id DESC LIMIT 50) ORDER BY created_at DESC, id DESC" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_kind_in0", 1
           "@s0_e_kind_in1", 2
           "@s1_e_deleted_eq", false
           "@s1_e_expires_at_gt", now :> obj
           "@s1_e_kind_in0", 3
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))

    [<Fact>]
    let ``Query Tags with Kinds`` () =
        let filter1 = createFilter "{ \"kinds\" : [1,2], \"#e\": [\"888888\"] }"
        let filter2 = createFilter "{ \"kinds\" : [3], \"limit\": 123 }"
        let query, parameters = Database.buildQueryForFilters [filter1; filter2] defaultLimit maxLimit now
        should equal "SELECT * FROM (SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s0_e_deleted_eq AND e.expires_at > @s0_e_expires_at_gt AND e.kind IN (@s0_e_kind_in0,@s0_e_kind_in1) AND e.id IN (SELECT t.event_id FROM tags t WHERE t.name = @s1_t_name_eq AND t.value IN (@s1_t_value_in0) AND t.kind IN (@s1_t_kind_in0,@s1_t_kind_in1)) ORDER BY e.created_at DESC, e.id DESC LIMIT 50) UNION SELECT * FROM (SELECT e.serialized_event, e.created_at, e.id FROM events e WHERE e.deleted = @s2_e_deleted_eq AND e.expires_at > @s2_e_expires_at_gt AND e.kind IN (@s2_e_kind_in0) ORDER BY e.created_at DESC, e.id DESC LIMIT 123) ORDER BY created_at DESC, id DESC" query
        should equal [
           "@s0_e_deleted_eq", false :> obj
           "@s0_e_expires_at_gt", now :> obj
           "@s0_e_kind_in0", 1
           "@s0_e_kind_in1", 2
           "@s1_t_name_eq", "e"
           "@s1_t_value_in0", "888888"
           "@s1_t_kind_in0", 1
           "@s1_t_kind_in1", 2
           "@s2_e_deleted_eq", false
           "@s2_e_expires_at_gt", now :> obj
           "@s2_e_kind_in0", 3
        ] (parameters |> List.map (fun (k, v) -> k, v.Value ))
