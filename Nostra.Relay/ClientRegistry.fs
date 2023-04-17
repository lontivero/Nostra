module ClientRegistry

open System.Collections.Generic
open System.Net
open Nostra.Relay

type EventEvaluator = StoredEvent -> unit
type ClientId = ClientId of IPAddress * uint16
    
type ClientRegistry = {
    subscribe : ClientId -> EventEvaluator -> unit
    unsubscribe : ClientId -> unit
    notifyEvent : StoredEvent -> unit        
}

type ClientRegistryAction =
    | Subscribe of ClientId * EventEvaluator
    | Unsubscribe of ClientId
    | NotifyEvent of StoredEvent
    
let createClientRegistry () =
    let evaluators = Dictionary<ClientId, EventEvaluator>()

    let notifyToAll event =
        evaluators
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun evaluator -> evaluator event)
        
    let worker =
        MailboxProcessor<ClientRegistryAction>.Start(fun inbox ->
            let rec loop () = async { 
                let! msg = inbox.Receive()
                match msg with
                | Subscribe (clientId, evaluator) -> evaluators.Add(clientId, evaluator) 
                | Unsubscribe clientId -> evaluators.Remove(clientId) |> ignore
                | NotifyEvent storedEvent -> notifyToAll storedEvent
                return! loop ()
            }
            loop () )
        
    {
        subscribe = fun clientId evaluator -> worker.Post (Subscribe (clientId, evaluator))
        unsubscribe = fun clientId -> worker.Post (Unsubscribe clientId)
        notifyEvent = fun storedEvent -> worker.Post (NotifyEvent storedEvent)
    }

