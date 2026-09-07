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

let processClientRegistrationRequestLoop
        (evaluators: Dictionary<ClientId, EventEvaluator>)
        (notifyToAll: StoredEvent -> unit)
        (inbox: MailboxProcessor<ClientRegistryAction>) = async {
    while true do
        let! msg = inbox.Receive()
        match msg with
        | Subscribe (clientId, evaluator) -> evaluators.Add(clientId, evaluator)
        | Unsubscribe clientId -> evaluators.Remove(clientId) |> ignore
        | NotifyEvent storedEvent -> notifyToAll storedEvent
}

let createClientRegistry () =
    let evaluators = Dictionary<ClientId, EventEvaluator>()

    let notifyToAll event =
        evaluators
        |> Seq.map (_.Value)
        |> Seq.iter (fun evaluator ->
            try evaluator event
            with _ -> ())

    let worker =
        MailboxProcessor<ClientRegistryAction>.Start(processClientRegistrationRequestLoop evaluators notifyToAll)

    {
        subscribe = fun clientId evaluator -> worker.Post (Subscribe (clientId, evaluator))
        unsubscribe = fun clientId -> worker.Post (Unsubscribe clientId)
        notifyEvent = fun storedEvent -> worker.Post (NotifyEvent storedEvent)
    }

