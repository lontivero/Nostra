using Microsoft.FSharp.Collections;
using static Nostra.Client;
using Response = Nostra.Client.Response;

namespace Nostra.CSharp;

#region Option Extensions

/// <summary>
/// Extension methods for working with F# Option types in C#.
/// </summary>
public static class OptionExtensions
{
    /// <summary>
    /// Converts an F# Option to a nullable value type.
    /// </summary>
    public static T? ToNullable<T>(this FSharpOption<T> option) where T : struct
        => FSharpOption<T>.get_IsSome(option) ? option.Value : null;

    /// <summary>
    /// Converts an F# Option to a nullable reference type.
    /// </summary>
    public static T? ToNullableRef<T>(this FSharpOption<T> option) where T : class
        => FSharpOption<T>.get_IsSome(option) ? option.Value : null;

    /// <summary>
    /// Converts a nullable value type to an F# Option.
    /// </summary>
    public static FSharpOption<T> ToOption<T>(this T? value) where T : struct
        => value.HasValue ? FSharpOption<T>.Some(value.Value) : FSharpOption<T>.None;

    /// <summary>
    /// Converts a nullable reference type to an F# Option.
    /// </summary>
    public static FSharpOption<T> ToOptionRef<T>(this T? value) where T : class
        => value is not null ? FSharpOption<T>.Some(value) : FSharpOption<T>.None;

    /// <summary>
    /// Creates Some(value).
    /// </summary>
    public static FSharpOption<T> Some<T>(T value) => FSharpOption<T>.Some(value);

    /// <summary>
    /// Creates None.
    /// </summary>
    public static FSharpOption<T> None<T>() => FSharpOption<T>.None;
}

#endregion

#region Result Extensions

/// <summary>
/// Extension methods for working with F# Result types in C#.
/// </summary>
public static class ResultExtensions
{
    /// <summary>
    /// Tries to get the success value from a Result.
    /// </summary>
    public static bool TryGetValue<T, TError>(this FSharpResult<T, TError> result,
        out T value, out TError? error)
    {
        if (result.IsOk)
        {
            value = result.ResultValue;
            error = default;
            return true;
        }
        value = default!;
        error = result.ErrorValue;
        return false;
    }

    /// <summary>
    /// Gets the success value or throws an exception.
    /// </summary>
    public static T GetValueOrThrow<T, TError>(this FSharpResult<T, TError> result)
        => result.IsOk
            ? result.ResultValue
            : throw new InvalidOperationException(result.ErrorValue?.ToString());

    /// <summary>
    /// Gets the success value or returns a default value.
    /// </summary>
    public static T GetValueOrDefault<T, TError>(this FSharpResult<T, TError> result, T defaultValue)
        => result.IsOk ? result.ResultValue : defaultValue;

    /// <summary>
    /// Maps the success value using the provided function.
    /// </summary>
    public static FSharpResult<TResult, TError> Select<T, TResult, TError>(
        this FSharpResult<T, TError> result, Func<T, TResult> selector)
        => result.IsOk
            ? FSharpResult<TResult, TError>.NewOk(selector(result.ResultValue))
            : FSharpResult<TResult, TError>.NewError(result.ErrorValue);
}

#endregion

#region List Extensions

/// <summary>
/// Extension methods for converting between C# collections and F# lists.
/// </summary>
public static class List
{
    /// <summary>
    /// Converts an IEnumerable to an F# list.
    /// </summary>
    public static FSharpList<T> ToFSharpList<T>(this IEnumerable<T> source)
        => ListModule.OfSeq(source);

    /// <summary>
    /// Creates an F# list from params array.
    /// </summary>
    public static FSharpList<T> FSharpList<T>(params T[] items)
        => ListModule.OfArray(items);
}

#endregion

#region RelayMessage Pattern Matching

/// <summary>
/// C#-friendly discriminated union for relay messages.
/// Enables idiomatic pattern matching in C#.
/// </summary>
public abstract record RelayMessageResult
{
    private RelayMessageResult() { }

    /// <summary>
    /// An event message from the relay.
    /// </summary>
    public sealed record Event(string SubscriptionId, EventT EventData) : RelayMessageResult;

    /// <summary>
    /// A notice message from the relay.
    /// </summary>
    public sealed record Notice(string Message) : RelayMessageResult;

    /// <summary>
    /// An acknowledgment message from the relay.
    /// </summary>
    public sealed record Ack(EventIdT EventId, bool Success, string Message) : RelayMessageResult;

    /// <summary>
    /// End of stored events message.
    /// </summary>
    public sealed record EndOfStoredEvents(string SubscriptionId) : RelayMessageResult;
}

/// <summary>
/// Extension methods for RelayMessage.
/// </summary>
public static class RelayMessageExtensions
{
    /// <summary>
    /// Converts an F# RelayMessage discriminated union to a C#-friendly record type.
    /// Enables idiomatic pattern matching with switch expressions.
    /// </summary>
    public static RelayMessageResult ToResult(this RelayMessage msg)
    {
        if (msg.IsRMEvent)
        {
            var (subscriptionId, eventData) = Response.GetEvent(msg);
            return new RelayMessageResult.Event(subscriptionId, eventData);
        }
        if (msg.IsRMNotice)
        {
            return new RelayMessageResult.Notice(Response.GetNotice(msg));
        }
        if (msg.IsRMACK)
        {
            var (eventId, success, message) = Response.GetAck(msg);
            return new RelayMessageResult.Ack(eventId, success, message);
        }
        if (msg.IsRMEOSE)
        {
            return new RelayMessageResult.EndOfStoredEvents(Response.GetEOSE(msg));
        }
        throw new InvalidOperationException("Unknown relay message type");
    }

    /// <summary>
    /// Tries to get the event data if this is an event message.
    /// </summary>
    public static bool TryGetEvent(this RelayMessage msg, out string subscriptionId, out EventT eventData)
    {
        if (msg.IsRMEvent)
        {
            (subscriptionId, eventData) = Response.GetEvent(msg);
            return true;
        }
        subscriptionId = default!;
        eventData = default!;
        return false;
    }
}

#endregion

#region RelayClient Extensions

/// <summary>
/// Extension methods for RelayClient providing C#-friendly overloads.
/// </summary>
public static class RelayClientExtensions
{
    /// <summary>
    /// Subscribes to events matching the provided filters.
    /// </summary>
    public static void Subscribe(this RelayClient relay, string subscriptionId,
        params SubscriptionFilter[] filters)
    {
        Client.Subscribe(subscriptionId, filters.ToFSharpList(), relay);
    }

    /// <summary>
    /// Publishes an event to the relay.
    /// </summary>
    public static void Publish(this RelayClient relay, EventT signedEvent)
    {
        Client.Publish(signedEvent, relay);
    }

    /// <summary>
    /// Starts listening for messages from the relay with raw Result handling.
    /// </summary>
    public static Task StartListeningRawAsync(this RelayClient relay,
        Action<FSharpResult<RelayMessage, string>> callback)
    {
        var fsharpFunc = FuncConvert.FromAction(callback);
        return StartListening(fsharpFunc, relay);
    }

    /// <summary>
    /// Starts listening for messages with C#-friendly pattern matching support.
    /// Uses RelayMessageResult for idiomatic switch expressions.
    /// </summary>
    public static Task StartListeningAsync(this RelayClient relay,
        Action<RelayMessageResult> onMessage,
        Action<string>? onError = null)
    {
        return relay.StartListeningRawAsync(result =>
        {
            if (result.TryGetValue(out var message, out var error))
            {
                onMessage(message.ToResult());
            }
            else if (onError != null && error != null)
            {
                onError(error);
            }
        });
    }
}

#endregion

#region Shareable Extensions

/// <summary>
/// Extension methods for encoding shareable Nostr identifiers.
/// </summary>
public static class Shareable
{
    /// <summary>
    /// Encodes an event as a shareable nevent bech32 string.
    /// </summary>
    public static string ToNEvent(
        EventIdT eventId,
        IEnumerable<string>? relays = null,
        AuthorIdT? author = null,
        Kind? kind = null)
    {
        var relayList = (relays ?? []).ToFSharpList();
        var authorOption = author is not null
            ? FSharpOption<AuthorIdT>.Some(author)
            : FSharpOption<AuthorIdT>.None;
        var kindOption = kind.HasValue
            ? FSharpOption<Kind>.Some(kind.Value)
            : FSharpOption<Kind>.None;

        return ShareableModule.ToNEvent(eventId, relayList, authorOption, kindOption);
    }

    /// <summary>
    /// Encodes a public key as a shareable npub bech32 string.
    /// </summary>
    public static string ToNPub(AuthorIdT author)
        => ShareableModule.ToNPub(author);

    /// <summary>
    /// Encodes a secret key as a shareable nsec bech32 string.
    /// </summary>
    public static string ToNSec(SecretKeyT secret)
        => ShareableModule.ToNSec(secret);

    /// <summary>
    /// Encodes an event ID as a shareable note bech32 string.
    /// </summary>
    public static string ToNote(EventIdT eventId)
        => ShareableModule.ToNote(eventId);

    /// <summary>
    /// Encodes a profile as a shareable nprofile bech32 string.
    /// </summary>
    public static string ToNProfile(AuthorIdT author, IEnumerable<string>? relays = null)
    {
        var relayList = (relays ?? []).ToFSharpList();
        return ShareableModule.ToNProfile(author, relayList);
    }
}

#endregion

#region Filter Builder Extensions

/// <summary>
/// Extension methods for building subscription filters in a fluent style.
/// </summary>
public static class Filter
{
    /// <summary>
    /// Creates a filter for all notes.
    /// </summary>
    public static SubscriptionFilter All => FilterModule.all;

    /// <summary>
    /// Creates a filter for notes (kind 1).
    /// </summary>
    public static SubscriptionFilter Notes(this SubscriptionFilter filter)
        => FilterModule.notes(filter);

    /// <summary>
    /// Creates a filter for metadata events.
    /// </summary>
    public static SubscriptionFilter Metadata(this SubscriptionFilter filter)
        => FilterModule.metadata(filter);

    /// <summary>
    /// Creates a filter for contact list events.
    /// </summary>
    public static SubscriptionFilter Contacts(this SubscriptionFilter filter)
        => FilterModule.contacts(filter);

    /// <summary>
    /// Creates a filter for encrypted messages.
    /// </summary>
    public static SubscriptionFilter EncryptedMessages(this SubscriptionFilter filter)
        => FilterModule.encryptedMessages(filter);

    /// <summary>
    /// Filters events created since the specified time.
    /// </summary>
    public static SubscriptionFilter Since(this SubscriptionFilter filter, DateTime since)
        => FilterModule.since(since, filter);

    /// <summary>
    /// Filters events created until the specified time.
    /// </summary>
    public static SubscriptionFilter Until(this SubscriptionFilter filter, DateTime until)
        => FilterModule.until(until, filter);

    /// <summary>
    /// Limits the number of events returned.
    /// </summary>
    public static SubscriptionFilter Limit(this SubscriptionFilter filter, int count)
        => FilterModule.limit(count, filter);

    /// <summary>
    /// Filters events by authors.
    /// </summary>
    public static SubscriptionFilter ByAuthors(this SubscriptionFilter filter,
        params AuthorIdT[] authors)
        => FilterModule.authors(authors.ToFSharpList(), filter);

    /// <summary>
    /// Filters events by authors.
    /// </summary>
    public static SubscriptionFilter ByAuthors(this SubscriptionFilter filter,
        IEnumerable<AuthorIdT> authors)
        => FilterModule.authors(authors.ToFSharpList(), filter);
}

#endregion

#region Static Helper Class

/// <summary>
/// Static helper class providing easy access to common Nostr operations.
/// </summary>
public static class Nostr
{
    /// <summary>
    /// Connects to a relay asynchronously.
    /// </summary>
    public static Task<RelayClient> ConnectAsync(string relayUrl)
        => ConnectToRelayAsync(new Uri(relayUrl));

    /// <summary>
    /// Connects to a relay asynchronously.
    /// </summary>
    public static Task<RelayClient> ConnectAsync(Uri relayUri)
        => ConnectToRelayAsync(relayUri);

    /// <summary>
    /// Creates a new random secret key.
    /// </summary>
    public static SecretKeyT CreateRandomKey()
        => SecretKey.CreateRandom();

    /// <summary>
    /// Creates an unsigned note event.
    /// </summary>
    public static Event.UnsignedEvent CreateNote(string content)
        => Event.CreateNote(content);

    /// <summary>
    /// Signs an unsigned event with the provided secret key.
    /// </summary>
    public static EventT SignEvent(SecretKeyT secretKey, Event.UnsignedEvent unsignedEvent)
        => Event.Sign(secretKey, unsignedEvent);

    /// <summary>
    /// Serializes an event to JSON.
    /// </summary>
    public static string SerializeEvent(EventT signedEvent)
        => Event.Serialize(signedEvent);

}

#endregion