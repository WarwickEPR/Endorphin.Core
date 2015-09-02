namespace Endorphin.Core

open System
open System.Reactive.Linq

/// Internal support functions for the library of transformations to events and observables defined
/// below.
module internal Support = 

    let addToEvent (source : IEvent<_>) callback =
        source |> Event.add callback 

    let addToObservable (source : IObservable<_>) callback =
        source |> Observable.add callback

    let asObservable (event : IEvent<_>) =
        event :> IObservable<_>

    let concatSeq add =
        let output = new Event<_>()
        add (fun xs -> for x in xs do output.Trigger x)
        output.Publish

    let every n add =
        let output = new Event<_>()
        let mutable index = 0 
        let sync = new obj()

        add (fun x -> lock sync (fun () -> 
            index <- (index + 1) % n
            if index = 0 then output.Trigger x))

        output.Publish

    let mapi mapping add =
        let output = new Event<_>()
        let sync = new obj()
        let mutable index = 0

        add (fun x -> lock sync (fun () -> 
            output.Trigger (mapping index x)
            index <- index + 1))

        output.Publish

    let bufferMapiCountOverlapped count mapping add =
        let output = new Event<_>()
        let mutable buffer = Array.zeroCreate count
        let mutable didLoop = false
        let mutable index = 0
        let mutable bufferIndex = 0

        add (fun x -> lock buffer (fun () ->
            buffer.[bufferIndex] <- mapping index x
            index <- index + 1
            bufferIndex <- (bufferIndex + 1) % count
            
            if bufferIndex = 0 then didLoop <- true
            if didLoop then seq { for i in 0 .. count - 1 -> buffer.[(bufferIndex + i) % count] }
            else            seq { for i in 0 .. bufferIndex - 1 -> (buffer).[i] }
            |> Array.ofSeq
            |> output.Trigger))

        output.Publish

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Event =

    /// Takes an input event where each observation is a sequence and returns a new event which fires
    /// for every element in the sequence.
    let concatSeq source =
        Support.addToEvent source |> Support.concatSeq

    /// Maps an event onto a sequence and fires for every element of that sequence.
    let collectSeq f =
        Event.map f >> concatSeq

    /// Builds a new event which fires every n-th occurrence of the input event.
    let every n source =
        Support.addToEvent source |> Support.every n

    /// Builds a new event whose elements are the result of applying the given mapping to each element
    /// in the source event and its zero-based integer index.
    let mapi mapping source =
        Support.addToEvent source |> Support.mapi mapping

    /// Build a new event, applying the given mapping to each element in the source event and its zero-
    /// based integer index, then collecting the values into a buffer of the specified size. At each
    /// occurrence, the output event is the contents of the buffer which contains the most recent values
    /// up to its specified size.
    let bufferMapiCountOverlapped count mapping source =
        Support.addToEvent source |> Support.bufferMapiCountOverlapped count mapping

    /// Build a new event, applying the given mapping to each element in the source event, then
    /// collecting the values into a buffer of the specified size. At each occurrence, the output
    /// event is the contents of the buffer which contains the most recent values up to its
    /// specified size.
    let bufferMapCountOverlapped count mapping source =
        Support.addToEvent source |> Support.bufferMapiCountOverlapped count (fun _ x -> mapping x)

    /// Build a new event, collecting the elements of the source event into a buffer of the specified
    /// size. At each occurrence, the output event is the contents of the buffer which contains the
    /// most recent elements up to its specified size.
    let bufferCountOverlapped count source =
        Support.addToEvent source |> Support.bufferMapiCountOverlapped count (fun _ x -> x)

[<AutoOpen>]
module ObservableHelpers =
    type Notification<'T> =
        | Completed
        | Next of 'T
        | Error of exn

    type NotificationEvent<'T> = Event<Notification<'T>>

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Observable =

    /// Takes an input observable where each observation is a sequence and returns a new event which
    /// fires for every element in the sequence.
    let concatSeq source =
        Support.addToObservable source |> Support.concatSeq

    /// Maps an observable onto a sequence and fires for every element of that sequence.
    let collectSeq f =
        Observable.map f >> concatSeq

    /// Builds a new observable which fires every n-th occurrence of the input observable.
    let every n source =
        Support.addToObservable source |> Support.every n

    /// Builds a new observable whose elements are the result of applying the given mapping to each
    /// element of the input observable and its zero-based integer index.
    let mapi mapping source =
        Support.addToObservable source |> Support.mapi mapping

    /// Build a new observable, applying the given mapping to each element in the source observable and
    /// its zero-based integer index, then collecting the values into a buffer of the specified size. At
    /// each occurrence, the output event is the contents of the buffer which contains the most recent
    /// values up to its specified size.
    let bufferMapiCountOverlapped count mapping source =
        Support.addToObservable source |> Support.bufferMapiCountOverlapped count mapping

    /// Build a new observable, applying the given mapping to each element in the source observable,
    /// then collecting the values into a buffer of the specified size. At each occurrence, the output
    /// observable is the contents of the buffer which contains the most recent values up to its
    /// specified size.
    let bufferMapCountOverlapped count mapping source =
        Support.addToObservable source |> Support.bufferMapiCountOverlapped count (fun _ x -> mapping x)

    /// Build a new observable, collecting the elements of the source observable into a buffer of the
    /// specified size. At each occurrence, the output observable  is the contents of the buffer which
    /// contains the most recent elements up to its specified size.
    let bufferCountOverlapped count source =
        Support.addToObservable source |> Support.bufferMapiCountOverlapped count (fun _ x -> x)

    /// Creates an observable from a source event which emits notifications that either hold a value,
    /// completion or error.
    let fromNotificationEvent (source : IEvent<Notification<'T>>) =
        Observable.Create(fun (observer : IObserver<_>) ->
            source.Subscribe(fun notification ->
                match notification with
                | Notification.Next value -> observer.OnNext value
                | Notification.Completed  -> observer.OnCompleted()
                | Notification.Error exn  -> observer.OnError exn))