// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open System
open System.Reactive.Linq

/// Helper functions for manipulating creating and manipulating a ring buffer.
module internal RingBuffer =
    type RingBuffer<'T> =   
        { mutable StartIndex : int
          mutable FillCount  : int
          Data               : 'T array }

    /// Creates an empty ring buffer of the specified length.
    let create length =
        { StartIndex = 0
          FillCount = 0
          Data = Array.zeroCreate length }
    
    /// Returns the first index of the first element in the ring buffer.
    let private startIndex buffer = buffer.StartIndex

    /// Returns the number of elements currently stored in the ring buffer.
    let fillCount buffer = buffer.FillCount

    /// Returns the length of the buffer.
    let length buffer = buffer.Data.Length

    /// Indicates whether the buffer is full or not.
    let private isFilled buffer = (fillCount buffer = length buffer)

    /// Enumerates the elements currently in the buffer.
    let enumerate buffer = seq { for i in 0 .. fillCount buffer - 1 -> buffer.Data.[(startIndex buffer + i) % length buffer] }

    /// Pushes the element to the buffer by mutating it and returns the buffer
    let push x buffer =
        buffer.Data.[(startIndex buffer + fillCount buffer) % (length buffer)] <- x
        buffer.StartIndex <- 
            if isFilled buffer
            then startIndex buffer + 1
            else startIndex buffer
        buffer.FillCount  <-
            if isFilled buffer
            then fillCount buffer
            else fillCount buffer + 1
        buffer

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Event =

    /// Takes an input event where each observation is a sequence and returns a new event which fires
    /// for every element in the sequence.
    let concatSeq source =
        let output = new Event<_>()
        Event.add (fun xs -> for x in xs do output.Trigger x) source
        output.Publish

    /// Maps an event onto a sequence and fires for every element of that sequence.
    let collectSeq f = Event.map f >> concatSeq

    /// Builds a new event whose elements are the result of applying the given mapping to each element
    /// in the source event and its zero-based integer index.
    let mapi mapping =
        Event.scan (fun (i, x') x -> (i + 1, mapping i x)) (0, Unchecked.defaultof<_>)
        >> Event.map snd

    /// Builds a new event which fires on the first occurrence of the given event and  every n-th
    /// thereafter.
    let every n =
        mapi (fun i x -> (i % n = 0, x))
        >> Event.choose (fun (isNth, x) -> if isNth then Some x else None)

    /// Build a new event, collecting the elements of the source event into a buffer of the specified
    /// size. At each occurrence, the output event is the contents of the buffer which contains the
    /// most recent elements up to its specified size.
    let ringBuffer size =
        Event.scan (fun buffer x -> RingBuffer.push x buffer) (RingBuffer.create size)
        >> Event.map (fun buffer -> RingBuffer.enumerate buffer)

    /// Build a new event, applying the given mapping to each element in the source event, then
    /// collecting the values into a buffer of the specified size. At each occurrence, the output
    /// event is the contents of the buffer which contains the most recent values up to its
    /// specified size.
    let ringBufferMap size mapping =
        Event.scan (fun buffer x -> RingBuffer.push (mapping x) buffer) (RingBuffer.create size)
        >> Event.map (fun buffer -> RingBuffer.enumerate buffer)

    /// Build a new event, applying the given mapping to each element in the source event and its zero-
    /// based integer index, then collecting the values into a buffer of the specified size. At each
    /// occurrence, the output event is the contents of the buffer which contains the most recent values
    /// up to its specified size.
    let ringBufferMapi size mapping =
        Event.scan (fun (i, buffer) x -> (i + 1, RingBuffer.push (mapping i x) buffer)) (0, RingBuffer.create size)
        >> Event.map (fun (i, buffer) -> RingBuffer.enumerate buffer)

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
        let output = new Event<_>()
        Observable.add (fun xs -> for x in xs do output.Trigger x) source
        output.Publish :> IObservable<_>

    /// Maps an observable onto a sequence and fires for every element of that sequence.
    let collectSeq f =
        Observable.map f >> concatSeq
    
    /// Builds a new observable whose elements are the result of applying the given mapping to each
    /// element of the input observable and its zero-based integer index.
    let mapi mapping =
        Observable.scan (fun (i, x') x -> (i + 1, mapping i x)) (0, Unchecked.defaultof<_>)
        >> Observable.map snd

    /// Builds a new event which fires on the first occurrence of the given event and  every n-th
    /// thereafter.
    let every n =
        mapi (fun i x -> (i % n = 0, x))
        >> Observable.choose (fun (isNth, x) -> if isNth then Some x else None)

    /// Build a new observable, collecting the elements of the source observable into a buffer of the
    /// specified size. At each occurrence, the output observable  is the contents of the buffer which
    /// contains the most recent elements up to its specified size.
    let ringBuffer size =
        Observable.scan (fun buffer x -> RingBuffer.push x buffer) (RingBuffer.create size)
        >> Observable.map (fun buffer -> RingBuffer.enumerate buffer)

    /// Build a new observable, applying the given mapping to each element in the source observable,
    /// then collecting the values into a buffer of the specified size. At each occurrence, the output
    /// observable is the contents of the buffer which contains the most recent values up to its
    /// specified size.
    let ringBufferMap size mapping =
        Observable.scan (fun buffer x -> RingBuffer.push (mapping x) buffer) (RingBuffer.create size)
        >> Observable.map (fun buffer -> RingBuffer.enumerate buffer)

    /// Build a new observable, applying the given mapping to each element in the source observable and
    /// its zero-based integer index, then collecting the values into a buffer of the specified size. At
    /// each occurrence, the output event is the contents of the buffer which contains the most recent
    /// values up to its specified size.
    let ringBufferMapi size mapping =
        Observable.scan (fun (i, buffer) x -> (i + 1, RingBuffer.push (mapping i x) buffer)) (0, RingBuffer.create size)
        >> Observable.map (fun (i, buffer) -> RingBuffer.enumerate buffer)

    /// Creates an observable from a source event which emits notifications that either hold a value,
    /// completion or error.
    let fromNotificationEvent (source : IEvent<Notification<'T>>) =
        Observable.Create(fun (observer : IObserver<_>) ->
            source.Subscribe(fun notification ->
                match notification with
                | Notification.Next value -> observer.OnNext value
                | Notification.Completed  -> observer.OnCompleted()
                | Notification.Error exn  -> observer.OnError exn))