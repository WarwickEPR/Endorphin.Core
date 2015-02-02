namespace Endorphin.Core

open System
open System.Reactive.Linq

[<AutoOpen>]
module ObservableExtensions =

    /// Some extensions to the Observable type which provide a more F#-friendly API.
    type Observable with
    
        /// Creates an IObservable from the supplied source event and parameters. The OnCompleted event is fired when an element
        /// in the sequence satisfies the specified predicate. The OnError event fires if an error selector is specified and it
        /// returns a Some value for some element in the sequence.
        static member CreateFromEvent 
           (source : IEvent<'T>,
            completedPredicate : 'T -> bool,
            ?errorSelector : 'T -> exn option) =

            Observable.Create(fun (observer : IObserver<_>) ->
                // subscribe to the source event and trigger the OnNext event when it occurs
                let sourceSub =  
                    source |> Observable.subscribe (fun e ->
                        observer.OnNext e
                        // also trigger OnCompleted or OnError if appropriate
                        if completedPredicate e then observer.OnCompleted()
                        elif errorSelector.IsSome then 
                            match errorSelector.Value e with
                            | Some exn -> observer.OnError exn
                            | None -> ())
                             
                // when the IObserver is disposed, cancel the subscriptions
                { new System.IDisposable with
                    member __.Dispose() =
                        sourceSub.Dispose() })

        /// Zips a list of n observables into a single observable where each element is of type System.Collections.Generic.IList.
        static member zipn (sources : IObservable<'a> list) =
            Observable.Zip(List.toSeq sources)

        /// Zips a pair of observables into a single observable where each element is a tuple.
        static member zip2 (source1 : IObservable<'a>, source2 : IObservable<'b>) =
            Observable.Zip(source1, source2, fun item1 item2 -> (item1, item2))

        /// Zips three observables into a single observable where each element is a tuple.
        static member zip3 (source1 : IObservable<'a>, source2 : IObservable<'b>, source3 : IObservable<'c>) =
            Observable.Zip(source1, source2, source3, fun item1 item2 item3 -> (item1, item2, item3))
            
        /// Zips four observables into a single observable where each element is a tuple.
        static member zip4 (source1 : IObservable<'a>, source2 : IObservable<'b>, source3 : IObservable<'c>, source4 : IObservable<'d>) =
            Observable.Zip(source1, source2, source3, source4, fun item1 item2 item3 item4 -> (item1, item2, item3, item4))

        /// Buffers an observable into elements of type System.Colllections.Generic.IList with the specified buffer size.
        static member buffer (bufferSize : int) (source : IObservable<'a>) =
            Observable.Buffer(source, bufferSize)
    
    type IObservable<'T> with
        
        /// Returns an workflow which asynchronously awaits the first element of an observable sequence.
        member observable.AwaitFirstAsync() =
            observable.FirstAsync()
            |> Async.AwaitObservable

        /// Returns an workflow which asynchronously awaits the first element of an observable sequence which matches the specified
        /// predicate.
        member observable.AwaitFirstAsync predicate =
            observable.FirstAsync (Func<_, _> predicate)
            |> Async.AwaitObservable

        /// Returns an workflow which asynchronously awaits the last element of an observable sequence.
        member observable.AwaitLastAsync() =
            observable.LastAsync()
            |> Async.AwaitObservable
            
        /// Returns an workflow which asynchronously awaits the last element of an observable sequence which matches the specified
        /// predicate.
        member observable.AwaitLastAsync predicate =
            observable.LastAsync (Func<_, _> predicate)
            |> Async.AwaitObservable
