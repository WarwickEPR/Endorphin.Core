namespace Endorphin.Core

open System
open System.Reactive.Linq
open System.Threading

[<AutoOpen>]
module ObservableExtensions =

    /// Some extensions to the Observable type which provide a more F#-friendly API.
    type Observable with

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