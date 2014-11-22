namespace Endorphin.Core

open System
open System.Reactive.Linq
open System.Threading

module ObservableExtensions =
    type Observable with
        static member zipn (sources : IObservable<'a> list) =
            Observable.Zip(List.toSeq sources)

        static member zip2 (source1 : IObservable<'a>, source2 : IObservable<'b>) =
            Observable.Zip(source1, source2, fun item1 item2 -> (item1, item2))

        static member zip3 (source1 : IObservable<'a>, source2 : IObservable<'b>, source3 : IObservable<'c>) =
            Observable.Zip(source1, source2, source3, fun item1 item2 item3 -> (item1, item2, item3))
            
        static member zip4 (source1 : IObservable<'a>, source2 : IObservable<'b>, source3 : IObservable<'c>, source4 : IObservable<'d>) =
            Observable.Zip(source1, source2, source3, source4, fun item1 item2 item3 item4 -> (item1, item2, item3, item4))

        static member buffer (bufferSize : int) (source : IObservable<'a>) =
            Observable.Buffer(source, bufferSize)