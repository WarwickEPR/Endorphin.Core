namespace Endorphin.Core

[<AutoOpen>]
module Event =
    open System.Threading
    open System

    let concatSeq (source : IEvent<'a seq>) =
        let out = new Event<_>()
        source |> Event.add (fun xs -> for x in xs do out.Trigger x)
        out.Publish

    let collectSeq f =
        Event.map f >> concatSeq

    let every n (source : IEvent<_>) =
        let out = new Event<_>()
        let count = ref 0 
        source.Add (fun arg -> incr count; if !count % n = 0 then out.Trigger arg)
        out.Publish