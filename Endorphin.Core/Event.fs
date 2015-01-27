namespace Endorphin.Core

open System.Threading
open System

[<AutoOpen>]
module Event =
    
    /// Takes an input event where each observation is a sequence and returns a new event which fires
    /// for every element in the sequence.
    let concatSeq (source : IEvent<'a seq>) =
        let out = new Event<_>()
        source |> Event.add (fun xs -> for x in xs do out.Trigger x)
        out.Publish

    /// Maps an event onto a sequence and fires for every element of that sequence.
    let collectSeq f =
        Event.map f >> concatSeq

    /// Takes an input event and returns one which fires only every n occurances.
    let every n (source : IEvent<_>) =
        let out = new Event<_>()
        let count = ref 0 
        source.Add (fun arg -> incr count; if !count % n = 0 then out.Trigger arg)
        out.Publish