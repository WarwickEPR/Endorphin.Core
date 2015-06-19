[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Endorphin.Core.Event

/// Takes an input event where each observation is a sequence and returns a new event which fires
/// for every element in the sequence.
let concatSeq (source : IEvent<_>) =
    let out = new Event<_>()
    source |> Event.add (fun xs -> for x in xs do out.Trigger x)
    out.Publish

/// Maps an event onto a sequence and fires for every element of that sequence.
let collectSeq f =
    Event.map f >> concatSeq

let private incrMod n cell =
    cell := (!cell + 1) % n 

/// Takes an input event and returns one which fires only every n occurances.
let every n (source : IEvent<_>)  =
    let out = new Event<_>()
    let index = ref 0 
    source |> Event.add (fun x -> lock index (fun () -> incrMod n index; if !index = 0 then out.Trigger x))
    out.Publish

let mapi mapping (source : IEvent<_>)  =
    let out = new Event<_>()
    let index = ref 0
    source |> Event.add (fun x -> lock index (fun () -> out.Trigger (mapping !index x)) ; incr index)
    out.Publish

let windowMap size mapping (source : IEvent<_>) =
    let out = new Event<_>()
    let buffer = ref (Array.zeroCreate size)
    let bufferIndex = ref 0
    let didLoop = ref false
    source.Add (fun x ->
        lock buffer (fun () ->
            (!buffer).[!bufferIndex] <- mapping x
            incrMod size bufferIndex
            if !bufferIndex = 0 then didLoop := true
            if !didLoop then
                seq { for i in 0 .. size - 1 ->
                        (!buffer).[(!bufferIndex + i) % size] }
                |> Array.ofSeq
                |> out.Trigger))
    out.Publish

let windowMapi size mapping (source : IEvent<_>) =
    let out = new Event<_>()
    let buffer = ref (Array.zeroCreate size)
    let bufferIndex = ref 0
    source.Add (fun x ->
        lock buffer (fun () ->
            (!buffer).[!bufferIndex % size] <- mapping !bufferIndex x
            incr bufferIndex
            if !bufferIndex > size then
                seq { for i in 0 .. size - 1 ->
                        (!buffer).[(!bufferIndex + i) % size] }
                |> Array.ofSeq
                |> out.Trigger))
    out.Publish

let windowMapSeq size mapping (source : IEvent<_>) =
    let out = new Event<_>()
    let buffer = ref (Array.zeroCreate size)
    let bufferIndex = ref 0
    let didLoop = ref false
    source.Add (fun x ->
        lock buffer (fun () ->
            for el in mapping x do
                (!buffer).[!bufferIndex] <- el
                incrMod size bufferIndex
                if !bufferIndex = 0 then didLoop := true
            if !didLoop then
                seq { for i in 0 .. size - 1 ->
                        (!buffer).[(!bufferIndex + i) % size] }
                |> Array.ofSeq
                |> out.Trigger))
    out.Publish

let windowMapiSeq size mapping (source : IEvent<_>) =
    let out = new Event<_>()
    let buffer = ref (Array.zeroCreate size)
    let bufferIndex = ref 0
    source.Add (fun x ->
        lock buffer (fun () ->
            for el in mapping x do
                (!buffer).[!bufferIndex] <- el
                incr bufferIndex
            if !bufferIndex > size then
                seq { for i in 0 .. size - 1 ->
                        (!bufferIndex + i - size), (!buffer).[(!bufferIndex + i) % size] }
                |> Array.ofSeq
                |> out.Trigger))
    out.Publish