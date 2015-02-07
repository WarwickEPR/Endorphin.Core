namespace Endorphin.Core.CSharpInterop

open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type CSharpInteropExtensions() =

    [<Extension>]
    /// Converts a muatble C# dictionary to an immutable F# map.
    static member AsFSharpMap (dictionary : Dictionary<'K, 'V>) =
        (dictionary :> seq<_>)
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    [<Extension>]
    /// Converts a sequence into an F# set.
    static member AsFSharpSet (sequence : 'T seq) =
        Set.ofSeq sequence

    [<Extension>]
    /// Helper extension method for C# interop. Use it to start an async workflow as a TPL
    /// task. The Task<'T> can then be awaited within an async C# method.
    static member StartAsTask (comp : Async<'T>) =
        comp |> Async.StartAsTask

    [<Extension>]
    /// Helper extension method for C# interop. Runs an async workflow synchronously.
    static member RunSynchronously (comp : Async<'T>) =
        comp |> Async.RunSynchronously