namespace Endorphin.Core.CSharpInterop

open System.Runtime.CompilerServices

[<Extension>]
type CSharpInteropExtensions() =

    /// Helper extension method for C# interop. Use it to start an async workflow as a TPL
    /// task. The Task<'T> can then be awaited within an async C# method.
    [<Extension>]
    static member StartAsTask (comp : Async<'T>) =
        comp |> Async.StartAsTask

    /// Helper extension method for C# interop. Runs an async workflow synchronously.
    [<Extension>]
    static member RunSynchronously (comp : Async<'T>) =
        comp |> Async.RunSynchronously