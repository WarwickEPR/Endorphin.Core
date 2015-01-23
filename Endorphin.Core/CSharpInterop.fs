namespace Endorphin.Core.CSharpInterop

open System.Runtime.CompilerServices

[<Extension>]
type CSharpInteropExtensions() =

    /// <summary>
    /// Helper extension method for C# interop. Use it to start an async workflow as a TPL
    /// task. The Task<'T> can then be awaited within an async C# method.
    /// </summary>
    [<Extension>]
    static member StartAsTask (comp : Async<'T>) =
        comp |> Async.StartAsTask

    [<Extension>]
    static member RunSynchronously (comp : Async<'T>) =
        comp |> Async.RunSynchronously