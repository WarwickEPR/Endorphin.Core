namespace Endorphin.Core

open System
open System.Threading

[<AutoOpen>]
module AsyncExtensions =

    /// <summary>
    /// Standard type alias for <see cref="Microsoft.FSharp.Control.MailboxProcessor" /> agent.
    /// </summary>
    type Agent<'T> = MailboxProcessor<'T>

    /// <summary>
    /// Extensions methods for System.Threading.Synchronization context as described in the following
    /// blog post: http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx
    /// </summary>
    type SynchronizationContext with
        
        /// <summary>
        /// A standard helper extension method to raise an event on the GUI thread
        /// </summary>
        member syncContext.RaiseEvent (event: Event<_>) args =
            syncContext.Post((fun _ -> event.Trigger args), state=null)
        
        /// <summary>
        /// A standard helper extension method to capture the current synchronization context.
        /// If none is present, use a context that executes work in the thread pool.
        /// </summary>
        static member CaptureCurrent () =
            match SynchronizationContext.Current with
            | null -> new SynchronizationContext()
            | ctxt -> ctxt

    /// <summary>
    /// Type alias for <see cref="System.Threading.ManualResetEvent" /> to avoid confusion between
    /// wait handles and events.
    /// </summary>
    type ManualResetHandle = ManualResetEvent

    /// <summary>
    /// Type alias for <see cref="System.Threading.CancellationTokenSource" />.
    /// </summary>
    type CancellationCapability = CancellationTokenSource

    /// <summary>
    /// Cancellation capability with generic options which must be specified at cancellation.
    /// </summary>
    type CancellationCapability<'Opts>() =
        let cancellationCapability = new CancellationCapability()
        let cancellationOptions = ref None

        member this.Cancel options =
            match !cancellationOptions with
            | None ->
                cancellationOptions := Some(options)
                cancellationCapability.Cancel()
            | Some(_) ->
                failwith "Cancellation has already been requested."

        member this.Options : 'Opts =
            match !cancellationOptions with
            | Some(options) -> options
            | None -> failwith "Cannot obtain cancellation options as cancellation has not yet been requested."

        member this.IsCancellationRequested =
            cancellationCapability.IsCancellationRequested

        member this.Token =
            cancellationCapability.Token

        interface IDisposable with
            member this.Dispose() =
                cancellationCapability.Dispose()