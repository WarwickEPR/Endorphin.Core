namespace Endorphin.Core

open System
open System.Threading

[<AutoOpen>]
module AsyncExtensions =

    /// Standard type alias for Microsoft.FSharp.Control.MailboxProcessor agent.
    type Agent<'T> = MailboxProcessor<'T>

    /// Extensions methods for System.Threading.Synchronization context as described in the following
    /// blog post: http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx
    type SynchronizationContext with
        
        /// A standard helper extension method to raise an event on the GUI thread.
        member syncContext.RaiseEvent (event: Event<_>) args =
            syncContext.Post((fun _ -> event.Trigger args), state=null)
        
        /// A standard helper extension method to capture the current synchronization context.
        /// If none is present, use a context that executes work in the thread pool.
        static member CaptureCurrent () =
            match SynchronizationContext.Current with
            | null -> new SynchronizationContext()
            | ctxt -> ctxt

    /// Type alias for System.Threading.ManualResetEvent to avoid confusion between wait handles and events.
    type ManualResetHandle = ManualResetEvent

    /// Type alias for System.Threading.CancellationTokenSource.
    type CancellationCapability = CancellationTokenSource

    /// Cancellation capability with generic options which must be specified at cancellation.
    type CancellationCapability<'T>() =
        let cancellationCapability = new CancellationCapability()
        let cancellationOptions = ref None

        /// Cancels the cancellation capability after setting the specified cancellation options. Can only be
        /// called once.
        member this.Cancel options =
            match !cancellationOptions with
            | None ->
                cancellationOptions := Some options
                cancellationCapability.Cancel()
            | Some _ ->
                failwith "Cancellation has already been requested."

        /// Returns the options argument specifed at cancellation. Throws an exception if cancellation has not
        /// taken place.
        member this.Options : 'T =
            match !cancellationOptions with
            | Some options -> options
            | None -> failwith "Cannot obtain cancellation options as cancellation has not yet been requested."

        /// Checks if cancellation has already been requested.
        member this.IsCancellationRequested =
            cancellationCapability.IsCancellationRequested

        /// Returns the cancellation token.
        member this.Token =
            cancellationCapability.Token

        interface IDisposable with
            /// Disposes of the System.Threading.CancellationCapability object.
            member this.Dispose() =
                cancellationCapability.Dispose()