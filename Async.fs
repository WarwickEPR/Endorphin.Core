namespace Endorphin.Core

open System
open System.Threading

[<AutoOpen>]
module AsyncExtensions =

    /// Standard type alias for Microsoft.FSharp.Control.MailboxProcessor agent.
    type Agent<'T> = MailboxProcessor<'T>

    type Async with

        /// Creates an asynchronous workflow that will be resumed when the specified observables produces a
        /// value. The workflow will return the value produced by the observable. Taken from Fsharpx project
        /// source on GitHub: https://github.com/fsprojects/fsharpx/blob/7246e314e3fdeae07f3465b9126f2bc22faa0cd5/src/FSharpx.Core/Observable.fs.
        /// The Fsharpx NuGet version has not been updated with a fix regarding workflow cancellation so using
        /// the GitHub implementation for now. TODO: get the Fsharpx library once the NuGet version is up to
        /// date and remove this method. Note: this method has been modifiedin order to prevent a bug which
        /// occurs with observables with replay semantics.
        static member AwaitObservable(observable : IObservable<'T1>) =
            let synchronize f = 
                let ctx = System.Threading.SynchronizationContext.Current 
                f (fun g ->
                    let nctx = System.Threading.SynchronizationContext.Current 
                    if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g()), null)
                    else g() )
    
            let continued = ref false
            let continuedLock = new obj()
            let removeObj : IDisposable option ref = ref None
            let removeLock = new obj()
            let setRemover r = 
                lock removeLock (fun () ->  removeObj := Some r)
            let remove() =
                lock removeLock (fun () ->
                    match !removeObj with
                    | Some d -> 
                        removeObj := None
                        d.Dispose()
                    | None   -> ())
            synchronize (fun f ->
            let workflow =
                Async.FromContinuations((fun (cont,econt,ccont) ->
                    let rec finish cont value =
                        remove()
                        f (fun () -> lock continuedLock (fun () ->
                            if not !continued then
                                cont value
                                continued := true))
                    let observer = 
                        observable.Subscribe
                            ({ new IObserver<_> with
                                member __.OnNext(v) = finish cont v
                                member __.OnError(e) = finish econt e
                                member __.OnCompleted() =
                                    let msg = "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."
                                    finish ccont (new System.OperationCanceledException(msg)) })
                    lock continuedLock (fun () -> if not !continued then setRemover observer else observer.Dispose())
                    () ))
            async {
                let! cToken = Async.CancellationToken
                let token : CancellationToken = cToken
                use __ = token.Register((fun _ -> remove()), null)
                return! workflow
            })

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
        member __.Cancel options =
            match !cancellationOptions with
            | None ->
                cancellationOptions := Some options
                cancellationCapability.Cancel()
            | Some _ ->
                failwith "Cancellation has already been requested."

        /// Returns the options argument specifed at cancellation. Throws an exception if cancellation has not
        /// taken place.
        member __.Options : 'T =
            match !cancellationOptions with
            | Some options -> options
            | None -> failwith "Cannot obtain cancellation options as cancellation has not yet been requested."

        /// Checks if cancellation has already been requested.
        member __.IsCancellationRequested =
            cancellationCapability.IsCancellationRequested

        /// Returns the cancellation token.
        member __.Token =
            cancellationCapability.Token

        interface IDisposable with
            /// Disposes of the System.Threading.CancellationCapability object.
            member __.Dispose() =
                cancellationCapability.Dispose()