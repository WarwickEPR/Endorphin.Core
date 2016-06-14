namespace Endorphin.Instrument.PicoScope3000

module Async =

    /// Semaphore to ensure only one continuation path is followed in a raw "from continuations"
    /// computational expression
    type ContinuationGuard() =
        let sync = new obj()
        let mutable didCancel = false
        let mutable didFinish = false
        member x.IsCancelled = lock sync (fun () -> didCancel)
        member x.Cancel = lock sync (fun () -> if didCancel || didFinish then false else didCancel <- true; true)
        member x.IsFinished = lock sync (fun () -> didFinish)
        member x.Finish = lock sync (fun () -> if didFinish || didCancel then false else didFinish <- true; true)
    
    /// registers a compensation function, with a guard, which will evaluated before cancellation continuation
    /// if cancellation is requested in the current context
    let registerCompensation (guard:ContinuationGuard) (ct:System.Threading.CancellationToken) ccont compensation =
        let compensation' = match compensation with
                            | Some compensation -> compensation
                            | None -> System.OperationCanceledException()
        async { return ct.Register (fun () -> if guard.Cancel then compensation' |>  ccont) }