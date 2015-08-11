[<AutoOpen>]
module AsyncExtensions

open System
open System.Threading

/// Standard type alias for Microsoft.FSharp.Control.MailboxProcessor agent.
type Agent<'T> = MailboxProcessor<'T>

/// MailboxProcessor extensions
type MailboxProcessor<'T> with

    /// Provides a mechanism for posting messages with an asynchronous reply channel which will call the
    /// error continuation of the caller if the failure event is fired before the agent responds to the
    /// message.
    member agent.PostAndAsyncReplyFailable (failureEvent : IObservable<exn>) buildMessage = async {
        // health warning: don't mess with this unless you know what you are doing

        // capture the current cancellation token
        let! token = Async.CancellationToken
            
        // create a an asynchronous workflow from continuations, where the specified continuation function
        // will be called when the workflow is complete, with either success, error or cancellation
        return! Async.FromContinuations(fun (cont, econt, ccont) ->
                
            // create a cancellation capability which will be used to stop listening for a message if an
            // error or cancellation occurs
            let requestCancellationCapability = new CancellationTokenSource()
                
            // start another agent internally which wil listen for the first message, indicating either success,
            // failure or cancellation
            let continuator = Agent.Start((fun (mailbox : Agent<Choice<'Reply, exn, OperationCanceledException>>) ->
                async {
                    // register a callback with the cancellation token which will post a message indicating
                    // that the request has been cancelled
                    use __ = token.Register((fun _ ->
                        let result = Choice3Of3 (new OperationCanceledException("The opeartion was cancelled."))
                        mailbox.Post result))

                    // subscribe to the provided failure event
                    use __ = failureEvent.Subscribe(fun exn -> mailbox.Post(Choice2Of3 exn))

                    // wait for the result
                    let! message = mailbox.Receive()
                        
                    match message with // and depending on the result
                    | Choice1Of3 reply -> cont reply // call the success continuation
                    | Choice2Of3 exn -> 
                        requestCancellationCapability.Cancel() // cancel the request
                        econt exn // call the failure continuation
                    | Choice3Of3 exn -> 
                        requestCancellationCapability.Cancel() // cancel the request
                        ccont exn })) // or call the cancellation continuation

            // start another asynchronous workflow which will post the supplied message to the agent and post a
            // message indicating success to the continuator agent when it receives a response
            Async.Start( async {
                let! reply = agent.PostAndAsyncReply buildMessage
                continuator.Post(Choice1Of3 reply) }, requestCancellationCapability.Token)) }

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