namespace ExtCore.Control

[<AutoOpen>]
module ErrorHandling =
    /// Wraps a value as a success represented by a Choice<'T, 'Error>.
    let succeed = Choice1Of2
    /// Wraps a value as a failure represented by a Choice<'T, 'Error>.
    let fail = Choice2Of2

    /// Conventional definition for pattern-matching Choice<'T, 'Error> as Success or Failure.
    let (|Success|Failure|) =
        function
        | Choice1Of2 s -> Success s
        | Choice2Of2 f -> Failure f

    /// A handle to a capability to reply to a PostAndReply message with either success or failure.
    type AsyncChoiceReplyChannel<'T, 'Error> = AsyncReplyChannel<Choice<'T, 'Error>>

    // Extensions to AsyncChoice builder which bind and return from Choice<'T, 'Error> and Async<'T> values.
    type AsyncChoiceBuilder with

        /// Binds an expression of type Choice<'T, 'Error>.
        member __.Bind (choice : Choice<'T, 'Error>, binder : 'T -> AsyncChoice<'U, 'Error>) =
            async {
                match choice with
                | Success s -> return! binder s
                | Failure f -> return fail f }

        /// Returns from an expression of type Choice<'T, 'Error>.
        member __.ReturnFrom (choice : Choice<'T, 'Error>) =
            choice |> async.Return

[<AutoOpen>]
module Utils =
    let defer f = { new System.IDisposable with member __.Dispose() = f () }

module AsyncChoice =
    let liftAsync comp = comp |> Async.map succeed
    let liftChoice (choice : Choice<'a, 'b>) = choice |> async.Return
    let guard cond err = if cond then succeed () else fail err