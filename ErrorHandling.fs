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

module AsyncChoice =
    let liftAsync comp = comp |> Async.map succeed
    let liftChoice (choice : Choice<'a, 'b>) = choice |> async.Return

module Choice =
    /// Fold a list of choices into a single choice based on the two folds specified
    let foldChoice (foldType        : 'TState -> 'T -> 'TState)
                   (startTypeState  : 'TState)
                   (foldError       : 'eState -> 'e -> 'eState)
                   (startErrorState : 'eState)
                   (choiceList      : Choice<'T, 'e> list)
                                    : Choice<'TState, 'eState> =
        let rec loop list typeState errorState =
            match list with
            | [] ->
                if errorState = startErrorState then
                    succeed typeState
                else
                    fail errorState
            | hd :: tl ->
                match hd with
                | Success s -> loop tl (foldType typeState s) errorState
                | Failure f -> loop tl typeState (foldError errorState f)
        loop choiceList startTypeState startErrorState