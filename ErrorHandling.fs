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

    /// Fold an array of Choice<'T, 'Error> into a single Choice<'T [], 'EState>, depending on a
    /// folding function to fold the errors into the desried form.
    let private foldChoices (folder : 'EState -> 'Error -> 'EState)
                            (state  : 'EState)
                            (arr    : Choice<'T, 'Error> [])
                            : Choice<'T [], 'EState> =
        let rec loop (arr : Choice<_,_> []) index typeacc erroracc =
            if index = arr.Length then
                if erroracc = state then
                    succeed typeacc
                else
                    fail erroracc

            else
                match arr.[index] with
                | Success s -> loop arr (index+1) (Array.append typeacc [|s|]) erroracc
                | Failure f -> loop arr (index+1) typeacc (folder erroracc f)
        loop arr 0 [||] state

    /// Compose a sequence of AsyncChoice workflows into a single workflow which runs all of them in
    /// parallel, and folds the errors into a single one using a fold function and an initial state.
    let Parallel state (fold : 'EState -> 'Error -> 'EState) (sequence : AsyncChoice<'T, 'Error> seq)
                 : AsyncChoice<'T [], 'EState> =
        async {
            let! sequence' = sequence |> Async.Parallel
            return foldChoices fold state sequence'
        }