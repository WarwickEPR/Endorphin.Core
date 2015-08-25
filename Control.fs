﻿namespace ExtCore.Control

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

module Choice =
    /// Make a Choice<'a,'b> into a Choice<'a option,'b>.
    let liftInsideOption = function
        | Success s -> succeed (Some s)
        | Failure f -> fail f

    /// Map a list, where each mapping results in a choice, binding the result of each mapping.
    let mapList map list =
        let rec loop acc list = choice {
            match list with
            | [] -> return List.rev acc
            | hd :: tl ->
                let! hd' = map hd
                return! loop (hd' :: acc) tl }
        loop [] list

[<AutoOpen>]
module Utils =
    let defer f = { new System.IDisposable with member __.Dispose() = f () }

module AsyncChoice =
    let liftAsync comp = comp |> Async.map succeed
    let liftChoice (choice : Choice<'a, 'b>) = choice |> async.Return
    let guard cond err = if cond then succeed () else fail err   

    /// Ignore any success value from an AsyncChoice workflow, but still propagate the failure
    let ignore input = async {
        let! choice = input
        return
            match choice with
            | Success s -> succeed ()
            | Failure f -> fail f }

    /// Build a new AsyncChoice workflow whose Choice1Of2 is the result of apply the mapping function
    /// to the Choice1Of2 values of the two passed workflows, propagating any failures if necessary.
    let map2 (mapping : 'A -> 'B -> 'C) a b : AsyncChoice<'C, 'Error> = asyncChoice {
        let! a' = a
        let! b' = b
        return mapping a' b' }

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
            return foldChoices fold state sequence' }

[<AutoOpen>]
module AsyncExtensions =
    /// Contains the result of an Async workflow.
    type AsyncResult<'T> =
        | AsyncSuccess of result : 'T
        | AsyncFailure of error : exn
        | AsyncCancellation of exn : System.OperationCanceledException

    type Async<'T> with
        /// Starts an Async workflow on the thread pool with the specified cancellation token, returning
        /// a handle which can be used to await its result. 
        static member StartWithResultHandle (workflow, ?cancellationToken) =    
            Async.FromContinuations(fun (cont, econt, ccont) -> 
                match cancellationToken with
                | Some ct ->
                    Async.StartWithContinuations(workflow,
                        AsyncSuccess >> cont,
                        AsyncFailure >> cont,
                        AsyncCancellation >> cont,
                        ct)
                | None ->
                    Async.StartWithContinuations(workflow,
                        AsyncSuccess >> cont,
                        AsyncFailure >> cont,
                        AsyncCancellation >> cont))