namespace Endorphin.Core

open System.Threading

[<AutoOpen>]
module Utils =
    let defer f = { new System.IDisposable with member __.Dispose() = f () }

(*
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
            return foldChoices fold state sequence' } *)

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Async =
    /// Apply a mapping function inside an async workflow.
    let map mapping workflow = async {
        let! result = workflow
        return mapping result }

[<AutoOpen>]
module AsyncExtensions =
    
    /// A single-fire result channel which can be used to await a result asynchronously.
    type ResultChannel<'T>() =               
        let mutable result = None   // result is None until one is registered
        let mutable savedConts = [] // list of continuations which will be applied to the result
        let syncRoot = new obj()    // all writes of result are protected by a lock on syncRoot

        /// Record the result, starting any registered continuations.
        member channel.RegisterResult (res : 'T) =
            let grabbedConts = // grab saved continuations and register the result
                lock syncRoot (fun () ->
                    if channel.ResultAvailable then // if a result is already saved the raise an error
                        failwith "Multiple results registered for result channel."
                    else // otherwise save the result and return the saved continuations
                        result <- Some res
                        List.rev savedConts)

            // run all the grabbed continuations with the provided result
            grabbedConts |> List.iter (fun cont -> cont res)

        /// Check if a result has been registered with the channel.
        member channel.ResultAvailable = result.IsSome

        /// Wait for a result to be registered on the channel asynchronously.
        member channel.AwaitResult () = async {
            let! ct = Async.CancellationToken // capture the current cancellation token
        
            // create a flag which indicates whether a continuation has been called (either cancellation
            // or success, and protect access under a lock; the performCont function sets the flag to true
            // if it wasn't already set and returns a boolen indicating whether a continuation should run
            let performCont = 
                let mutable continued = false
                let localSync = obj()
                (fun () ->
                    lock localSync (fun () ->
                        if not continued 
                        then continued <- true ; true
                        else false))
        
            // wait for a result to be registered or cancellation to occur asynchronously
            return! Async.FromContinuations(fun (cont, _, ccont) ->
                let resOpt = 
                    lock syncRoot (fun () ->
                        match result with
                        | Some _ -> result // if a result is already set, capture it
                        | None   ->
                            // otherwise register a cancellation continuation and add the success continuation
                            // to the saved continuations
                            let reg = ct.Register(fun () -> 
                                if performCont () then 
                                    ccont (new System.OperationCanceledException("The operation was canceled.")))
                                
                            let cont' = (fun res ->
                                // modify the continuation to first check if cancellation has already been
                                // performed and if not, also dispose the cancellation registration
                                if performCont () then
                                    reg.Dispose()
                                    cont res)
                            savedConts <- cont' :: savedConts
                            None)

                // if a result already exists, then call the result continuation outside the lock
                match resOpt with
                | Some res -> cont res
                | None     -> ()) }