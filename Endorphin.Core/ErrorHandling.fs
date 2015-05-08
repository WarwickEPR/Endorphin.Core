namespace ExtCore.Control

[<AutoOpen>]
module ErrorHandling =
    let succeed = Choice1Of2
    let fail = Choice2Of2

    let (|Success|Failure|) =
        function
        | Choice1Of2 s -> Success s
        | Choice2Of2 f -> Failure f

    type AsyncChoiceReplyChannel<'T, 'Error> = AsyncReplyChannel<Choice<'T, 'Error>>

    type AsyncChoiceBuilder with
        member __.Bind (choice : Choice<'T, 'Error>, binder : 'T -> AsyncChoice<'U, 'Error>) =
            async {
                match choice with
                | Success s -> return! binder s
                | Failure f -> return fail f }

        member __.Bind (asyncValue : Async<'T>, binder : 'T -> AsyncChoice<'U, 'Error>) =
            async {
                let! value = asyncValue
                return! binder value }

        member __.ReturnFrom (asyncValue : Async<'T>) =
            async {
                let! value = asyncValue
                return succeed value }

        member __.ReturnFrom (choice : Choice<'T, 'Error>) =
            choice |> async.Return