namespace Endorphin.Core

open System

// I use the `match ... with` syntax throughout rather than the `function` syntax because the former
// gives nicer type signatures in XMLdoc and on hover in Visual Studio.

[<AutoOpen>]
module ChoiceAuto =
    let (|Success|Failure|) = function
        | Choice1Of2 s -> Success s
        | Choice2Of2 f -> Failure f

[<RequireQualifiedAccess>]
/// Functions for dealing with choices between success and failure states.
module Choice =
    /// Return a success state of the given value.
    let succeed = Choice1Of2
    /// Return a failure state of the given value.
    let fail = Choice2Of2

    /// Map a function onto a choice type.  If the choice is in the success state, then apply the mapping,
    /// otherwise return the previous failure message.
    let map (mapping : 'T -> 'U) (state : Choice<'T, 'Error>) = 
        match state with
        | Success s -> succeed <| mapping s
        | Failure f -> fail f

    /// Map a function onto the error type of a choice state.
    let mapError (mapping : 'T -> 'U) (state : Choice<'Success, 'T>) =
        match state with
        | Success s -> succeed s
        | Failure f -> fail <| mapping f

    /// Bind a choice-returning function onto an existing choice.  If the given state is in the success
    /// state, then apply the binding to the successful value.  If the given state is in the failure state,
    /// then simply propagate that failure.
    let bind (binder : 'T -> Choice<'U, 'Error>) state =
        match state with
        | Success s -> binder s
        | Failure f -> fail f

    /// Return the success value of a state, or raise its exception.
    let bindOrRaise (state : Choice<'T, #exn>) =
        match state with
        | Success s -> s
        | Failure f -> raise f

    /// Lift any value into being a choice success. Does the same thing as Choice.succeed, but without
    /// necessarily the connotation that it was a success.
    let lift value : Choice<'T, 'Error> = succeed value

[<Sealed>]
type ChoiceBuilder () =
    /// The default case for the empty computation expression is a Success with no value.
    static let zero = Choice1Of2 ()

    member __.Bind (state : Choice<'T, 'Error>, binder) : Choice<'U, 'Error> =
        Choice.bind binder state

    member __.Return value : Choice<'T, 'Error> =
        Choice.lift value

    member __.ReturnFrom state : Choice<'T, 'Error> =
        state

    member __.Zero () : Choice<unit, 'Error> =
        zero

    member __.Delay generator : Choice<'T, 'Error> =
        generator ()

    member __.Combine (state, value) : Choice<'T, 'Error> =
        match state with
        | Success () -> value
        | Failure f  -> Choice.fail f

[<AutoOpen>]
module ChoiceComputationExpression =
    let choice = new ChoiceBuilder ()