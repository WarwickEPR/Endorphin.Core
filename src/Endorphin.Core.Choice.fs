// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open System

// I use the `match ... with` syntax throughout rather than the `function` syntax because the former
// gives nicer type signatures in XMLdoc and on hover in Visual Studio.

[<AutoOpen>]
/// Functions for dealing with choices between success and failure states.
module internal Choice =
    [<RequireQualifiedAccess>]
    module Choice =
        /// Return a success state of the given value.
        let succeed = Choice1Of2
        /// Return a failure state of the given value.
        let fail = Choice2Of2

        /// Map a function onto a choice type.  If the choice is in the success state, then apply the mapping,
        /// otherwise return the previous failure message.
        let map (mapping : 'T -> 'U) (state : Choice<'T, 'Error>) =
            match state with
            | Choice1Of2 s -> succeed <| mapping s
            | Choice2Of2 f -> fail f

        /// Map a function onto the error type of a choice state.  If the choice is in the error state, then
        /// apply the mapping.  Otherwise simply return the success state.
        let mapError (mapping : 'T -> 'U) (state : Choice<'Success, 'T>) =
            match state with
            | Choice1Of2 s -> succeed s
            | Choice2Of2 f -> fail <| mapping f

        /// Bind a choice-returning function onto an existing choice.  If the given state is in the success
        /// state, then apply the binding to the successful value.  If the given state is in the failure state,
        /// then simply propagate that failure.
        let bind (binder : 'T -> Choice<'U, 'Error>) state =
            match state with
            | Choice1Of2 s -> binder s
            | Choice2Of2 f -> fail f

        /// Return the success value of a state, or raise the exception stored in the error state.
        let bindOrRaise (state : Choice<'T, #exn>) =
            match state with
            | Choice1Of2 s -> s
            | Choice2Of2 f -> raise f

        /// Lift any value into being a choice success. Does the same thing as Choice.succeed, but without
        /// the implication it was a success.
        let lift value : Choice<'T, 'Error> = succeed value

        [<Sealed>]
        type Builder () =
            /// The default case for the empty computation expression is a Success with no value.
            static let zero = Choice1Of2 ()

            /// Bind a new choice onto the global state.
            member __.Bind (state : Choice<'T, 'Error>, binder) : Choice<'U, 'Error> =
                bind binder state

            /// Return a non-choice value as a choice.
            member __.Return value : Choice<'T, 'Error> =
                lift value

            /// Return a choice value from a computational expression.
            member __.ReturnFrom state : Choice<'T, 'Error> =
                state

            /// Get the zero value of the choice computational expression.
            member __.Zero () : Choice<unit, 'Error> =
                zero

            /// Delay performing a function inside a choice computational expression.
            member __.Delay generator : Choice<'T, 'Error> =
                generator ()

            /// Combine a value with the global state.  The global state must be Choice<unit, _> for this
            /// to work.
            member __.Combine (state, value) : Choice<'T, 'Error> =
                match state with
                | Choice1Of2 () -> value
                | Choice2Of2 f  -> fail f

    /// Success or failure semantics for Choice types.  These function simply as aliases for Choice1Of2
    /// and Choice2Of2.
    let (|Success|Failure|) = function
        | Choice1Of2 s -> Success s
        | Choice2Of2 f -> Failure f

    /// Create the choice computational expression.
    let choice = new Choice.Builder ()

[<AutoOpen>]
/// Functions for dealing with option types.
module internal Option =
    [<RequireQualifiedAccess>]
    module Option =
        /// Lift a value into an option type.
        let lift value = Some value

        [<Sealed>]
        type Builder () =
            /// The default case for the empty computation expression is a Some with no value.
            static let zero = Some ()

            /// Bind a new option onto the global state.
            member __.Bind (state, binder) =
                Option.bind binder state

            /// Return a non-option value as an option.
            member __.Return value =
                lift value

            /// Return an option value from a computational expression.
            member __.ReturnFrom state =
                state

            /// Get the zero value of the option computational expression.
            member __.Zero () =
                zero

            /// Delay performing a function inside an option computational expression.
            member __.Delay generator =
                generator ()

            /// Combine a value with the global state. The global state must be Option<unit> for this
            /// to work.
            member __.Combine (state, value : _ option) =
                match state with
                | Some () -> value
                | None    -> None

    /// Create the choice computational expression.
    let maybe = new Option.Builder ()
