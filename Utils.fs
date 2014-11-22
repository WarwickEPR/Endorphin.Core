namespace Endorphin.Core

open NationalInstruments.VisaNS
open System
open System.Threading
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open System.Reactive.Linq
open Endorphin.Core.ObservableExtensions

module Utils =

    type Agent<'T> = MailboxProcessor<'T>

    type MessageBasedSession with
        /// <summary>
        /// Returns an asynchronous computation which reads the next string from an NI VISA
        /// device session.
        /// </summary>
        /// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
        /// which represents the NI VISA message-based instrument control session.</param>
        member session.ReadAsync() =
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            Async.FromBeginEnd(
                (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
                session.EndReadString)

        /// <summary>
        /// Returns an asynchronous computation which writes a string message to an NI VISA
        /// device session.
        /// </summary>
        /// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
        /// which represents the NI VISA message-based instrument control session.</param>
        /// <param name="message">The message to be written.</param>
        member session.WriteAsync message = 
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            Async.FromBeginEnd(
                (fun (callback, state) -> session.BeginWrite(message, callback, state)),
                session.EndWrite)

        /// <summary>
        /// Returns an asynchronous computation which first writes a string message to an NI
        /// VISA device session and then awaits a response to that message.
        /// </summary>
        /// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
        /// which represents the NI VISA message-based instrument control session.</param>
        /// <param name="message">The message to be written.</param>
        member session.QuerryAsync message =
            async {
                // The cancellation handler ensures that no outstanding asynchronous reads or writes
                // are still on-going when a query is cancelled and that the device buffers are cleared.
                use! cancellationHandler = Async.OnCancel(fun() -> 
                    session.Terminate()
                    session.Clear())
                do! session.WriteAsync message
                return! session.ReadAsync() }

    /// <summary>
    /// Partial active pattern which returns the list of matches (excluding the complete pattern)
    /// for a regular expression.
    /// </summary>
    /// <param name="regex">The regular expression string.</param>
    /// <param name="str">The string which needs to be matched.</param>
    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    /// <summary>
    /// Partial active pattern which matches strings which can be parsed as 32-bit integers and
    /// returns the corresponding integer.
    /// </summary>
    /// <param name="str">The string to be parsed.</param>
    let (|ParseInteger|_|) str =
        try
            Some(Int32.Parse(str))
        with _ -> None

    /// <summary>
    /// Partial active pattern which matches strings which can be parsed as double-precision
    /// floating point numbers and returns the corresponding number.
    /// </summary>
    /// <param name="str">The string to be parsed.</param>
    let (|ParseFloat|_|) str =
        try
            Some(Double.Parse(str))
        with _ -> None

    /// <summary>
    /// Partial active pattern which mathes strings which are either "1" or "0", returning either
    /// true or false respectively.
    /// </summary>
    let (|ParseIntegerBool|_|) =
        function
        | "0" -> Some(false)
        | "1" -> Some(true)
        | _ -> None

    /// <summary>
    /// Extensions methods for System.Threading.Synchronization context as described in the following
    /// blog post: http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx
    /// </summary>
    type SynchronizationContext with
        
        /// <summary>
        /// A standard helper extension method to raise an event on the GUI thread
        /// </summary>
        member syncContext.RaiseEvent (event: Event<_>) args =
            syncContext.Post((fun _ -> event.Trigger args), state=null)
        
        /// <summary>
        /// A standard helper extension method to capture the current synchronization context.
        /// If none is present, use a context that executes work in the thread pool.
        /// </summary>
        static member CaptureCurrent () =
            match SynchronizationContext.Current with
            | null -> new SynchronizationContext()
            | ctxt -> ctxt