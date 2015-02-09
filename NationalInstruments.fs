namespace Endorphin.Core

open NationalInstruments.VisaNS

[<AutoOpen>]
module NationalInstruments =

    /// Extensions for NationalInstruments.VisaNS.MessageBasedSession which wrap Begin-End pattern
    /// methods into asynchronous workflows.
    type MessageBasedSession with
        
        /// Returns an asynchronous computation which reads the next string from an NI VISA
        /// device session.
        member session.ReadAsync() =
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            Async.FromBeginEnd(
                (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
                session.EndReadString)

        /// Returns an asynchronous computation which writes a string message to an NI VISA
        /// device session.
        member session.WriteAsync message = 
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            Async.FromBeginEnd(
                (fun (callback, state) -> session.BeginWrite(message, callback, state)),
                session.EndWrite)

        /// Returns an asynchronous computation which first writes a string message to an NI
        /// VISA device session and then awaits a response to that message.
        member session.QueryAsync message =
            async {
                // The cancellation handler ensures that no outstanding asynchronous reads or writes
                // are still on-going when a query is cancelled and that the device buffers are cleared.
                use! __ = Async.OnCancel(fun() -> 
                    session.Terminate()
                    session.Clear())
                do! session.WriteAsync message
                return! session.ReadAsync() }