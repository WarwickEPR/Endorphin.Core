namespace Endorphin.Core

open NationalInstruments.VisaNS

[<AutoOpen>]
module NationalInstruments =

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