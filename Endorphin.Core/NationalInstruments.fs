namespace Endorphin.Core

open Endorphin.Core.String
open NationalInstruments.VisaNS
open log4net

/// VISA access to instruments with asynchronous messaging
module NationalInstruments =

    /// Extensions for NationalInstruments.VisaNS.MessageBasedSession which wrap Begin-End pattern
    /// methods into asynchronous workflows.
    type MessageBasedSession with
        
        /// Returns an asynchronous computation which reads the next string from an NI VISA
        /// device session.
        member session.ReadAsync() =
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            let asyncRead = 
                Async.FromBeginEnd(
                    (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
                    session.EndReadString)
            async {
                try
                    let! result = asyncRead
                    return Choice.succeed result
                with exn -> return Choice.fail exn.Message }

        /// Create an asynchronous computation which reads a byte array from an NI VISA device
        /// session.
        member session.ReadBytesAsync() =
            let asyncReadBytes =
                Async.FromBeginEnd(
                    (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
                    session.EndReadByteArray)
            async {
                try
                    let! result = asyncReadBytes
                    return Choice.succeed result
                with exn -> return Choice.fail exn.Message }

        /// Returns an asynchronous computation which writes a string message to an NI VISA
        /// device session.
        member session.WriteAsync message = 
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            let asyncWrite = 
                Async.FromBeginEnd(
                    (fun (callback, state) -> session.BeginWrite(message, callback, state)),
                    session.EndWrite)
            async {
                try
                    do! asyncWrite
                    return Choice.succeed ()
                with
                    exn -> return Choice.fail exn.Message }

        /// Create an asynchronous computation which writes a byte array message to an NI
        /// VISA device session.
        member session.WriteBytesAsync bytes =
            let asyncWrite =
                Async.FromBeginEnd(
                    (fun (callback, state) -> session.BeginWrite(bytes, 0, bytes.Length, callback, state)),
                    session.EndWrite)
            async {
                try
                    do! asyncWrite
                    return Choice.succeed ()
                with
                    exn -> return Choice.fail exn.Message }

        /// Returns an asynchronous computation which first writes a string message to an NI
        /// VISA device session and then awaits a response to that message.
        member session.QueryAsync message =
            asyncChoice {
                // The cancellation handler ensures that no outstanding asynchronous reads or writes
                // are still on-going when a query is cancelled and that the device buffers are cleared.
                use! __ = 
                    AsyncChoice.liftAsync 
                    <| Async.OnCancel(fun() -> 
                        session.Terminate()
                        session.Clear())
                do! session.WriteAsync message
                return! session.ReadAsync() }

        /// Create an asynchronous workflow which writes a byte array to an NI VISA device session,
        /// then reads a response from the same device.
        member session.QueryBytesAsync message =
            asyncChoice {
                use! __ =
                    AsyncChoice.liftAsync
                    <| Async.OnCancel(fun() ->
                        session.Terminate()
                        session.Clear())
                do! session.WriteAsync message
                return! session.ReadBytesAsync() }
    
    [<AutoOpen>]
    /// Contains a model for communication over a National Instruments VISA session.
    module Model =
        /// Provides an interface for communicating with a devices over a National Instruments VISA
        /// MessageBasedSession. Requests are queued and handled asynchronously.
        type VisaInstrument =
            internal { ReadString  : unit -> AsyncChoice<string, string>
                       WriteString : string -> unit
                       QueryString : string -> AsyncChoice<string, string>
                       ReadBytes   : unit -> AsyncChoice<byte [], string>
                       WriteBytes  : byte array -> unit
                       QueryBytes  : string -> AsyncChoice<byte array, string>
                       Close       : unit -> AsyncChoice<unit, string> } 

    [<RequireQualifiedAccess>]
    /// Contains functions for communication with devices which support the National Instruments VISA
    /// interface.
    module Visa =
    
        /// Truncates a message string to 25 characters for logging.
        let private truncateString str =
            if String.length str > 25
            then (String.sub str 0 22 |> String.ofSubstring) + "..."
            else str
        
        /// Represents messages which can be sent over a VISA message-based session.
        type private VisaMessage =
            | ReadString   of replyChannel : AsyncChoiceReplyChannel<string,string>
            | WriteString  of visaCommand : string
            | QueryString  of visaCommand : string * replyChannel : AsyncChoiceReplyChannel<string,string>
            | ReadBytes    of replyChannel  : AsyncChoiceReplyChannel<byte [],string>
            | WriteBytes   of visaCommand  : byte []
            | QueryBytes   of visaCommand  : string * replyChannel : AsyncChoiceReplyChannel<byte [], string>
            | CloseSession of replyChannel : AsyncChoiceReplyChannel<unit,string>

        /// Returns a description of the given VisaMessage.
        let private messageDescription = function
            | ReadString _         -> "Read string from session"
            | WriteString msg      -> sprintf "Write string to session: \"%s\"" (truncateString msg)
            | QueryString (msg, _) -> sprintf "Query response after writing string to session: \"%s\"" (truncateString msg)
            | ReadBytes _          -> "Read bytes from session"
            | WriteBytes msg       -> sprintf "Write bytes to session: \"%s\"" (truncateString <| bytesToHexidecimalString msg)
            | QueryBytes (msg, _)  -> sprintf "Query response after writing bytes to session: \"%s\"" (truncateString msg)
            | CloseSession _       -> "Close session"

        /// Creates an agent which establishes a MessageBasedSession to the specified VISA address
        /// with the specified timeout and handles VisaMessages. 
        let private createAgent visaAddress timeout =
            Agent.Start(fun mailbox ->
                let log = LogManager.GetLogger (sprintf "VISA Instrument \"%s\"" visaAddress)
                
                // trims terminantion characters at the end of a response.     
                let trimResponse = Choice.map (String.trimEnd [|'\n' ; '\r'|])

                // logs a string read from the VISA session if the read was successful
                let logString = function
                    | Success str -> sprintf "Successfully read string from session: %s." (truncateString <| str) |> log.Debug
                    | Failure _   -> ()

                // logs a byte array read from a VISA session if the read was successful
                let logByteArray = function
                    | Success bytes -> sprintf "Successfully read byte array from session: %s." (truncateString <| bytesToHexidecimalString bytes) |> log.Debug
                    | Failure _     -> ()

                // closes the VISA session and returns a response over the specified reply channel
                let closeSession (replyChannel : AsyncChoiceReplyChannel<unit, string>) = async {
                    "Closing session." |> log.Info
                    if mailbox.CurrentQueueLength <> 0 then
                        let error = sprintf "Closing session with %d messages remaining in mailbox queue." mailbox.CurrentQueueLength 
                        log.Error error 
                        Choice.fail error |> replyChannel.Reply
                    else Choice.succeed () |> replyChannel.Reply }

                // dequeues VisaMessages from the mailbox and handles them if communication to the
                // instrument fails
                let rec failed error = async {
                    let! message = mailbox.Receive()
                    let errorMessage = sprintf "Received message '%s' after communication to instrument failed due to error: %s." error (messageDescription message)

                    match message with
                    | CloseSession replyChannel ->
                        do! closeSession replyChannel

                    | WriteString _
                    | WriteBytes _  -> 
                        errorMessage |> log.Error
                        return! failed error

                    | ReadString replyChannel
                    | QueryString (_, replyChannel) ->
                        errorMessage |> log.Error
                        (Choice.fail errorMessage) |> replyChannel.Reply
                        return! failed error

                    | ReadBytes replyChannel
                    | QueryBytes (_, replyChannel) ->
                        errorMessage |> log.Error
                        (Choice.fail errorMessage) |> replyChannel.Reply
                        return! failed error }

                // dequeus VisaMessages from the mailbox and communicates accordingly with the VISA
                // instrument using the MessageBasedSession
                let rec loop (instrument : MessageBasedSession) = async {
                    let! message = mailbox.Receive()
                    sprintf "Received message: %s." (messageDescription message) |> log.Debug

                    let continueAfter response = 
                        match response with
                        | Success _     -> loop instrument
                        | Failure error ->
                            sprintf "Communication to instrument failed: %s." error |> log.Error
                            failed error

                    match message with
                    | ReadString replyChannel ->
                        let! response = instrument.ReadAsync() |> Async.map trimResponse
                        response |> replyChannel.Reply
                        logString response 
                        return! continueAfter response

                    | WriteString visaCommand -> 
                        let! response = instrument.WriteAsync visaCommand
                        return! continueAfter response

                    | QueryString (visaCommand, replyChannel) ->
                        let! response = instrument.QueryAsync visaCommand |> Async.map trimResponse
                        response |> replyChannel.Reply
                        logString response
                        return! continueAfter response

                    | ReadBytes replyChannel ->
                        let! response = instrument.ReadBytesAsync()
                        response |> replyChannel.Reply
                        logByteArray response
                        return! continueAfter response

                    | WriteBytes visaCommand ->
                        let! response = instrument.WriteBytesAsync visaCommand
                        return! continueAfter response

                    | QueryBytes (visaCommand, replyChannel) ->
                        let! response = instrument.QueryBytesAsync visaCommand
                        response |> replyChannel.Reply
                        logByteArray response
                        return! continueAfter response

                    | CloseSession replyChannel -> do! closeSession replyChannel }

                // startup: connects to the instrument and enters the message processing loop
                async {
                    try
                        sprintf "Opening instrument \"%s\"." visaAddress |> log.Info
                        use instrument = ResourceManager.GetLocalManager().Open(visaAddress, AccessModes.NoLock, timeout) :?> MessageBasedSession
                        instrument.Timeout <- timeout
                        return! loop instrument
                    with exn ->
                        let error = sprintf "Failed to connect to instrument: %s." exn.Message
                        error |> log.Error
                        return! failed error })

        /// Opens a connection to a VISA instrument using the specified address and timeout in
        /// milliseconds. The returned VisaInstrument can then be used to communicate with the
        /// instrument.
        let openInstrument visaAddress timeout =
            let agent = createAgent visaAddress timeout

            let queryString visaCommand = agent.PostAndAsyncReply(fun replyChannel -> QueryString(visaCommand, replyChannel))
            let readString () = agent.PostAndAsyncReply ReadString
            let writeString visaCommand = agent.Post (WriteString visaCommand)
            let queryBytes visaCommand = agent.PostAndAsyncReply(fun replyChannel -> QueryBytes(visaCommand, replyChannel))
            let readBytes () = agent.PostAndAsyncReply ReadBytes
            let writeBytes visaCommand = agent.Post (WriteBytes visaCommand)
            let close () = agent.PostAndAsyncReply CloseSession

            { ReadString  = readString
              WriteString = writeString
              QueryString = queryString 
              ReadBytes   = readBytes
              WriteBytes  = writeBytes
              QueryBytes  = queryBytes
              Close       = close }

        /// Closes the connection to the VISA instrument.
        let closeInstrument instrument = instrument.Close

        /// Reads a string from the VISA instrument session.
        let readString      instrument = instrument.ReadString

        /// Writes a string to the VISA instrument session.
        let writeString     instrument = instrument.WriteString
        
        /// Writes a string to the VISA instrument session and a waits a response.
        let queryString     instrument = instrument.QueryString

        /// Reads a byte array from the VISA instrument session.
        let readBytes       instrument = instrument.ReadBytes

        /// Writes a byte array to the VISA instrument session.
        let writeBytes      instrument = instrument.WriteBytes

        /// Writes a byte array to the VISA instrument session and awaits a response.
        let queryBytes      instrument = instrument.QueryBytes
