namespace Endorphin.Core

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
    
    type private VisaMessage =
        | ReadString of replyChannel : AsyncReplyChannel<string>
        | WriteString of visaCommand : string
        | QueryString of visaCommand : string * replyChannel : AsyncReplyChannel<string>
        | CloseSession of replyChannel : AsyncReplyChannel<unit>

    /// Public interface to VISA instruments
    type VisaInstrument = 
        abstract member Query : string -> Async<string>
        abstract member Write : string -> unit
        abstract member Read : unit -> Async<string>
        abstract member Close : unit -> Async<unit> 

    /// Asynchronously handles requests to 
    let private createAgent visaAddress =
        Agent.Start(fun mailbox ->
            let log = LogManager.GetLogger (sprintf "VISA Instrument %s" visaAddress)

            let rec loop (instrument : MessageBasedSession) =
                async {
                    let! message = mailbox.Receive()
                    
                    match message with
                    
                    | ReadString replyChannel ->
                        "Reading string." |> log.Debug
                        let! response = instrument.ReadAsync()
                        let trimmedResponse = response.TrimEnd [|'\n' ; '\r'|]
                        sprintf "Received string: \"%s\"" trimmedResponse |> log.Debug
                        trimmedResponse |> replyChannel.Reply
                        return! loop instrument

                    | WriteString visaCommand -> 
                        sprintf "Writing string: \"%s\"" visaCommand |> log.Debug
                        do! instrument.WriteAsync visaCommand
                        return! loop instrument

                    | QueryString (visaCommand, replyChannel) ->
                        sprintf "Query with string: \"%s\"" visaCommand |> log.Debug
                        let! response = instrument.QueryAsync visaCommand
                        let trimmedResponse = response.TrimEnd [|'\n' ; '\r'|]
                        sprintf "Received string: \"%s\"" trimmedResponse |> log.Debug
                        trimmedResponse |> replyChannel.Reply
                        return! loop instrument

                    | CloseSession replyChannel -> 
                        "Closing session." |> log.Info
                        if mailbox.CurrentQueueLength <> 0 then
                            sprintf "%d messages remaining in mailbox queue." |> log.Error
                        replyChannel.Reply ()

                        return () }
                     
            // startup
            async {
                sprintf "Opening instrument \"%s\"" visaAddress |> log.Info
                use instrument = ResourceManager.GetLocalManager().Open visaAddress :?> MessageBasedSession
                return! loop instrument })

    let private query (agent : Agent<VisaMessage>) visaCommand =
        agent.PostAndAsyncReply(fun replyChannel -> QueryString(visaCommand, replyChannel))
        
    let private read (agent : Agent<VisaMessage>) =
        agent.PostAndAsyncReply ReadString

    let private write (agent : Agent<VisaMessage>) visaCommand =
        agent.Post (WriteString visaCommand)

    let private close (agent : Agent<VisaMessage>) =
        agent.PostAndAsyncReply CloseSession

    let openInstrument visaAddress =
        let agent = createAgent visaAddress
        { new VisaInstrument with
            member instrument.Query visaCommand = query agent visaCommand
            member instrument.Write visaCommand = write agent visaCommand
            member instrument.Read() = read agent
            member instrument.Close() = close agent }
