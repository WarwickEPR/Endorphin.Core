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
            let asyncRead = 
                Async.FromBeginEnd(
                    (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
                    session.EndReadString)
            async {
                try
                    let! result = asyncRead
                    return Choice1Of2 result
                with exn -> return Choice2Of2 exn.Message }

        /// Returns an asynchronous computation which writes a string message to an NI VISA
        /// device session.
        member session.WriteAsync message = 
            // Creates an asynchronous computation from the begin/end asynchronous pattern.
            // See the documentation for Async.FromBeginEnd for details.
            let asyncWrite = Async.FromBeginEnd(
                (fun (callback, state) -> session.BeginWrite(message, callback, state)),
                 session.EndWrite)
            async {
                try
                    do! asyncWrite
                    return Choice1Of2 ()
                with
                    exn -> return Choice2Of2 exn.Message }

        /// Returns an asynchronous computation which first writes a string message to an NI
        /// VISA device session and then awaits a response to that message.
        member session.QueryAsync message =
            async {
                // The cancellation handler ensures that no outstanding asynchronous reads or writes
                // are still on-going when a query is cancelled and that the device buffers are cleared.
                use! __ = Async.OnCancel(fun() -> 
                    session.Terminate()
                    session.Clear())
                let! response = session.WriteAsync message
                match response with
                   | Choice1Of2 result -> return! session.ReadAsync()
                   | Choice2Of2 error -> return Choice2Of2 error }
                     
    type private VisaMessage =
        | ReadString of replyChannel : AsyncReplyChannel<Choice<string,string>>
        | WriteString of visaCommand : string
        | QueryString of visaCommand : string * replyChannel : AsyncReplyChannel<Choice<string,string>>
        | CloseSession of replyChannel : AsyncReplyChannel<Choice<unit,string>>

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
                        match response with
                        | Choice2Of2 exn -> return Choice2Of2 exn
                        | Choice1Of2 rsp ->
                            let trimmedResponse = rsp.TrimEnd [|'\n' ; '\r'|]
                            sprintf "Received string: \"%s\"" trimmedResponse |> log.Debug
                            Choice1Of2 trimmedResponse |> replyChannel.Reply
                            return! loop instrument

                    | WriteString visaCommand -> 
                        sprintf "Writing string: \"%s\"" visaCommand |> log.Debug
                        let! response = instrument.WriteAsync visaCommand
                        match response with
                        | Choice2Of2 exn -> return Choice2Of2 exn
                        | Choice1Of2 rsp -> return! loop instrument

                    | QueryString (visaCommand, replyChannel) ->
                        sprintf "Query with string: \"%s\"" visaCommand |> log.Debug
                        let! response = instrument.QueryAsync visaCommand
                        match response with
                        | Choice2Of2 exn -> return Choice2Of2 exn
                        | Choice1Of2 rsp -> 
                            let trimmedResponse = rsp.TrimEnd [|'\n' ; '\r'|]
                            sprintf "Received string: \"%s\"" trimmedResponse |> log.Debug
                            Choice1Of2 trimmedResponse |> replyChannel.Reply
                            return! loop instrument

                    | CloseSession replyChannel -> 
                        "Closing session." |> log.Info
                        if mailbox.CurrentQueueLength <> 0 then
                            sprintf "%d messages remaining in mailbox queue." |> log.Error
                        Choice1Of2 () |> replyChannel.Reply 

                        return Choice1Of2 () }
                     
            // startup
            async {
                sprintf "Opening instrument \"%s\"" visaAddress |> log.Info
                use instrument = ResourceManager.GetLocalManager().Open(visaAddress,AccessModes.NoLock,100) :?> MessageBasedSession
                instrument.Timeout <- 1000
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
