namespace Endorphin.Core

open ExtCore.Control
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
                    return succeed result
                with exn -> return fail exn.Message }

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
                    return succeed result
                with exn -> return fail exn.Message }

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
                    return succeed ()
                with
                    exn -> return fail exn.Message }

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
                    return succeed ()
                with
                    exn -> return fail exn.Message }

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
                do! session.WriteBytesAsync message
                return! session.ReadBytesAsync() }
    
    [<RequireQualifiedAccess>]
    module Visa =

        type private VisaMessage =
            | ReadString of replyChannel : AsyncChoiceReplyChannel<string,string>
            | WriteString of visaCommand : string
            | QueryString of visaCommand : string * replyChannel : AsyncChoiceReplyChannel<string,string>
            | ReadBytes of replyChannel  : AsyncChoiceReplyChannel<byte [],string>
            | WriteBytes of visaCommand  : byte []
            | QueryBytes of visaCommand  : byte [] * replyChannel : AsyncChoiceReplyChannel<byte [], string>
            | CloseSession of replyChannel : AsyncChoiceReplyChannel<unit,string>

        type Instrument = private Instrument of Agent<VisaMessage>

        /// Asynchronously handles requests to 
        let private createAgent visaAddress timeout =
            let trimEnd (str : string) = str.TrimEnd [|'\n' ; '\r'|]
        
            Agent.Start(fun mailbox ->
                let log = LogManager.GetLogger (sprintf "VISA Instrument %s" visaAddress)
            
                let closeSession (replyChannel : AsyncChoiceReplyChannel<unit, string>) = async {
                    "Closing session." |> log.Info
                    if mailbox.CurrentQueueLength <> 0 then
                        let error = sprintf "%d messages remaining in mailbox queue." mailbox.CurrentQueueLength 
                        log.Error error 
                        fail error |> replyChannel.Reply
                    else succeed () |> replyChannel.Reply }

                let rec failed () = async {
                    let! message = mailbox.Receive()
                    match message with
                    | ReadString replyChannel ->
                        let error = "Received read request after communication to instrument failed."
                        log.Error error
                        fail error |> replyChannel.Reply
                        return! failed ()
                
                    | WriteString visaCommand ->
                        sprintf "Received write request after communication to instrument failed: %s." visaCommand |> log.Error
                        return! failed ()

                    | QueryString (visaCommand, replyChannel) ->
                        let error = sprintf "Received query request after communication to instrument failed: %s." visaCommand
                        log.Error error
                        fail error |> replyChannel.Reply
                        return! failed ()

                    | ReadBytes replyChannel ->
                        let error = "Received read request after communication to instrument failed."
                        log.Error error
                        fail error |> replyChannel.Reply
                        return! failed ()

                    | WriteBytes visaCommand -> // TODO: possibly log the whole command?
                        sprintf "Received a data write request after communication to instrument failed: \"%A\"" visaCommand |> log.Error
                        return! failed()

                    | QueryBytes (visaCommand, replyChannel) ->
                        let error = sprintf "Received a data query reuest after commnication to instrument failed: %A." visaCommand
                        log.Error error
                        fail error |> replyChannel.Reply
                        return! failed ()

                    | CloseSession replyChannel ->
                        do! closeSession replyChannel
                        return () }

                let rec loop (instrument : MessageBasedSession) = async {
                    let! message = mailbox.Receive()
                    match message with
                    
                    | ReadString replyChannel ->
                        "Reading string." |> log.Debug
                        let! response = instrument.ReadAsync()
                        let trimmedResponse = Choice.map trimEnd response
                        trimmedResponse |> replyChannel.Reply
                        sprintf "Received string response: \"%A\"" trimmedResponse |> log.Debug
                    
                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()

                    | WriteString visaCommand -> 
                        sprintf "Writing string: \"%s\"" visaCommand |> log.Debug
                        let! response = instrument.WriteAsync visaCommand
                    
                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()
                    
                    | QueryString (visaCommand, replyChannel) ->
                        sprintf "Query with string: \"%s\"" visaCommand |> log.Debug
                        let! response = instrument.QueryAsync visaCommand
                        let trimmedResponse = Choice.map trimEnd response
                        trimmedResponse |> replyChannel.Reply
                        sprintf "Received string response: \"%A\"" trimmedResponse |> log.Debug
                    
                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()

                    | ReadBytes replyChannel ->
                        "Reading byte array." |> log.Debug
                        let! response = instrument.ReadBytesAsync()
                        response |> replyChannel.Reply
                        sprintf "Received string response : \"%A\"" response |> log.Debug

                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()

                    | WriteBytes visaCommand ->
                        sprintf "Writing byte array \"%A\"" visaCommand |> log.Debug
                        let! response = instrument.WriteBytesAsync visaCommand

                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()

                    | QueryBytes (visaCommand, replyChannel) ->
                        sprintf "Query with byte array: \"%A\"" visaCommand |> log.Debug
                        let! response = instrument.QueryBytesAsync visaCommand
                        response |> replyChannel.Reply
                        sprintf "Received string response: \"%A\"" response |> log.Debug

                        match response with
                        | Success _     -> return! loop instrument
                        | Failure error -> log.Error error ; return! failed ()

                    | CloseSession replyChannel -> do! closeSession replyChannel

                    return () }

                // startup
                async {
                    try
                        sprintf "Opening instrument \"%s\"" visaAddress |> log.Info
                        use instrument = ResourceManager.GetLocalManager().Open(visaAddress, AccessModes.NoLock, timeout) :?> MessageBasedSession
                        instrument.Timeout <- timeout
                        return! loop instrument
                    with exn ->
                        sprintf "Failed to connect to instrument: %s." exn.Message |> log.Error
                        return! failed () })

        let queryInstrument (Instrument agent) visaCommand =
            agent.PostAndAsyncReply(fun replyChannel -> QueryString(visaCommand, replyChannel))
        
        let readString (Instrument agent) =
            agent.PostAndAsyncReply ReadString

        let writeString (Instrument agent) visaCommand =
            agent.Post (WriteString visaCommand)

        let queryBytesInstrument (Instrument agent) visaCommand =
            agent.PostAndAsyncReply(fun replyChannel -> QueryBytes(visaCommand, replyChannel))

        let readBytes (Instrument agent) =
            agent.PostAndAsyncReply ReadBytes

        let writeBytes (Instrument agent) visaCommand =
            agent.Post (WriteBytes visaCommand)

        let closeInstrument (Instrument agent) =
            agent.PostAndAsyncReply CloseSession

        let openInstrument visaAddress timeout =
            createAgent visaAddress timeout |> Instrument

        // Access the instrument through an interface to allow it to
        // be mocked easily for simple offline tests

        type IVisa =
            abstract member queryInstrument : string -> Async<Choice<string,string>>
            abstract member writeString     : string -> unit
            abstract member readString      : unit   -> Async<Choice<string,string>>
            abstract member queryBytesInstrument : byte [] -> Async<Choice<byte[], string>>
            abstract member writeBytes      : byte [] -> unit
            abstract member readBytes       : unit    -> Async<Choice<byte [],string>>
            abstract member closeInstrument : unit -> Async<Choice<unit,string>>

        type VisaInstrument (visaAddress,timeout) =
            let agent = openInstrument visaAddress timeout
            interface IVisa with
                member __.queryInstrument (str) = queryInstrument agent str
                member __.writeString (str) = writeString agent str
                member __.readString () = readString agent
                member __.queryBytesInstrument (bytes) = queryBytesInstrument agent bytes
                member __.writeBytes (bytes) = writeBytes agent bytes
                member __.readBytes () = readBytes agent
                member __.closeInstrument () = closeInstrument agent