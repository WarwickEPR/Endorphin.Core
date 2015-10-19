namespace Endorphin.Core

open NationalInstruments.VisaNS
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open log4net

/// Functions for creating connections and talking to VISA instruments in raw IO mode.  Formatted IO using
/// the VISA formatting options is not supported.
[<RequireQualifiedAccess>]
module Visa =
    /// A message which may be passed to the VISA agent, describing what needs to be done.
    type internal Message =
        | ReadString  of                    reply : AsyncReplyChannel<Choice<string, exn>>
        | ReadBytes   of                    reply : AsyncReplyChannel<Choice<byte [], exn>>
        | WriteString of message : string
        | WriteBytes  of message : byte []
        | QueryString of message : string * reply : AsyncReplyChannel<Choice<string, exn>>
        | QueryBytes  of message : string * reply : AsyncReplyChannel<Choice<byte [], exn>>
        | Close       of                    reply : AsyncReplyChannel<Choice<unit, exn>>

    /// A VISA instrument, capable of reading, writing and querying strings and byte arrays.
    type Instrument = internal {
        ReadString  : unit -> Async<Choice<string, exn>>
        WriteString : string -> unit
        QueryString : string -> Async<Choice<string, exn>>
        ReadBytes   : unit -> Async<Choice<byte [], exn>>
        WriteBytes  : byte array -> unit
        QueryBytes  : string -> Async<Choice<byte array, exn>>
        Close       : unit -> Async<Choice<unit, exn>> }

    /// Functions for managing the agent through which communications to the VISA instrument must pass.
    module private Agent =
        /// Get a string representation of the unit type.
        let private unitString () = "unit"

        /// Take a value and return the same value with no side effects.
        let private passthrough value : 'T = value

        /// Truncate a string down to a set number of characters length (currently 75), replacing the
        /// end characters with "..." if necessary.
        let private truncate (str : string) =
            let length = 75
            if str.Length <= length then str |> String.trimEnd [| '\r' ; '\n' |]
            else sprintf "%s..." str.[0 .. length - 4] |> String.trimEnd [| '\r' ; '\n' |]

        /// Remove unnecessary terminators from a string.
        let private removeTerminators value = Choice.map (fun str -> String.trimEnd [| '\n'; '\r' |] str) value

        /// Get the description of a VISA message passed to the agent.
        let private description = function
            | ReadString  _        -> "Read string"
            | ReadBytes   _        -> "Read bytes"
            | WriteString msg      -> sprintf "Write string %s" (truncate msg)
            | WriteBytes  msg      -> sprintf "Write bytes %s"  (truncate <| String.hexOfBytes msg)
            | QueryString (msg, _) -> sprintf "Query string %s" (truncate msg)
            | QueryBytes  (msg, _) -> sprintf "Query bytes %s"  (truncate msg)
            | Close _              -> "Close the agent."

        /// Open a VISA instrument session as an IMessageBasedSession, so communication can occur.
        let private openSession visaId (timeout : int<ms>) =
            let rm = ResourceManager.GetLocalManager ()
            let instrument = rm.Open (visaId, AccessModes.NoLock, int <| timeout) :?> MessageBasedSession
            instrument.Timeout <- int <| timeout
            instrument

        /// A try/catch wrapper around the standard Async.FromBeginEnd to convert exceptions encountered
        /// into Choice failures instead.  This way, exceptions can be propagated up from the agent, and
        /// raised elsewhere.  The agent then doesn't need to be restarted, and it can just continue.
        let private beginEndWrapper starter ender = async {
            try
                let! result = Async.FromBeginEnd (starter, ender)
                return Choice.succeed result
            with exn -> return Choice.fail exn }

        /// The generic read operation on an instrument, with F# allocating the space.  The ender function
        /// is passed the buffer, so it can choose what it wants to do with it.
        let private readGeneric ender (instrument : MessageBasedSession) =
            beginEndWrapper (fun (callback, state) ->
                                instrument.BeginRead (instrument.DefaultBufferSize, callback, state))
                            ender

        /// The generic write operation on a VISA instrument, regardless of the type of data to be written.
        /// The writer function actually handles the writing, the generic part handles putting the
        /// callback into the correct type, and dealing with the result.
        let private writeGeneric writer (instrument : MessageBasedSession) message =
            beginEndWrapper (fun (callback, state) -> writer (message, callback, state))
                            instrument.EndWrite

        /// Read a string from a VISA instrument using an Async.FromBeginEnd layout to do the reading
        /// asynchronously.
        let private readString (instrument : MessageBasedSession) =
            readGeneric instrument.EndReadString instrument

        /// Read an array of bytes from a VISA instrument using an Async.FromBeginEnd layout to do the
        /// reading asynchronously.
        let private readBytes (instrument : MessageBasedSession) =
            readGeneric instrument.EndReadByteArray instrument

        /// Write a string to a VISA instrument using an Async.FromBeginEnd layout to do the writing
        /// asynchronously.
        let private writeString (instrument : MessageBasedSession) (str : string) =
            writeGeneric (fun (str : string, callback, state) -> instrument.BeginWrite (str, callback, state)) instrument str

        /// Write an array of bytes to a VISA instrument using an Async.FromBeginEnd layout to do the
        /// writing asynchronously.
        let private writeBytes (instrument : MessageBasedSession) (bytes : byte []) =
            writeGeneric (fun ((bytes : byte []), callback, state) ->
                             instrument.BeginWrite (bytes, 0, bytes.Length, callback, state))
                         instrument bytes

        /// The generic form of the query operation, with the reader used to interpret the returned
        /// message.  Regardless of the read type, the writing is always done as a string.
        let queryGeneric reader (instrument : MessageBasedSession) (query : string) = async {
            let! writeResult = writeString instrument query
            match writeResult with
                | Success () -> return! reader instrument
                | Failure f  -> return  Choice.fail f }

        /// Query a VISA instrument for a reply in the form of a string, using a string as a query.
        let private queryString instrument query = queryGeneric readString instrument query
        /// Query a VISA instrument for a reply of a byte array, using a string as a query.
        let private queryBytes  instrument query = queryGeneric readBytes  instrument query

        /// Create an agent for communication with a VISA instrument.
        let create visaId (timeout : int<ms>) =
            let instrument = openSession visaId timeout
            Agent.Start (fun (mailbox : Agent<Message>) ->
                /// The log to write commands in the agent to.
                let log = LogManager.GetLogger instrument.ResourceName

                /// Write a response to the log, whether it succeeded or failed.
                let logResponse description stringify = function
                    | Success s  ->
                        sprintf "Successfully executed message \"%s\" with response \"%s\"" description (truncate <| stringify s) |> log.Debug
                    | Failure (exn : exn) ->
                        sprintf "Failed to execute message \"%s\", exception \"%s\"" description exn.Message |> log.Error

                /// Close down the agent, disposing of the instrument.
                let close description (replyChannel : AsyncReplyChannel<Choice<unit, exn>>) = async {
                    log.Info "Closing agent"
                    let queue = mailbox.CurrentQueueLength
                    let response =
                        if queue = 0 then
                            Choice.succeed <| instrument.Dispose ()
                        else
                            let error = sprintf "Received close signal with %d messages still in the mailbox" queue
                            log.Error error
                            Choice.fail (Exception error)
                    response |> replyChannel.Reply
                    logResponse description unitString response }

                /// Loop through any remaining messages in the agent's queue, printing their contents to
                /// the log.
                let rec failed error = async {
                    let! message = mailbox.Receive ()
                    let desc = description message
                    let logMessage =
                        sprintf "After communication failed because of \"%s\", a message was received to \"%s\"" error desc

                    match message with
                    | Close replyChannel -> do! close desc replyChannel
                    | WriteString _
                    | WriteBytes  _ ->
                        logMessage |> log.Error
                        return! failed error

                    | ReadString replyChannel
                    | QueryString (_, replyChannel) ->
                        logMessage |> log.Error
                        Choice.fail (Exception logMessage) |> replyChannel.Reply
                        return! failed error

                    | ReadBytes replyChannel
                    | QueryBytes (_, replyChannel) ->
                        logMessage |> log.Error
                        Choice.fail (Exception logMessage) |> replyChannel.Reply
                        return! failed error }

                /// Loop through messages waiting in the mailbox of the agent, passing them onto the
                /// VISA instrument.
                let rec loop () = async {
                    let! message = mailbox.Receive ()
                    let desc = description message
                    do sprintf "Received message \"%s\"" desc |> log.Info

                    let continueLoop = function
                        | Success _           -> loop ()
                        | Failure (exn : exn) ->
                            let error = sprintf "Communication with instrument failed with message \"%s\"" exn.Message
                            error |> log.Error
                            failed error

                    match message with
                        | ReadString reply ->
                            let! result = readString instrument |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc passthrough result
                            return! continueLoop result

                        | ReadBytes reply ->
                            let! result = readBytes instrument
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! continueLoop result

                        | WriteString msg ->
                            let! result = writeString instrument msg
                            return! continueLoop result

                        | WriteBytes msg ->
                            let! result = writeBytes instrument msg
                            return! continueLoop result

                        | QueryString (query, reply) ->
                            let! result = queryString instrument query |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc passthrough result
                            return! continueLoop result

                        | QueryBytes (query, reply) ->
                            let! result = queryBytes instrument query
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! continueLoop result

                        | Close reply ->
                            do! close desc reply }
                loop () |> Async.map ignore)

    /// Open a VISA instrument with the given VISA identifier address and a timeout in milliseconds to
    /// wait for each raw command.
    let openInstrument visaId (timeout : int<ms>) =
        let agent = Agent.create visaId timeout

        let queryString visaCommand = agent.PostAndAsyncReply(fun replyChannel -> QueryString(visaCommand, replyChannel))
        let readString () = agent.PostAndAsyncReply ReadString
        let writeString visaCommand = agent.Post (WriteString visaCommand)
        let queryBytes visaCommand = agent.PostAndAsyncReply(fun replyChannel -> QueryBytes(visaCommand, replyChannel))
        let readBytes () = agent.PostAndAsyncReply ReadBytes
        let writeBytes visaCommand = agent.Post (WriteBytes visaCommand)
        let close () = agent.PostAndAsyncReply Close

        { ReadString  = readString
          ReadBytes   = readBytes
          WriteString = writeString
          WriteBytes  = writeBytes
          QueryString = queryString
          QueryBytes  = queryBytes
          Close       = close }

    /// Close the connection to a VISA instrument.
    let closeInstrument instrument = instrument.Close

    module String =
        /// Read a string that the VISA instrument is broadcasting.
        let read instrument = async {
            let! result = instrument.ReadString ()
            return Choice.bindOrRaise result }
        /// Write a string to a VISA instrument.
        let write instrument = instrument.WriteString
        /// Query a VISA instrument for a string, using the given string as a query.
        let query instrument message = async {
            let! result = instrument.QueryString message
            return Choice.bindOrRaise result }

    module Bytes =
        /// Read a byte array from a broadcasting VISA instrument.
        let read instrument = async {
            let! result = instrument.ReadBytes ()
            return Choice.bindOrRaise result }
        /// Write a byte array to a VISA instrument.
        let write instrument = instrument.WriteBytes
        /// Query a VISA instrument for a byte array, using the given string as the query.
        let query instrument message = async {
            let! result = instrument.QueryBytes message
            return Choice.bindOrRaise result }