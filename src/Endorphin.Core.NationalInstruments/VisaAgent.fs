// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open NationalInstruments.Visa
open Ivi.Visa
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open log4net

/// Functions for creating connections and talking to VISA instruments in raw IO mode. Formatted IO using
/// the VISA formatting options is not supported.
[<RequireQualifiedAccess>]
module Visa =
    /// A VISA command which may be performed by the agent, describing what needs to be done.
    type internal VisaCommand =
        | ReadString  of                     reply : AsyncReplyChannel<Choice<string, exn>>
        | ReadBytes   of                     reply : AsyncReplyChannel<Choice<byte [], exn>>
        | WriteString of message : string
        | WriteBytes  of message : byte []
        | QueryString of message : string  * reply : AsyncReplyChannel<Choice<string, exn>>
        | QueryBytes  of message : byte [] * reply : AsyncReplyChannel<Choice<byte [], exn>>

    /// A message which may be passed to the VISA agent, describing what needs to be done.
    type internal Message =
        | Command of command : VisaCommand
        | Close   of reply : AsyncReplyChannel<Choice<unit, exn>>

    /// A VISA instrument, capable of reading, writing and querzRying strings and byte arrays.
    [< AbstractClass >]
    type Instrument internal () =
        abstract member ReadString  : unit -> Async<Choice<string, exn>>
        abstract member WriteString : string -> unit
        abstract member QueryString : string -> Async<Choice<string, exn>>
        abstract member ReadBytes   : unit -> Async<Choice<byte [], exn>>
        abstract member WriteBytes  : byte [] -> unit
        abstract member QueryBytes  : byte [] -> Async<Choice<byte [], exn>>
        abstract member Close       : unit -> Async<Choice<unit, exn>>
        interface SCPI.IScpiInstrument with
            member this.Write value = this.WriteBytes value |> (fun x -> async {x})
            member this.Query query = this.QueryBytes query |> Async.map Choice.bindOrRaise
            member val Terminator : byte [] = [||] with get, set

    /// Serial port stop bit mode.
    type StopBitMode =
        | One
        | OnePointFive
        | Two

    /// Serial port parity mode.
    type ParityMode =
        | NoParity
        | Odd
        | Even

    /// Serial port configuration.
    type SerialConfiguration = {
        BaudRate    : int
        DataBits    : int16
        StopBits    : StopBitMode
        Parity      : ParityMode }

    /// Functions for managing the agent through which communications to the VISA instrument must pass.
    module private Agent =
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
            | Command(ReadString _        ) -> "Read string"
            | Command(ReadBytes  _        ) -> "Read bytes"
            | Command(WriteString msg     ) -> sprintf "Write string %s" (truncate msg)
            | Command(WriteBytes  msg     ) -> sprintf "Write bytes %s" (truncate <| String.hexOfBytes msg)
            | Command(QueryString (msg, _)) -> sprintf "Query string %s" (truncate msg)
            | Command(QueryBytes  (msg, _)) -> sprintf "Query bytes %s" (truncate <| String.hexOfBytes msg)
            | Close _                             -> "Close agent."

        /// Open a VISA instrument session as an IMessageBasedSession, so communication can occur.
        let private openSession visaId (timeout : int<ms>) =
            let rm = new ResourceManager()
            let instrument = rm.Open (visaId, AccessModes.None, int <| timeout) :?> IMessageBasedSession
            instrument.TimeoutMilliseconds <- int timeout
            instrument.TerminationCharacterEnabled <- true
            instrument

        /// Open a VISA instrument session with additional configuration options for a serial connection
        let private openSerialSession visaId (timeout : int<ms>) (configuration : SerialConfiguration) =
            let rm = new ResourceManager()
            let instrument = rm.Open (visaId, AccessModes.None, int <| timeout) :?> SerialSession
            instrument.TimeoutMilliseconds <- int timeout
            instrument.BaudRate <- configuration.BaudRate
            instrument.DataBits <- configuration.DataBits

            instrument.StopBits <-
                match configuration.StopBits with
                | One              -> SerialStopBitsMode.One
                | OnePointFive     -> SerialStopBitsMode.OneAndOneHalf
                | Two              -> SerialStopBitsMode.Two

            instrument.Parity <-
                match configuration.Parity with
                | NoParity         -> SerialParity.None
                | Even             -> SerialParity.Even
                | Odd              -> SerialParity.Odd

            instrument :> IMessageBasedSession

        /// Read a sring from a VISA instrument synchronously.
        let private readString (instrument : IMessageBasedSession) = async {
            try
                let result = instrument.RawIO.ReadString ()
                return Choice.succeed result
            with exn -> return Choice.fail exn }

        /// Read a byte array from a VISA instrument synchronously.
        let private readBytes (instrument : IMessageBasedSession) = async {
            try
                let result = instrument.RawIO.Read ()
                return Choice.succeed result
            with exn -> return Choice.fail exn }

        /// Write a string to a VISA instrument synchronously.
        let private writeString (instrument : IMessageBasedSession) (str : string) = async {
            try
                instrument.RawIO.Write str
                return Choice.succeed ()
            with exn -> return Choice.fail exn }

        /// Write an array of bytes to a VISA instrument synchronously
        let private writeBytes (instrument : IMessageBasedSession) (bytes : byte []) = async {
            try
                instrument.RawIO.Write bytes
                return Choice.succeed ()
            with exn -> return Choice.fail exn }

        /// The generic form of the query operation, with the reader used to interpret the returned
        /// message.  Regardless of the read type, the writing is always done as a string.
        let queryGeneric writer reader (instrument : IMessageBasedSession) query = async {
            let! writeResult = writer instrument query
            match writeResult with
                | Success () -> return! reader instrument
                | Failure f  -> return  Choice.fail f }

        /// Query the string response of a VISA instrument to a command string synchronously.
        let private queryString instrument query = queryGeneric writeString readString instrument query

        /// Query the byte array response of a VISA instrument to a command string synchronously.
        let private queryBytes instrument query = queryGeneric writeBytes readBytes instrument query

        /// Create an agent for communication with a VISA instrument. The optional write delay is used to
        /// delay further communication with the hardware after each write command for a specified period.
        let internal create visaId (timeout : int<ms>) (writeDelay : int<ms> option) (configuration : SerialConfiguration option) =
            let instrument =
                match configuration with
                | Some config     -> openSerialSession visaId timeout config
                | None            -> openSession visaId timeout

            MailboxProcessor.Start (fun (mailbox : MailboxProcessor<Message>) ->
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
                    logResponse description (fun () -> "()") response }

                /// Asynchronously sleeps for the write delay if one is specified.
                let writeSleep () = async {
                    match writeDelay with
                    | Some delay -> do! Async.Sleep (int delay)
                    | None       -> return () }

                /// Loop through messages waiting in the mailbox of the agent, passing them onto the
                /// VISA instrument.
                let rec loop () = async {
                    let! message = mailbox.Receive ()
                    let desc = description message
                    do sprintf "Received message \"%s\"" desc |> log.Info

                    match message with
                        | Command(ReadString reply)  ->
                            let! result = readString instrument |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc id result
                            return! loop ()

                        | Command(ReadBytes reply) ->
                            let! result = readBytes instrument
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! loop ()

                        | Command(WriteString msg) ->
                            let! result = writeString instrument msg
                            do! writeSleep ()
                            return! loop ()

                        | Command(WriteBytes msg) ->
                            let! result = writeBytes instrument msg
                            do! writeSleep ()
                            return! loop ()

                        | Command(QueryString (query, reply)) ->
                            let! result = queryString instrument query |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc id result
                            return! loop ()

                        | Command(QueryBytes (query, reply)) ->
                            let! result = queryBytes instrument query
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! loop ()

                        | Close reply ->
                            do! close desc reply }

                async {
                    do! writeSleep()
                    return! loop () } )

    let private openInstrument (agent: MailboxProcessor<Message>) =
        { new Instrument () with
            member this.ReadString ()    = (fun replyChannel -> Command (ReadString replyChannel))           |> agent.PostAndAsyncReply
            member this.ReadBytes ()     = (fun replyChannel -> Command (ReadBytes  replyChannel))           |> agent.PostAndAsyncReply
            member this.WriteString str  = Command (WriteString str)                                         |> agent.Post
            member this.WriteBytes bytes = Command (WriteBytes bytes)                                        |> agent.Post
            member this.QueryString str  = (fun replyChannel -> Command (QueryString (str,   replyChannel))) |> agent.PostAndAsyncReply
            member this.QueryBytes bytes = (fun replyChannel -> Command (QueryBytes  (bytes, replyChannel))) |> agent.PostAndAsyncReply
            member this.Close ()         = Close |> agent.PostAndAsyncReply }

    /// Open a RS-232 VISA instrument with the given VISA identifier address and a timeout in milliseconds
    /// to wait for each raw command. The optional write delay is used to delay further communication with
    /// the hardware after each write command for a specified period.
    let openSerialInstrument visaId (timeout : int<ms>) (writeDelay : int<ms> option) (configuration : SerialConfiguration) =
        let agent = Agent.create visaId timeout writeDelay (Some configuration)
        openInstrument agent

    /// Open a Ethernet VISA instrument with the given VISA identifier address and a timeout in milliseconds
    /// to wait for each raw command. The optional write delay is used to delay further communication with
    /// the hardware after each write command for a specified period.
    let openTcpipInstrument visaId (timeout : int<ms>) (writeDelay : int<ms> option) =
        let agent = Agent.create visaId timeout writeDelay None
        openInstrument agent

    /// Open a GPIB VISA instrument with the given VISA identifier address and a timeout in milliseconds
    /// to wait for each raw command. The optional write delay is used to delay further communication with
    /// the hardware after each write command for a specified period.
    let openGpibInstrument visaId (timeout : int<ms>) (writeDelay : int<ms> option) =
        let agent = Agent.create visaId timeout writeDelay None
        openInstrument agent

    /// Close the connection to a VISA instrument.
    let closeInstrument (instrument : Instrument) = instrument.Close () |> Async.map Choice.bindOrRaise

    /// Functions related to string-based I/O.
    module String =
        /// Read a string that the VISA instrument is broadcasting using synchronous I/O.
        let read (instrument : Instrument) = instrument.ReadString >> Async.map Choice.bindOrRaise

        /// Write a string to a VISA instrument using synchronous I/O.
        let write (instrument : Instrument) = instrument.WriteString

        /// Query a VISA instrument for a string, using the given string as a query using synchronous I/O.
        let query (instrument : Instrument) = instrument.QueryString >> Async.map Choice.bindOrRaise

    /// Functions related to byte array-based I/O.
    module Bytes =
        /// Read a byte array that the VISA instrument is broadcasting using synchronous I/O.
        let read (instrument : Instrument) = instrument.ReadBytes () |> Async.map Choice.bindOrRaise

        /// Write a byte array to a VISA instrument using synchronous I/O.
        let write (instrument : Instrument) = instrument.WriteBytes

        /// Query a VISA instrument for a byte array, using the given string as a query using synchronous I/O.
        let query (instrument : Instrument) = instrument.QueryBytes >> Async.map Choice.bindOrRaise
