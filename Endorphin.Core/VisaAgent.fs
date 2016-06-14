// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open NationalInstruments.VisaNS
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open log4net

/// Functions for creating connections and talking to VISA instruments in raw IO mode. Formatted IO using
/// the VISA formatting options is not supported.
[<RequireQualifiedAccess>]
module Visa =
    /// A VISA command which may be performed by the agent, describing what needs to be done.
    type internal VisaCommand =
        | ReadString  of                    reply : AsyncReplyChannel<Choice<string, exn>>
        | ReadBytes   of                    reply : AsyncReplyChannel<Choice<byte [], exn>>
        | WriteString of message : string
        | WriteBytes  of message : byte []
        | QueryString of message : string * reply : AsyncReplyChannel<Choice<string, exn>>
        | QueryBytes  of message : string * reply : AsyncReplyChannel<Choice<byte [], exn>>

    /// Represents the synchrnoisation of the VISA I/O operation. In contrast to asynchronous operations,
    /// synchronous I/O can speed up communication with instruments where many short messages are sent
    /// over the API but cause the agent thread to hang while waiting for I/O to complete.
    type internal Synchronisation = Sync | Async

    /// A message which may be passed to the VISA agent, describing what needs to be done.
    type internal Message =
        | Command of command : VisaCommand * synchrnoisation : Synchronisation
        | Close   of reply : AsyncReplyChannel<Choice<unit, exn>>

    /// A VISA instrument, capable of reading, writing and querying strings and byte arrays.
    type Instrument = internal {
        ReadString  : Synchronisation -> Async<Choice<string, exn>>
        WriteString : Synchronisation -> string -> unit
        QueryString : Synchronisation -> string -> Async<Choice<string, exn>>
        ReadBytes   : Synchronisation -> Async<Choice<byte [], exn>>
        WriteBytes  : Synchronisation -> byte array -> unit
        QueryBytes  : Synchronisation -> string -> Async<Choice<byte array, exn>>
        Close       : unit -> Async<Choice<unit, exn>> }
    
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

        let private synchronisationDescription = function
            | Sync  -> "synchronously"
            | Async -> "asynchronously"

        /// Get the description of a VISA message passed to the agent.
        let private description = function
            | Command(ReadString _,         sync) -> sprintf "Read string %s" (synchronisationDescription sync)
            | Command(ReadBytes  _,         sync) -> sprintf "Read bytes %s" (synchronisationDescription sync)
            | Command(WriteString msg,      sync) -> sprintf "Write string %s %s" (truncate msg) (synchronisationDescription sync)
            | Command(WriteBytes  msg,      sync) -> sprintf "Write bytes %s %s" (truncate <| String.hexOfBytes msg) (synchronisationDescription sync)
            | Command(QueryString (msg, _), sync) -> sprintf "Query string %s %s" (truncate msg) (synchronisationDescription sync)
            | Command(QueryBytes  (msg, _), sync) -> sprintf "Query bytes %s %s" (truncate msg) (synchronisationDescription sync)
            | Close _                             -> "Close agent."

        /// Open a VISA instrument session as an IMessageBasedSession, so communication can occur.
        let private openSession visaId (timeout : int<ms>) =
            let rm = ResourceManager.GetLocalManager ()
            let instrument = rm.Open (visaId, AccessModes.NoLock, int <| timeout) :?> MessageBasedSession
            instrument.Timeout <- int timeout
            instrument.TerminationCharacterEnabled <- true
            instrument

        /// Open a VISA instrument session with additional configuration options for a serial connection
        let private openSerialSession visaId (timeout : int<ms>) (configuration : SerialConfiguration) = 
            let rm = ResourceManager.GetLocalManager ()
            let instrument = rm.Open (visaId, AccessModes.NoLock, int <| timeout) :?> SerialSession
            instrument.Timeout <- int timeout
            instrument.BaudRate <- configuration.BaudRate
            instrument.DataBits <- configuration.DataBits

            instrument.StopBits <-
                match configuration.StopBits with
                | One              -> StopBitType.One
                | OnePointFive     -> StopBitType.OneAndOneHalf
                | Two              -> StopBitType.Two

            instrument.Parity <- 
                match configuration.Parity with
                | NoParity         -> Parity.None
                | Even             -> Parity.Even
                | Odd              -> Parity.Odd

            instrument :> MessageBasedSession

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
        let private readAsyncGeneric ender (instrument : MessageBasedSession) =
            beginEndWrapper (fun (callback, state) ->
                                instrument.BeginRead (instrument.DefaultBufferSize, callback, state))
                            ender

        /// The generic write operation on a VISA instrument, regardless of the type of data to be written.
        /// The writer function actually handles the writing, the generic part handles putting the
        /// callback into the correct type, and dealing with the result.
        let private writeAsyncGeneric writer (instrument : MessageBasedSession) message =
            beginEndWrapper (fun (callback, state) -> writer (message, callback, state))
                            instrument.EndWrite

        /// Read a string from a VISA instrument using an Async.FromBeginEnd layout to do the reading
        /// asynchronously.
        let private readStringAsync (instrument : MessageBasedSession) =
            readAsyncGeneric instrument.EndReadString instrument

        /// Read an array of bytes from a VISA instrument using an Async.FromBeginEnd layout to do the
        /// reading asynchronously.
        let private readBytesAsync (instrument : MessageBasedSession) =
            readAsyncGeneric instrument.EndReadByteArray instrument
        
        /// Read a sring from a VISA instrument synchronously.
        let private readStringSync (instrument : MessageBasedSession) = async {
            try
                let result = instrument.ReadString ()
                return Choice.succeed result
            with exn -> return Choice.fail exn }

        /// Read a byte array from a VISA instrument synchronously.
        let private readBytesSync (instrument : MessageBasedSession) = async {
            try
                let result = instrument.ReadByteArray ()
                return Choice.succeed result
            with exn -> return Choice.fail exn }

        /// Read a string from the VISA instrument with the given synchronisation.
        let private readString = function
            | Sync  -> readStringSync
            | Async -> readStringAsync

        /// Read an array of bytes from the VISA instrument with the given synchronisation.
        let private readBytes = function
            | Sync  -> readBytesSync
            | Async -> readBytesAsync

        /// Write a string to a VISA instrument using an Async.FromBeginEnd layout to do the writing
        /// asynchronously.
        let private writeStringAsync (instrument : MessageBasedSession) (str : string) =
            writeAsyncGeneric (fun (str : string, callback, state) -> instrument.BeginWrite (str, callback, state)) instrument str

        /// Write an array of bytes to a VISA instrument using an Async.FromBeginEnd layout to do the
        /// writing asynchronously.
        let private writeBytesAsync (instrument : MessageBasedSession) (bytes : byte []) =
            writeAsyncGeneric (fun ((bytes : byte []), callback, state) ->
                             instrument.BeginWrite (bytes, 0, bytes.Length, callback, state))
                         instrument bytes
            
        /// Write a string to a VISA instrument synchronously.
        let private writeStringSync (instrument : MessageBasedSession) (str : string) = async {
            try
                instrument.Write str
                return Choice.succeed ()
            with exn -> return Choice.fail exn }
            
        /// Write an array of bytes to a VISA instrument synchronously
        let private writeBytesSync (instrument : MessageBasedSession) (bytes : byte []) = async {
            try
                instrument.Write bytes
                return Choice.succeed ()
            with exn -> return Choice.fail exn }

        /// Write a string to the VISA instrument with the given synchronisation.
        let private writeString = function
            | Sync  -> writeStringSync
            | Async -> writeStringAsync

        /// Write an array of bytes to the VISA instrument with the given synchronisation.
        let private writeBytes = function
            | Sync  -> writeBytesSync
            | Async -> writeBytesAsync

        /// The generic form of the query operation, with the reader used to interpret the returned
        /// message.  Regardless of the read type, the writing is always done as a string.
        let queryGeneric writer reader (instrument : MessageBasedSession) (query : string) = async {
            let! writeResult = writer instrument query
            match writeResult with
                | Success () -> return! reader instrument
                | Failure f  -> return  Choice.fail f }

        /// Query the string response of a VISA instrument to a command string asynchronously.
        let private queryStringAsync instrument query = queryGeneric writeStringAsync readStringAsync instrument query

        /// Query the byte array response of a VISA instrument to a command string asynchronously.
        let private queryBytesAsync instrument query = queryGeneric writeStringAsync readBytesAsync  instrument query

        /// Query the string response of a VISA instrument to a command string synchronously.
        let private queryStringSync instrument query = queryGeneric writeStringSync readStringSync instrument query

        /// Query the byte array response of a VISA instrument to a command string synchronously.
        let private queryBytesSync instrument query = queryGeneric writeStringSync readBytesSync  instrument query

        /// Query the string response of a VISA instrument to the given command string with the
        /// given synchronisation.
        let private queryString = function
            | Sync  -> queryStringSync
            | Async -> queryStringAsync

        /// Query the byte array response of a VISA instrument to the given command string with the
        /// given synchronisation.
        let private queryBytes = function
            | Sync  -> queryBytesSync
            | Async -> queryBytesAsync

        /// Create an agent for communication with a VISA instrument. The optional write delay is used to
        /// delay further communication with the hardware after each write command for a specified period.
        let internal create visaId (timeout : int<ms>) (writeDelay : int<ms> option) (configuration : SerialConfiguration option) =
            let instrument = 
                match configuration with
                | Some config     -> openSerialSession visaId timeout config
                | None            -> openSession visaId timeout 

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
                        | Command(ReadString reply, sync)  ->
                            let! result = readString sync instrument |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc id result
                            return! loop ()

                        | Command(ReadBytes reply, sync) ->
                            let! result = readBytes sync instrument
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! loop ()

                        | Command(WriteString msg, sync) ->
                            let! result = writeString sync instrument msg
                            do! writeSleep ()
                            return! loop ()

                        | Command(WriteBytes msg, sync) ->
                            let! result = writeBytes sync instrument msg
                            do! writeSleep ()
                            return! loop ()

                        | Command(QueryString (query, reply), sync) ->
                            let! result = queryString sync instrument query |> Async.map removeTerminators
                            result |> reply.Reply
                            logResponse desc id result
                            return! loop ()

                        | Command(QueryBytes (query, reply), sync) ->
                            let! result = queryBytes sync instrument query
                            result |> reply.Reply
                            logResponse desc String.hexOfBytes result
                            return! loop ()

                        | Close reply ->
                            do! close desc reply }
                
                async { 
                    do! writeSleep()
                    return! loop () } )

    let private openInstrument (agent: MailboxProcessor<Message>) =
        let readString  sync         = (fun replyChannel -> Command(ReadString replyChannel, sync))            |> agent.PostAndAsyncReply 
        let readBytes   sync         = (fun replyChannel -> Command(ReadBytes  replyChannel, sync))            |> agent.PostAndAsyncReply
        let writeString sync command = (Command(WriteString command, sync))                                    |> agent.Post
        let writeBytes  sync command = (Command(WriteBytes command, sync))                                     |> agent.Post
        let queryString sync command = (fun replyChannel -> Command(QueryString(command, replyChannel), sync)) |> agent.PostAndAsyncReply 
        let queryBytes  sync command = (fun replyChannel -> Command(QueryBytes (command, replyChannel), sync)) |> agent.PostAndAsyncReply
        let close () = Close |> agent.PostAndAsyncReply 

        { ReadString  = readString
          ReadBytes   = readBytes
          WriteString = writeString
          WriteBytes  = writeBytes
          QueryString = queryString
          QueryBytes  = queryBytes
          Close       = close }
    
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
    let closeInstrument instrument = instrument.Close () |> Async.map Choice.bindOrRaise

    /// Functions related to string-based I/O.
    module String =

        /// Read a string that the VISA instrument is broadcasting using synchronous I/O.
        let read instrument = instrument.ReadString Sync |> Async.map Choice.bindOrRaise

        /// Read a string that the VISA instrument is broadcasting using asynchronous I/O
        let readAsync instrument = instrument.ReadString Async |> Async.map Choice.bindOrRaise

        /// Write a string to a VISA instrument using synchronous I/O.
        let write instrument = instrument.WriteString Sync

        /// Write a string to a VISA instrument using asynchronous I/O.
        let writeAsync instrument = instrument.WriteString Async

        /// Query a VISA instrument for a string, using the given string as a query using synchronous I/O.
        let query instrument message = instrument.QueryString Sync message |> Async.map Choice.bindOrRaise
        
        /// Query a VISA instrument for a string, using the given string as a query using asynchronous I/O.
        let queryAsync instrument message = instrument.QueryString Async message |> Async.map Choice.bindOrRaise

    /// Functions related to byte array-based I/O.
    module Bytes =

        /// Read a byte array that the VISA instrument is broadcasting using synchronous I/O.
        let read instrument = instrument.ReadBytes Sync |> Async.map Choice.bindOrRaise

        /// Read a byte array that the VISA instrument is broadcasting using asynchronous I/O
        let readAsync instrument = instrument.ReadBytes Async |> Async.map Choice.bindOrRaise

        /// Write a byte array to a VISA instrument using synchronous I/O.
        let write instrument = instrument.WriteBytes Sync

        /// Write a byte array to a VISA instrument using asynchronous I/O.
        let writeAsync instrument = instrument.WriteBytes Async

        /// Query a VISA instrument for a byte array, using the given string as a query using synchronous I/O.
        let query instrument message = instrument.QueryBytes Sync message |> Async.map Choice.bindOrRaise
        
        /// Query a VISA instrument for a byte array, using the given string as a query using asynchronous I/O.
        let queryAsync instrument message = instrument.QueryBytes Async message |> Async.map Choice.bindOrRaise