// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.HMC804x

// Most of this file is taken almost directly from the Keysight, and should be factored out into a SCPI library

open Endorphin.Core
open System.Text
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Common functions to set/query values of a VISA instrument.
[<RequireQualifiedAccess>]
module internal IO =
    /// Query a key with a specific value, and parse the response into the internal representation.
    let queryKeyByValueString parseFunc key (HMC804x source) value = async {
        let! response = sprintf "%s? %s\n" key value |> Visa.String.queryAsync source
        return parseFunc response }

    /// Query a source for the value of a particular key, and parse the response into
    /// the internal representation.
    let queryKeyString parseFunc key (HMC804x source) = async {
        let! response = sprintf "%s?\n" key |> Visa.String.queryAsync source
        return parseFunc response }

    /// Query a source for the value of a particular key, then try to parse the response
    /// into the internal representation.  If it fails, then return an error message.
    let tryQueryKeyString (tryParseFunc : string -> Choice<'T, string>) key source = async {
        let! response = queryKeyString id key source
        return tryParseFunc response }

    let queryKeyByValueBytes parseFunc key (HMC804x source) value = async {
        let! response = sprintf "%s? %s\n" key value |> Visa.Bytes.queryAsync source
        return parseFunc response }

    let queryKeyBytes parseFunc key (HMC804x source) value = async {
        let! response = sprintf "%s?\n" key |> Visa.Bytes.queryAsync source
        return parseFunc response }

    [<AutoOpen>]
    module Error =
        /// Parse an error message into an error code and the associated message.
        let internal parseError (str : string) =
            let parts = str.Split ([|','|], 2) // split only on the first comma
            if Array.length parts <> 2 then raise << UnexpectedReplyException <| sprintf "Unexpected error string: %s." str
            match String.tryParse<int> parts.[0] with
            | Some code -> { Code = code ; Message = parts.[1] }
            | None      -> raise << UnexpectedReplyException <| sprintf "Unexpected error code string: %s." parts.[0]

        /// Format an error type nicely as a string.
        let private errorString error = sprintf "%d: %s" error.Code error.Message

        /// Given a key to specify which error, query the machine for the matching error
        /// and parse the result.
        let private queryError = queryKeyString parseError

        /// Key to find the next error in the machine's queue.
        let private nextErrorInQueueKey = ":SYSTEM:ERROR"
        /// Query the machine for the next error message in the error queue and parse the
        /// result.
        let private queryNextErrorInQueue = queryError nextErrorInQueueKey

        /// Loop through the error queue of the machine, creating a sequence of all the listed
        /// errors until the queue is empty.
        let queryErrorQueue source = 
            let rec errorQueueLoop errorList = async {
                let! nextError = queryNextErrorInQueue source
                if nextError.Code <> 0 then return! errorQueueLoop (nextError :: errorList)
                else return Seq.ofList <| List.rev errorList }
            errorQueueLoop List.empty

        /// Check if the machine's error queue has any messages in it.
        let checkErrorQueueIsEmpty errors =
            if Seq.length errors <> 0 then
                raise <| InstrumentErrorException (Seq.map errorString errors)
            else ()

    /// Create the string representation of a command, ready for writing to the instrument.
    let private createCommandString valueMap key value =
        sprintf "%s %s\n" key (valueMap value)

    /// Create an ASCII string representation of a command, ready for writing to the instrument.
    let private createCommandBytes valueMap (key : string) value =
        let bytesKey = Encoding.ASCII.GetBytes key
        Array.concat [ bytesKey ; " "B; (valueMap value); "\n"B ]

    /// Concatenate a sequence of command strings into one writeable command.
    let private concatenateCommandStrings : seq<string> -> string = String.concat ";"

    /// Concatenate a sequence of command byte arrays into one writeable command.
    let private concatenateCommandBytes commands =
        let length = Seq.length commands
        // start with length to include space for the semi-colons.
        let size = commands |> Seq.fold (fun count arr -> count + Array.length arr) length
        let arr = Array.create size 0uy
        let mutable index = 0
        let action command =
            let len = Array.length command
            arr.[index .. (index + len - 1)] <- command
            arr.[index + len] <- ';'B
            index <- index + len + 1
        Seq.iter action commands
        arr

    /// Generic function for writing any command to a source, then query the error queue.
    let private writeCommand writer (HMC804x source) command = async {
        command |> writer source
        let! errors = queryErrorQueue (HMC804x source)
        do checkErrorQueueIsEmpty errors }

    /// Generic function to create a command, then write that value to the machine.
    let private setValue creater writer valueMap key source value =
        creater valueMap key value
        |> writeCommand writer source


    /// Generic function to write a whole sequence of commands, given a sequence each of valueMaps,
    /// keys and values.
    let private setValueSequence creater collecter writer commands source =
        commands
        |> Seq.map (fun (map, key, value) -> creater map key value)
        |> collecter
        |> writeCommand writer source

    /// Set a quantity on the machine to a certain value by converting an internal
    /// representation to a machine representation, using the given valueMap.
    let setValueString valueMap key source value =
        setValue createCommandString Visa.String.writeAsync valueMap key source value

    /// Set a quantity on the machine to a certain value by covnerting an internal
    /// representation to a machine representation, using the given valueMap.  This request
    /// is sent as an ASCII string, rather than a UTF-8 string - it's more difficult to create
    /// and manipulate these values, but there is no risk of errant data causing problems
    /// when encoded to UTF-8.
    let setValueBytes valueMap key source value =
        setValue createCommandBytes Visa.Bytes.writeAsync valueMap key source value

    /// Write a sequence of commands as one single command in string format.
    let setValueStringSequence commands source =
        setValueSequence createCommandString concatenateCommandStrings Visa.String.writeAsync commands source

    /// Write a sequence of commands as one single command in bytes format.
    let setValueBytesSequence commands source =
        setValueSequence createCommandBytes concatenateCommandBytes Visa.Bytes.writeAsync commands source

    /// Write a key without a value to the machine.  Useful for "delete all" style functions.
    let writeKey key source = setValueString (fun _ -> "") key source None
 
    /// Functions related to identifying the connected machine.
    module Identify =
        /// Attempt to parse a device ID string into an internal representation of a
        /// device ID.
        let private parseDeviceId (str : string) =
            let trimWhiteSpace (str : string) = str.TrimStart([|' '|]).TrimEnd([|' '|])
            let parts = str.Split [|','|]
            if Array.length parts <> 5 then
                raise << UnexpectedReplyException <| sprintf "Unexpected device ID string: %s." str
            else
                { Manufacturer = parts.[0] |> trimWhiteSpace
                  ModelNumber  = parts.[1] |> trimWhiteSpace
                  SerialNumber = parts.[2] |> trimWhiteSpace
                  Version      = sprintf "%s, %s" parts.[3] parts.[4] |> trimWhiteSpace }

        /// Key needed to query the identity of any SCPI device.
        let private identityKey = "*IDN"
        /// Query the identity of the given device, and raise an exception if the returned
        /// identity string is not in the expected format.
        let queryIdentity = queryKeyString parseDeviceId identityKey

        /// Check that the model number of a machine is known by the program.
        let private checkModelNumber = function
            | "HMC8043" -> HMC8043
            | model     -> raise << UnexpectedReplyException <| sprintf "Unexpected current/voltage source model number: %s." model

        /// Get the model number of the given source, and raise an exception if this model
        /// is not known to the program.
        let identity source = async {
            let! identity = queryIdentity source
            return checkModelNumber (identity.ModelNumber) }

    /// Functions for connecting and disconnecting from instruments.
    [<AutoOpen>]
    module Connect =
        /// Open an instrument for communication at the given VISA address, with a specified
        /// timeout in milliseconds.
        let openInstrument visaAddress timeout = async {
            let visaInstrument = Visa.openTcpipInstrument visaAddress timeout None
            let source = HMC804x <| visaInstrument
            let! _ = Identify.identity source
            let! _ = Error.queryErrorQueue source // clear the error queue before doing anything
            return source }

        /// Close a given source.
        let closeInstrument (HMC804x source) = Visa.closeInstrument source


    /// Set the quantity represented by the given key to have the integer value given.
    let setInt = setValueString (fun (i : int) -> i.ToString())
    /// Query the given key for a plain integer value.
    let queryInt = queryKeyString int

    /// Set the quantity represented by the given key to have the unsigned 32-bit integer value given.
    let setUint32 = setValueString (fun (i : uint32) -> i.ToString())
    /// Query the given key for a plain uint32 value.
    let queryUint32 = queryKeyString uint32

    /// Set the quantity represented by the given key to have the unsigned 8-bit integer value given.
    let setUint8 = setValueString (fun (i : uint8) -> i.ToString())
    /// Query the given key for a plain uint8 value.
    let queryUint8 = queryKeyString uint8

    /// Set the quantity represented by the given key to have the unsigned byte value given.
    let setByte = setValueString (fun (i : byte) -> i.ToString())
    /// Query the given key for a plain byte value.
    let queryByte = queryKeyString byte

    /// Set the quantity represented by the given key to have the unsigned 16-bit integer value given.
    let setUint16 = setValueString (fun (i : uint16) -> i.ToString())
    /// Query the given key for a plain uint16 value.
    let queryUint16 = queryKeyString uint16

    /// Set the given key to the given on/off state.
    let setOnOffState = setValueString Parsing.onOffStateString
    /// Query the given key for an on/off state.
    let queryOnOffState = queryKeyString Parsing.parseOnOffState

    /// Set the given key to the given output channel
    let setOutput = setValueString Parsing.outputString
    /// Query the given key to get an output channel
    let queryOutput = queryKeyString Parsing.parseOutput

    /// Set the given key to the given current
    let setCurrent = setValueString Parsing.currentString
    /// Query the given key to get a current
    let queryCurrent = queryKeyString Parsing.parseCurrent

    /// Set the given key to the given voltage
    let setVoltage = setValueString Parsing.voltageString
    /// Query the given key to get a voltage
    let queryVoltage = queryKeyString Parsing.parseVoltage

    let setTriggerMode = setValueString Parsing.arbTriggerModeString
    let queryTriggerMode = queryKeyString Parsing.parseTriggerMode

    let setArb : string -> IVSource -> ArbSequence -> Async<unit> = setValueString Parsing.arbString

    let setArbRepetitions key inst =
        Parsing.arbRepetitions >> int >> setInt key inst