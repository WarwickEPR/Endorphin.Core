// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.LakeShoreTempController

open Endorphin.Core

/// Internal functions for workflows which set and query model values to and from the instrument.
module internal IO =
    /// Performs a query asynchronously and parses the result with the given parsing function.
    let private performQuery (parseFunc : string -> 'T) query (TempController tempController) = async {
        let! response = query |> Visa.String.queryAsync tempController
        return parseFunc response }

    /// Performs a query for the value corresponding to the given key and parses the result with
    /// the given parsing function.
    let private queryValue parseFunc key =
        performQuery parseFunc (sprintf "%s?" key)

    /// Performs a query for the value corresponding to the given key for the given temperature
    /// controller loop and parses the result with the given parsing function.
    let private queryValueForLoop parseFunc key tempController loop =
        performQuery parseFunc (sprintf "%s? %s" key (loopString loop)) tempController

    /// Queries the standard event status byte of the instrument which indicates command and
    /// execution errors.
    let private queryStandardEventStatus = queryValue parseStandardEventStatus Keys.standardEventStatus

    /// Sends the provided command string to the instrument and immediately checks the standard
    /// event status byte for errors.
    let private performCommand command (TempController tempController) = async {
        command |> Visa.String.writeAsync tempController
        let! status = queryStandardEventStatus (TempController tempController)
        if Status.commandError   status then invalidArg command "Invalid command sent to instrument."
        if Status.executionError status then invalidArg command "Invalid parameters for command." }

    /// Sets the value corresponding to the given key to the instrument after converting it to a
    /// string with the provided string conversion function.
    let private setValue stringFunc key tempController value =
        performCommand (sprintf "%s %s" key (stringFunc value)) tempController

    /// Sets the value corresponding to the given key for the specified loop to the instrument
    /// after converting it to a string with the string conversion function.
    let private setValueForLoop stringFunc key tempController loop value =
        performCommand (sprintf "%s %s, %s" key (loopString loop) (stringFunc value)) tempController

    /// Query the device identity and fail if it does not correspond to a LakeShore model 325
    /// temperature controller.
    let queryIdentity = queryValue tryParseIdentity

    /// Query the temperature corresponding to the given key for the given instrument control
    /// loop asynchronously.
    let queryTemperatureForLoop = queryValueForLoop parseTemperatureInK

    /// Set the temperature corresponding to the given key for the given instrument control
    /// loop to the specified value asynchronously.
    let setTemperatureForLoop = setValueForLoop temperatureString

    /// Query the heater output corresponding to the given key for the given instrument control
    /// loop asynchronously.
    let queryHeaterOutputForLoop = queryValueForLoop parseHeaterOuptput

    /// Set the heater output corresponding to the given key for the given instrument control
    /// loop to the specified value asynchronously.
    let setHeaterOutputForLoop = setValueForLoop heaterOutputString

    /// Query the heater range corresponding to the given key for the given instrument control
    /// loop asynchronously.
    let queryHeaterRangeForLoop = queryValueForLoop parseHeaterRange

    /// Set the heater range corresponding to the given key for the given instrument control
    /// loop to the specified value asynchronously.
    let setHeaterRangeForLoop = setValueForLoop heaterRangeString

    /// Query the control mode corresponding to the given key for the given instrument control
    /// loop asynchronously.
    let queryControlModeForLoop = queryValueForLoop parseControlMode

    /// Set the control mode corresponding to the given key for the given instrument control
    /// loop to the specified value asynchronously.
    let setControlModeForLoop = setValueForLoop controlModeString

    /// Query the PID settings corresponding to the given key for the given instrument control
    /// loop asynchronously.
    let queryPidSettingsForLoop = queryValueForLoop parsePidSettings

    /// Set the PID settings corresponding to the given key for the given instrument control
    /// loop to the specified value asynchronously.
    let setPidSettingsForLoop = setValueForLoop pidSettingsString