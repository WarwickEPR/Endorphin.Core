namespace Endorphin.Instrument.LakeShoreTempController

open Endorphin.Core.NationalInstruments
open ExtCore.Control

/// Internal functions for workflows which set and query model values to and from the instrument.
module internal IO =
    
    /// Performs a query asynchronously and tries to parse the result with the given parsing
    /// function which returns a Choice<'T, string> indicating success or failure.
    let private tryPerformQuery (tryParseFunc : string -> Choice<'T, string>) query (TempController tempController) = asyncChoice {
        let! response = query |> Visa.queryString tempController
        return! tryParseFunc response }

    /// Performs a query asynchronously and parses the result with the given parsing function.
    let private performQuery parseFunc = tryPerformQuery (parseFunc >> succeed)

    /// Performs a query for the value corrseponding to the given key and tries to parse the
    /// result with the given parsing function which returns a Choice<'T, string> indicating
    /// success or failure.
    let private tryQueryValue tryParseFunc key =
        tryPerformQuery tryParseFunc (sprintf "%s?" key)

    /// Performs a query for the value corresponding to the given key and parses the result with
    /// the given parsing function.
    let private queryValue parseFunc key =
        performQuery parseFunc (sprintf "%s?" key)

    /// Performs a query for the value corresponding to the given key for the given temperature
    /// controller loop and parses the result with the given parsing function.
    let private queryValueForLoop parseFunc key tempController loop =
        performQuery parseFunc (sprintf "%s? %s" key (loopString loop))

    /// Queries the standard event status byte of the instrument which indicates command and
    /// execution errors.
    let queryStandardEventStatus = queryValue parseStandardEventStatus Keys.standardEventStatus

    /// Sends the provided command string to the instrument and immediately checks the standard
    /// event status byte for errors.
    let private performCommand command (TempController tempController) = asyncChoice {
        command |> Visa.writeString tempController
        let! status = queryStandardEventStatus (TempController tempController)
        if Status.commandError   status then return! fail "Invalid command sent to instrument."
        if Status.executionError status then return! fail "Invalid parameters for command." }

    /// Sets the value corresponding to the given key to the instrument after converting it to a
    /// string with the provided string conversion function.
    let private setValue stringFunc key tempController value =
        performCommand (sprintf "%s %s" key (stringFunc value)) tempController

    /// Sets the value corresponding to the given key for the specified loop to the instrument
    /// after converting it to a string with the string conversion function.
    let private setValueForLoop stringFunc key tempController loop value =
        performCommand (sprintf "%s %s, %s" key (loopString loop) (stringFunc value)) tempController

