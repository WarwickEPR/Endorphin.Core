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
    let private queryValueForLoop parseFunc key loop tempController =
        performQuery parseFunc (sprintf "%s? %s" key (loopString loop))