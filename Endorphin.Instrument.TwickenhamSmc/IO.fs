namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core

/// Internal functions for workflows which set and query model values to and from the instrument.
module internal IO =

    /// Performs a query for the value corresponding to the given key and parses it with the given
    /// parsing function.
    let private queryValue parseFunc key (MagnetController (magnetController, _)) = async {
        let! response = (sprintf "%s\r\n" key) |> Visa.String.queryAsync magnetController
        return parseFunc response }
    
    /// Sets the value corresponding to the given key to the instrument after converting it to a
    /// string with the provided string conversion function.
    let private setValue stringFunc key (MagnetController (magnetController, _)) value = async {
        sprintf "%s%s\r\n" key (stringFunc value) |> Visa.String.writeAsync magnetController }

    /// Query magnet controller output parameters with the provided key.
    let queryOutputParameters = queryValue parseOutputParameters

    /// Query magnet controller current parameters with the provided key.
    let queryCurrentParrameters = queryValue parseCurrentParameters

    /// Query magnet controller operating parameters with the provided key.
    let queryOperatingParameters = queryValue parseOperatingParameters

    /// Query magnet controller set-point parameters with the provided key.
    let querySetPointParameters = queryValue parseSetPointParameters

    /// Set the magnet controller current corresponding to the given key.
    let setCurrent = setValue currentString

    /// Set the magnet controller ramp rate corresponding to the given key.
    let setRampRate = setValue rampRateString

    /// Set the magnet controller voltage corresponding to the given key.
    let setVoltage = setValue voltageString

    /// Set the magnet controller current direction corresponding to the given key.
    let setCurrentDirection = setValue currentDirectionString

    /// Set the magnet controller ramp target corresponding to the given key.
    let setRampTarget = setValue rampTargetString

    /// Set the magnet controller boolean state corresponding to the given key.
    let setBooleanState = setValue booleanStateString