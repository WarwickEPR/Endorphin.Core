// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.TwickenhamSmc

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open FSharp.Text.RegexProvider

[<AutoOpen>]
/// Internal parsing functions between model types and their corresponding VISA command
/// string formats.
module internal Parsing =

    /// Encode a voltage in volts as a string.
    let voltageString (voltage : decimal<V>) = sprintf "%04.1f" voltage

    /// Encode a current in amps as a string.
    let currentString (current : decimal<A>) = 
        // devices with lower output current require a string with 4 digits after the decimal point
        // to access the maximum available resolution while those with higher output currents require
        // a string with 3 digits before the decimal point to access the maximum output current
        if current >= 100.0M<A>
        then sprintf "%07.3f" current
        else sprintf "%07.4f" current

    /// Encode a ramp rate in amps per second as a string.
    let rampRateString (rampRate : decimal<A/s>) = sprintf "%08.5f" rampRate

    /// Encode a current direction as a string.
    let currentDirectionString = function
        | Forward -> "0"
        | Reverse -> "1"

    /// Parse a current direction string.
    let private parseCurrentDirection = function
        | "0" -> Forward
        | "1" -> Reverse
        | str -> failwithf "Unexpected current direction string: %s." str

    /// Encode a ramp target as a string.
    let rampTargetString = function
        | Zero  -> "0"
        | Lower -> "1"
        | Upper -> "2"

    /// Parse a ramp target string.
    let private parseRampTarget = function
        | "0" -> Zero
        | "1" -> Lower
        | "2" -> Upper
        | str -> failwithf "Unexpected ramp target string: %s." str

    /// Encode a boolean state as a string.
    let booleanStateString = function
        | false -> "0"
        | true  -> "1"

    /// Parse a boolean state string.
    let private parseBooleanState = function
        | "0" -> false
        | "1" -> true
        | str -> failwithf "Unexpected boolean state string: %s." str

    /// Regular expression for parsing magnet controller output parameters.
    type private OutputParametersRegex = Regex< @"\GI(?<OutputCurrent>[\+\-]\d{3}\.\d{3})V(?<OutputVoltage>[\+\-]\d{2}.\d)R(?<RampTarget>[012])[AV]$" >

    /// Parse a string containing the output parameters of the magnet controller.
    let parseOutputParameters str =
        let outputParametersMatch = OutputParametersRegex().Match(str)
        if outputParametersMatch.Success then
            { OutputCurrent = 1.0M<A> * decimal outputParametersMatch.OutputCurrent.Value
              OutputVoltage = 1.0M<V> * decimal outputParametersMatch.OutputVoltage.Value
              RampTarget    = parseRampTarget outputParametersMatch.RampTarget.Value }
        else failwithf "Unexpected magnet controller output parameter string: %s." str

    /// Regular expression for parsing magnet controller current parameters.
    type private CurrentParametersRegex = Regex< @"\GR(?<RampTarget>[012])M(?<ReachedTarget>[01])P(?<IsPaused>[01])X[0-5]H[012]Z0\.00E[0-3][0-7]Q[\+\-\s]\d{3}\.\d{3}$" >

    /// Parse a string containing the current pararmeters of the magnet controller.
    let parseCurrentParameters str =
        let currentParametersMatch = CurrentParametersRegex().Match(str)
        if currentParametersMatch.Success then
            { RampTarget    = parseRampTarget currentParametersMatch.RampTarget.Value
              ReachedTarget = parseBooleanState currentParametersMatch.ReachedTarget.Value
              IsPaused      = parseBooleanState currentParametersMatch.IsPaused.Value }
        else failwithf "Unexpected magnet controller current parameter string: %s." str

    /// Regular expression for magnet controller parsing operating parameters.
    type private OperatingParametersRegex = Regex< @"\GA(?<RampRate>\d{2}\.\d{5})D(?<CurrentDirection>[01])T[01]B[01]W\d{3}\.C0\.\d{6}$" >

    /// Parse a string containing the operating parameters of the magnet controller.
    let parseOperatingParameters str =
        let operatingParametersMatch = OperatingParametersRegex().Match(str)
        if operatingParametersMatch.Success then
            { RampRate         = 1.0M<A/s> * decimal operatingParametersMatch.RampRate.Value
              CurrentDirection = parseCurrentDirection operatingParametersMatch.CurrentDirection.Value }
        else failwithf "Unexpected magnet controller operating parameter string: %s." str

    /// Regular expression for parsing magnet controller set point parameters.
    type private SetPointParametersRegex = Regex< @"\GT[01]U(?<UpperSetPoint>\d{3}\.\d{3})L(?<LowerSetPoint>\d{3}\.\d{3})Y(?<TripVoltage>\d{2}\.\d)$" >

    /// Parse a string containing the set point parameters of the magnet controller.
    let parseSetPointParameters str =
        let setPointParametersMatch = SetPointParametersRegex().Match(str)
        if setPointParametersMatch.Success then
            { LowerSetPoint = 1.0M<A> * decimal setPointParametersMatch.LowerSetPoint.Value 
              UpperSetPoint = 1.0M<A> * decimal setPointParametersMatch.UpperSetPoint.Value
              TripVoltage   = 1.0M<V> * decimal setPointParametersMatch.TripVoltage.Value }
        else failwithf "Unexpected magnet controller set point parameter string: %s." str