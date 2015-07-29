namespace Endorphin.Instrument.LakeShoreTempController

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text

open Endorphin.Core.StringUtils
open ExtCore.Control

[<AutoOpen>]
/// Internal parsing functions between model types and their corresponding VISA command
/// string formats.
module internal Parsing =

    /// Attempt to parse the given identity string and check if correspond to a LakeShore
    /// model 325 temperature controller.
    let internal tryParseIdentity str =
        let parts = 
            String.split [|','|] str
            |> Array.map (String.trimStart [|' '|] >> String.trimEnd [|' '|])

        if Array.length parts <> 4   then fail <| sprintf "Unexpected device ID string: %s." str
        elif parts.[0] <> "LSCI"     then fail <| sprintf "Unexpected device manufacturer: %s." parts.[0]
        elif parts.[1] <> "MODEL325" then fail <| sprintf "Unexpected device model number: %s." parts.[1]
        else
            succeed <| 
                { Manufacturer = parts.[0]
                  ModelNumber  = parts.[1]
                  SerialNumber = parts.[2]
                  Version      = parts.[3] }

    /// Parse a temperature string in degrees K.
    let parseTemperatureInK (str : string) = TemperatureInK ((float str) * 1.0<K>)

    /// Encode a temperatures in degrees K to a string.
    let temperatureString (TemperatureInK temp) = sprintf "%+.5g" (float temp)

    /// Parse a heater output string in percent.
    let parseHeaterOuptput (str : string) = HeaterOutput ((float str) * 1.0<pct>)

    /// Encode a heater output in percent to a string.
    let heaterOutputString (HeaterOutput output) = sprintf "%+.4g" (float output)

    /// Parse a heater range string.
    let parseHeaterRange = function
        | "0" -> HeaterOff
        | "1" -> HeaterLow
        | "2" -> HeaterHigh
        | str -> failwithf "Unexpected heater range string: %s." str

    /// Encode a heater range to a string.
    let heaterRangeString = function
        | HeaterOff  -> "0"
        | HeaterLow  -> "1"
        | HeaterHigh -> "2"

    /// Parse a control mode string.
    let parseControlMode = function
        | "1" -> ManualPID
        | "2" -> ZoneMode
        | "3" -> OpenLoop
        | "4" -> AutoTunePID
        | "5" -> AutoTunePI
        | "6" -> AutoTuneP
        | str -> failwithf "Unexpected control mode string: %s" str

    /// Encode a control mode to a string.
    let controlModeString = function
        | ManualPID   -> "1"
        | ZoneMode    -> "2"
        | OpenLoop    -> "3"
        | AutoTunePID -> "4"
        | AutoTunePI  -> "5"
        | AutoTuneP   -> "6"

    /// Encode a control loop to a string.
    let loopString = function
        | Loop1 -> "1"
        | Loop2 -> "2"

    /// Parse a PID settings string.
    let parsePidSettings str = 
        let regex = @"\G([\+\-]\d{1,4}\.\d{0,4}),([\+\-]\d{1,4}\.\d{0,4}),([\+\-]\d{1,4}\.\d{0,4})\s$"
        match str with
        | ParseRegex regex [ ParseFloat p ; ParseFloat i ; ParseFloat d ] ->
            { Proportional = p
              Intergral    = i
              Differential = d }
        | str -> failwith "Unexpected PID string: %s." str

    /// Encode PID settings to a string.
    let pidSettingsString pid = sprintf "%+.5g, %+.5g, %+.5g" pid.Proportional pid.Intergral pid.Differential
    
    /// Parse a standard event status byte string.
    let parseStandardEventStatus (str : string) =
        StandardEventStatus <| Array.first (Encoding.ASCII.GetBytes str)
        