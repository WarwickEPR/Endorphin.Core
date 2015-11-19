namespace Endorphin.Instrument.FastScanningController
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point
open Endorphin.Core

[<AutoOpen>]
module internal Parsing = 
    let dwellString (dwellTime : float<ms>) = 
        sprintf "DWELL %.1f" dwellTime

    let triggerDelayString (triggerDelay : float<ms>) = 
        sprintf "DELAY %.1f" triggerDelay

    let downloadString (numberOfPoints : int) = 
        sprintf "DL %d" numberOfPoints

    let voltageString (point : VoltagePoint) = 
        sprintf "V %.3M,%.3M,%.3M" (tfst point / 1m<V>) (tsnd point / 1m<V>) (ttrd point / 1m<V>)

    let parseVoltages (response : string) : VoltagePoint = 
        try
            let voltageString = response.Trim().Split [| ':' |] 
            let voltageStringArray = voltageString.[1].Split [| ',' |]

            (decimal voltageStringArray.[0] * 1m<V>, decimal voltageStringArray.[1] * 1m<V>, decimal voltageStringArray.[2] * 1m<V>)
        with exn ->
            failwith (sprintf "Bad response from the fast scanning controller: %s; exception: %A" response exn)