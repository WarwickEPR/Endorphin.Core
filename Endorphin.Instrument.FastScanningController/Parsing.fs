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

    let uploadString (numberOfPoints : int) = 
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

    let parseUploadAcknowledgement (response : string) = 
        if response.Trim() <> "DL ACK" then
            failwith "Bad command to controller - will not acknowledge path upload. Response was: %s" response

    let parseUploadCompletion (response : string) = 
        if response.Trim() <> "DL DONE" then
            failwith "Could not complete path upload. Response was: %s" response

[<AutoOpen>]
module internal Convert = 
    let pointToVoltage (ScanningController (_, calibration)) (point : Point) = 
        (tfst point * 1e-6m<m/um> / calibration.X, tsnd point * 1e-6m<m/um> / calibration.Y, ttrd point * 1e-6m<m/um> / calibration.Z)
     
    let voltageToPoint (ScanningController (_, calibration)) (point : VoltagePoint) = 
        (tfst point * 1e6m<um/m> * calibration.X, tsnd point * 1e6m<um/m> * calibration.Y, ttrd point * 1e6m<um/m> * calibration.Z)
