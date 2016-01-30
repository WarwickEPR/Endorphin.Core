// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.FastScanningController

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point
open Endorphin.Core

[<AutoOpen>]
module internal Parsing = 
    let dwellString (dwellTime : int<ms>) = 
        sprintf "%s %d" Keys.dwellTime dwellTime

    let triggerDelayString (triggerDelay : float<ms>) = 
        sprintf "%s %.1f" Keys.delayTime triggerDelay

    let uploadString (numberOfPoints : int) = 
        sprintf "%s %d" Keys.uploadNumber numberOfPoints

    let voltageString (point : VoltagePoint) = 
        sprintf "%s %.3M,%.3M,%.3M" Keys.voltagePoint (tfst point / 1m<V>) (tsnd point / 1m<V>) (ttrd point / 1m<V>)

    let parseVoltages (response : string) : VoltagePoint = 
        try
            let voltageString = response.Trim().Split [| ':' |] 
            let voltageStringArray = voltageString.[1].Split [| ',' |]

            (decimal voltageStringArray.[0] * 1m<V>, decimal voltageStringArray.[1] * 1m<V>, decimal voltageStringArray.[2] * 1m<V>)
        with exn ->
            failwith (sprintf "Bad response from the fast scanning controller: %s; exception: %A" response exn)

    let parseUploadAcknowledgement (response : string) = 
        if response.Trim() <> Keys.uploadAcknowledgement then
            failwith <| sprintf "Bad command to controller - will not acknowledge path upload. Response was: %s" response

    let parseUploadCompletion (response : string) = 
        if response.Trim() <> Keys.uploadComplete then
            failwith <| sprintf "Could not complete path upload. Response was: %s" response

    let parseNumberOfPoints (response : string) : int = 
        int <| response.Substring 4

[<AutoOpen>]
module internal Convert = 
    let pointToVoltage (ScanningController (_, calibration)) (point : Point) = 
        (tfst point * 1e-6m<m/um> / calibration.X, tsnd point * 1e-6m<m/um> / calibration.Y, ttrd point * 1e-6m<m/um> / calibration.Z)
     
    let voltageToPoint (ScanningController (_, calibration)) (point : VoltagePoint) = 
        (tfst point * 1e6m<um/m> * calibration.X, tsnd point * 1e6m<um/m> * calibration.Y, ttrd point * 1e6m<um/m> * calibration.Z)
