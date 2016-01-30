// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../Endorphin.Utilities.Position/bin/Debug/Endorphin.Utilities.Position.dll"
#r "./bin/Debug/Endorphin.Instrument.FastScanningController.dll"

open Endorphin.Instrument.FastScanningController.Instrument
open Endorphin.Instrument.FastScanningController
open Endorphin.Utilities.Position
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let calibration = {X = 8e-6m<m/V>; Y = 8e-6m<m/V>; Z = 8e-6m<m/V>};

async {
    let! scanningController = ScanningController.openInstrument "COM7" 5000<ms> calibration

    let! initialPosition = Position.getPosition scanningController
    printfn "Initial position is: %A" initialPosition

    printfn "Moving to (10, 10, 10)"
    do! Position.setPosition scanningController (10m<um>, 10m<um>, 10m<um>)

    let! finalPosition = Position.getPosition scanningController
    printfn "Final position is: %A" finalPosition

    printfn "Now going to write a path"
    let path = Path.createSnake (0m<um>, 0m<um>, 60m<um>) 20<um> 0.1m<um> Path.Plane.XY
    do! Position.Path.writePathToController scanningController path

    printfn "Setting dwell time"
    do! Timing.setDwellTime scanningController 30<ms>

    printfn "Setting trigger delay"
    do! Timing.setTriggerDelay scanningController 3.0<ms>

    printfn "Running path"   
    do! Position.Path.runPath scanningController

    let! numPoints = Position.Path.getNumberOfPoints scanningController
    printfn "Number of points: %d" numPoints
} |> Async.RunSynchronously
