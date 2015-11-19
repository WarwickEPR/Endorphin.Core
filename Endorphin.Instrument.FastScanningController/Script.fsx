#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"
#r "../Endorphin.Utilities.Position/bin/Debug/Endorphin.Utilities.Position.dll"
#r "./bin/Debug/Endorphin.Instrument.FastScanningController.dll"

open Endorphin.Instrument.FastScanningController.Instrument
open Endorphin.Instrument.FastScanningController
open Endorphin.Utilities.Position
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
//log4net.Config.BasicConfigurator.Configure()


let calibration = {X = 8e-6m<m/V>; Y = 8e-6m<m/V>; Z = 8e-6m<m/V>};

async {
    let! scanningController = ScanningController.openInstrument "COM4" 5000<ms> calibration

    let! initialPosition = Position.getPosition scanningController
    printfn "Initial position is: %A" initialPosition

    printfn "Moving to (10, 10, 10)"
    do! Position.setPosition scanningController (10m<um>, 10m<um>, 10m<um>)

    let! finalPosition = Position.getPosition scanningController
    printfn "Final position is: %A" finalPosition

    printfn "Now going to write a path"
    let path = Path.createSnake (0m<um>, 0m<um>, 0m<um>) 20<um> 1m<um> Path.Plane.XY
    do! Position.writePathToController scanningController path

    printfn "Setting dwell time"
    do! Timing.setDwellTime scanningController 30<ms>

    printfn "Running path"
    do Position.runPath scanningController

    printfn "Did it work?"
} |> Async.RunSynchronously
