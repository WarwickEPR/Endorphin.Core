#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"
#r "../Endorphin.Utilities.Position/bin/Debug/Endorphin.Utilities.Position.dll"
#r "./bin/Debug/Endorphin.Instrument.FastScanningController.dll"

open Endorphin.Instrument.FastScanningController.Instrument
open Endorphin.Instrument.FastScanningController
open Endorphin.Utilities.Position
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
log4net.Config.BasicConfigurator.Configure()


let calibration = {X = 8e-6m<m/V>; Y = 8e-6m<m/V>; Z = 8e-6m<m/V>}

async {
    let! scanningController = ScanningController.openInstrument "COM4" 5000<ms> calibration
    printfn "Moving to (10, 10, 10)"
    do! Position.setPosition scanningController (2m<um>, 4m<um>, 6m<um>)
    let! initialPosition = Position.getPosition scanningController
    printfn "Initial position is: %A" initialPosition
    let! finalPosition = Position.getPosition scanningController
    printfn "Final position is: %A" finalPosition }
|> Async.RunSynchronously