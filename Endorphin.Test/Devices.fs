module Devices

open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework

let picoScope5000serial = "CW336/061"
let magnetControllerVisaAddress = "GPIB0::4"

let calibratedRampRates = 
    let firstDecade = 
        [ 0.00020<A/s>; 0.00024<A/s>; 0.00026<A/s>; 0.00030<A/s>; 0.00036<A/s>; 0.00042<A/s>; 0.00048<A/s>; 0.00054<A/s>; 
          0.00064<A/s>; 0.00072<A/s>; 0.00084<A/s>; 0.00098<A/s>; 0.00110<A/s>; 0.00130<A/s>; 0.00150<A/s>; 0.00170<A/s>; ]
        
    seq { 
        yield! firstDecade
        yield! Seq.map (fun value -> 10.0 * value) firstDecade
        yield! Seq.map (fun value -> 100.0 * value) firstDecade
        yield! Seq.map (fun value -> 1000.0 * value) firstDecade
        yield (10000.0) * firstDecade.[0] }

let magnetControllerParameters = {
    staticField = 14.0<T>
    fieldCalibration = -0.003<T/A>
    rampRateLimit = 0.1<A/s>
    tripVoltageLimit = 2.5<V>
    maximumCurrent = 20.0<A>
    currentLimit = 5.0<A>
    shuntCalibration = 0.020<V/A>
    outputResolutionInBits = 16 
    setPointDecimalPlaces = 3
    calibratedRampRates = calibratedRampRates }

let initialiseDefaultMagnetControllerState (magnetController : MagnetController) =
    magnetController.SetRampRate 0.098<A/s>
    magnetController.SetRampTarget Zero 
    magnetController.SetPause false
        
    let mutable notAtZero = true
    while notAtZero do
        let outputParams = magnetController.GetOutputParametersAsync()
                           |> Async.RunSynchronously
        notAtZero <- outputParams.outputCurrent <> 0.0<A>

    magnetController.SetCurrentDirection Forward
    magnetController.SetTripVoltage 2.0<V>
    magnetController.SetLowerSetPoint 0.0<A>
    magnetController.SetUpperSetPoint 5.0<A>

    let initialState = magnetController.GetAllParametersAsync() |> Async.RunSynchronously
    Assert.AreEqual(false, initialState.currentParameters.isPaused, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Zero, initialState.currentParameters.rampTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(true, initialState.currentParameters.reachedTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Forward, initialState.operatingParameters.currentDirection, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.098<A/s>, initialState.operatingParameters.rampRate, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.0<A>, initialState.outputParameters.outputCurrent, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Zero, initialState.outputParameters.rampTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.0<A>, initialState.setPointParameters.lowerLimit, "Failed to prepare magnet controller state.")
    Assert.AreEqual(5.0<A>, initialState.setPointParameters.upperLimit, "Failed to prepare magnet controller state.")
    Assert.AreEqual(2.0<V>, initialState.setPointParameters.tripVoltage, "Failed to prepare magnet controller state.")
