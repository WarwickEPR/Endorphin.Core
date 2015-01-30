module Config

open NUnit.Framework
open log4net.Config
open Endorphin.Core.Units
open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type log4netConfig() =
    static do BasicConfigurator.Configure() |> ignore

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
    StaticField = 14.0<T>
    FieldCalibration = -0.003<T/A>
    RampRateLimit = 0.1<A/s>
    TripVoltageLimit = 2.5<V>
    MaximumCurrent = 20.0<A>
    CurrentLimit = 5.0<A>
    ShuntCalibration = 0.020<V/A>
    ShuntOffset = 2e-5<V>
    ShuntNoise = 0.1<V>
    OutputResolution = 16<bits> 
    CalibratedRampRates = calibratedRampRates }

let initialiseDefaultMagnetControllerState (magnetController : MagnetController) =
    magnetController.SetRampRate 0.098<A/s>
    magnetController.SetRampTarget Zero 
    magnetController.SetPause false
    magnetController.SetMaximumRampRate()
    magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward |> Async.RunSynchronously
    magnetController.SetTripVoltage 2.0<V>
    magnetController.SetLowerSetPoint 0.0<A>
    magnetController.SetUpperSetPoint 5.0<A>

    let initialState = magnetController.GetAllParametersAsync() |> Async.RunSynchronously
    Assert.AreEqual(false, initialState.CurrentParameters.IsPaused, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Zero, initialState.CurrentParameters.RampTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(true, initialState.CurrentParameters.ReachedTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Forward, initialState.OperatingParameters.CurrentDirection, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.098<A/s>, initialState.OperatingParameters.RampRate, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.0<A>, initialState.OutputParameters.OutputCurrent, "Failed to prepare magnet controller state.")
    Assert.AreEqual(Zero, initialState.OutputParameters.RampTarget, "Failed to prepare magnet controller state.")
    Assert.AreEqual(0.0<A>, initialState.SetPointParameters.LowerSetPoint, "Failed to prepare magnet controller state.")
    Assert.AreEqual(5.0<A>, initialState.SetPointParameters.UpperSetPoint, "Failed to prepare magnet controller state.")
    Assert.AreEqual(2.0<V>, initialState.SetPointParameters.TripVoltage, "Failed to prepare magnet controller state.")
