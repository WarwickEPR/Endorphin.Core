#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open log4net.Config
open ExtCore.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// BasicConfigurator.Configure()

open Waveform.Control
open RfPulse.Control

let printResult =
    function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let phaseCycle1 =
    [ 0.0<rad> ; (System.Math.PI/2.0) * 1.0<rad> ]
    |> List.map PhaseInRad
    |> PhaseCycle

let phaseCycle2 =
    [ 90.0<deg> ; -90.0<deg> ]
    |> List.map PhaseInDeg
    |> PhaseCycle

let pulses = seq {
    yield Rf (phaseCycle1, SampleCount 60, SampleCount 10)
    yield Delay (SampleCount 60, SampleCount 10)
    yield Trigger { M1 = true; M2 = false; M3 = true; M4 = false }
    yield Rf (phaseCycle2, SampleCount 60, SampleCount 60)
    yield Marker ( { M1 = false; M2 = true; M3 = false; M4 = true }, SampleCount 120, SampleCount 0)
}

let experiment = Experiment (pulses, 1us)

asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000

    let! storedExperiment = storeExperiment keysight 10000000 experiment
    printfn "%A" storedExperiment

    (*
    do! deleteAllStoredSegments keysight
    do! deleteAllStoredSequences keysight
    *)

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously
|> printResult