#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open ARB
open Experiment
open ExtCore.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Control

// BasicConfigurator.Configure()

let result = function
    | Success ()    -> printfn "Success!"
    | Failure error -> printfn "Bad things happened: %s" error

let cycle1 =
    emptyPhaseCycle
    |> addPhase (PhaseInRad 0.0<rad>)
    |> addPhase (PhaseInDeg 90.0<deg>)

let cycle2 =
    emptyPhaseCycle
    |> addPhaseSequence [| for i in 1 .. 2 -> PhaseInDeg (float i * 90.0<deg>) |]

let marker1 = emptyMarkers |> markersWithMarker1 true
let markers1And3 = marker1 |> markersWithMarker3 true

let experiment =
    emptyExperiment
    |> addRfPulse cycle1 60u
    |> addDelayWithIncrement 60u 10u
    |> addTrigger marker1
    |> addRfPulseWithIncrement cycle2 10u 10u
    |> addMarkerPulse markers1And3 20u
    |> withRepetitions 128
    |> withShotRepetitionTime 10e-6<s>

asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000

    do! printCompressedExperiment experiment
    let! storedExperiment = storeExperiment keysight experiment

    (*
    do! deleteAllStoredSegments keysight
    do! deleteAllStoredSequences keysight
    *)

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously
|> result