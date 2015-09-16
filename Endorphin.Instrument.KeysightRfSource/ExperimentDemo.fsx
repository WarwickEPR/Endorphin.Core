#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight
open Experiment
open Control

let cycle1 =
    Phase.empty
    |> Phase.add (PhaseInRad 0.0<rad>)
    |> Phase.add (PhaseInDeg 90.0<deg>)

let cycle2 =
    Phase.empty
    |> Phase.addSeq [| for i in 1 .. 2 -> PhaseInDeg (float i * 90.0<deg>) |]

let marker1 = Markers.empty |> Markers.withMarker1 true
let markers1And3 = marker1  |> Markers.withMarker3 true

let pulses = seq {
    yield rf cycle1 60u
    yield delayWithIncrement 60u 10u
    yield trigger marker1
    yield rfWithIncrement cycle2 10u 10u
    yield marker markers1And3 20u }

let experiment =
    Experiment.empty
    |> withPulseSeq pulses
    |> withRepetitions 128
    |> withShotRepetitionTime 10e-6<s>

async {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000<ms>

    do printCompressedExperiment experiment
    let! storedExperiment = storeExperiment keysight experiment

    (*
    do! deleteAllStoredSegments keysight
    do! deleteAllStoredSequences keysight
    *)

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously