#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight
open ExtCore.Control
open Experiment
open Control

let result = function
    | Success ()    -> printfn "Success!"
    | Failure error -> printfn "Bad things happened: %s" error

let cycle1 =
    Phase.empty
    |> Phase.add (PhaseInRad 0.0<rad>)
    |> Phase.add (PhaseInDeg 90.0<deg>)

let cycle2 =
    Phase.empty
    |> Phase.addSeq [| for i in 1 .. 2 -> PhaseInDeg (float i * 90.0<deg>) |]

let marker1 = Markers.empty |> Markers.withMarker1 true
let markers1And3 = marker1  |> Markers.withMarker3 true

let experiment =
    Experiment.empty
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