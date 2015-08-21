#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open ExtCore.Control
open Control
open ARB
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


let printResult = function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let numSamples = 1000us

let sample1 =
    emptySample
    |> withI 10000s
    |> withQ 10000s
    |> withMarker1 true

let sample2 =
    emptySample
    |> withI -10000s
    |> withQ 20000s
    |> withMarker2 true

let segment1 =
    emptySegment
    |> addSample sample1 30us
    |> addSample sample2 30us
    |> segmentToWaveform

let segment2 =
    emptySegment
    |> addSample sample2 1us
    |> addSample sample1 59us
    |> segmentToWaveform

let sequence1 =
    emptySequence
    |> addWaveform segment1 12us
    |> addWaveform segment2 1us
    |> sequenceToWaveform

let sequence2 =
    emptySequence
    |> addWaveform sequence1 2us
    |> addWaveform segment1 3us
    |> sequenceToWaveform

asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000

    let! storedSegment1 = storeWaveform keysight segment1
    let! storedSegment2 = storeWaveform keysight segment2

    let! storedSequence1 = storeWaveform keysight sequence1
    let! storedSequence2 = storeWaveform keysight sequence2

    // do! deleteStoredWaveform keysight storedSegment2
    // do! deleteStoredWaveform keysight storedSequence2

    // do! deleteAllStoredSegments keysight
    // do! deleteAllStoredSequences keysight
    // do! deleteAllStoredWaveforms keysight

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously
|> printResult