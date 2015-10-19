#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Core
open Endorphin.Instrument.Keysight
open Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let numSamples = 1000us

let sample1 =
    Sample.empty
    |> Sample.withI 10000s
    |> Sample.withQ 10000s
    |> Sample.withMarker1 true

let sample2 =
    Sample.empty
    |> Sample.withI -10000s
    |> Sample.withQ 20000s
    |> Sample.withMarker2 true

let segment1 =
    Segment.empty
    |> Segment.add sample1 30us
    |> Segment.add sample2 30us
    |> Segment.toWaveform

let segment2 =
    Segment.empty
    |> Segment.add sample2 1us
    |> Segment.add sample1 59us
    |> Segment.toWaveform

let sequence1 =
    Sequence.empty
    |> Sequence.add segment1 12us
    |> Sequence.add segment2 1us
    |> Sequence.toWaveform

let sequence2 =
    Sequence.empty
    |> Sequence.add sequence1 2us
    |> Sequence.add segment1 3us
    |> Sequence.toWaveform

async {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000<ms>

    let! storedSegment1 = storeWaveform keysight segment1
    let! storedSegment2 = storeWaveform keysight segment2

    let! storedSequence1 = storeWaveform keysight sequence1
    let! storedSequence2 = storeWaveform keysight sequence2

    // do! deleteStoredWaveform keysight storedSegment2
    // do! deleteStoredWaveform keysight storedSequence2

    // do! deleteAllStoredSegments keysight
    // do! deleteAllStoredSequences keysight
    // do! deleteAllStoredWaveforms keysight

    do! RfSource.closeInstrument keysight }
|> Async.RunSynchronously