#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Core
open Endorphin.Instrument.Keysight
open Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// a complete set of markers done by the create function
let markersSet1 = Markers.create true true false true
// altering a single marker from a created set of markers
let markersSet2 = Markers.withMarker2 false markersSet1

// create a sample with the create function
let sample1 = Sample.create 10000s 10000s |> Sample.withMarker1 true

// create a sample by altering the empty sample
let sample2 =
    Sample.empty
    |> Sample.withI -10000s
    |> Sample.withQ 20000s
    |> Sample.withMarkers markersSet1

// make a segment by altering the empty segment
let segment1 =
    Segment.empty
    |> Segment.add sample1 30us
    |> Segment.add sample2 30us
    |> Segment.toWaveform

// make another segment by altering an empty segment
let segment2 =
    Segment.empty
    |> Segment.add sample2 1us
    |> Segment.add sample1 59us
    |> Segment.toWaveform

// build up a sequence using the segments we just created
let sequence1 =
    Sequence.empty
    |> Sequence.add segment1 12us
    |> Sequence.add segment2 1us
    |> Sequence.toWaveform

// build up a sequence using a subsequence as well
let sequence2 =
    Sequence.empty
    |> Sequence.add sequence1 2us
    |> Sequence.add segment1 3us
    |> Sequence.toWaveform

async {
    // open the keysight box
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000<ms>

    // store the two segments we created
    let! storedSegment1 = Waveform.store keysight segment1
    let! storedSegment2 = Waveform.store keysight segment2

    // stored the two sequences we created - there is no dependency management, so it all must
    // be done by us (i.e. store the segments yourself!)
    let! storedSequence1 = Waveform.store keysight sequence1
    let! storedSequence2 = Waveform.store keysight sequence2

    // begin playback on a stored waveform
    do! Waveform.playStored keysight storedSequence1

    // delete a single waveform file
    do! Waveform.delete keysight storedSegment2

    // delete all stored segments from the volatile memory of the machine
    do! Waveform.Segment.deleteAll keysight

    // delete all stored sequences from the non-volatile memory of the machine
    do! Waveform.Sequence.deleteAll keysight

    // delete all stored waveforms from the machine
    do! Waveform.deleteAll keysight

    // close the instrument, because we're done
    do! RfSource.closeInstrument keysight }
|> Async.RunSynchronously