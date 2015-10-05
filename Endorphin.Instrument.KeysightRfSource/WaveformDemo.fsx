#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

// This stuff is very "low-level" for creation - in general I'd expect you probably won't need this,
// it's just here for completeness.  This demonstrates direct creation of markers, samples, segments
// and sequences, and their storage and playback.

// There are four marker channels available on the N5172B, and all may be used when the machine is
// being used like this.

// A sample cannot be written to the machine directly, it is just a set of an I value, a Q value and
// a set of markers.  A seq of samples makes up a segment, which is the smallest and simplest
// "waveform" that can be stored.  A segment must be at least Segment.minimumLength (= 60) samples
// long, or the machine will fail to play it back.

// A sequence is the more complex version of a waveform on the machine.  Instead of being made up of
// samples, a sequence is made up of other waveforms, whether they are segments or sequences.  They
// can't be recursive, but there is no lower limit on sequence length.

// Both segments and sequences must be converted to "waveforms" before they can be used, since this
// is more similar to how the machine treats them.

open Endorphin.Core
open Endorphin.Instrument.Keysight
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
    let! storedSegment1 = Control.Waveform.store keysight segment1
    let! storedSegment2 = Control.Waveform.store keysight segment2

    // stored the two sequences we created - there is no dependency management, so it all must
    // be done by us (i.e. store the segments yourself!)
    let! storedSequence1 = Control.Waveform.store keysight sequence1
    let! storedSequence2 = Control.Waveform.store keysight sequence2

    // begin playback on a stored waveform
    do! Control.Waveform.playStored keysight storedSequence1

    // delete a single waveform file
    do! Control.Waveform.delete keysight storedSegment2

    // delete all stored segments from the volatile memory of the machine
    do! Control.Waveform.Segment.deleteAll keysight

    // delete all stored sequences from the non-volatile memory of the machine
    do! Control.Waveform.Sequence.deleteAll keysight

    // delete all stored waveforms from the machine
    do! Control.Waveform.deleteAll keysight

    // close the instrument, because we're done
    do! RfSource.closeInstrument keysight }
|> Async.RunSynchronously