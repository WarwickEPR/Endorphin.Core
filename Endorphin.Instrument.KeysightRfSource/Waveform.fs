namespace Endorphin.Instrument.Keysight

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

module Markers =
    /// A markers record with all markers turned off.
    let empty = { M1 = false; M2 = false; M3 = false; M4 = false }

    /// Set value of the first marker.
    let withMarker1 value markers = { markers with M1 = value }
    /// Set value of the second marker.
    let withMarker2 value markers = { markers with M2 = value }
    /// Set value of the third marker.
    let withMarker3 value markers = { markers with M3 = value }
    /// Set value of the fourth marker.
    let withMarker4 value markers = { markers with M4 = value }

    /// Create a full set of markers straight away.
    let create marker1 marker2 marker3 marker4 =
       { M1 = marker1
         M2 = marker2
         M3 = marker3
         M4 = marker4 }

    /// Make a marker byte out of the booleans in an IQ sample.
    let internal toByte markers =
        ((Convert.ToByte markers.M4) <<< 3) ||| ((Convert.ToByte markers.M3) <<< 2)
        ||| ((Convert.ToByte markers.M2) <<< 1) ||| (Convert.ToByte markers.M1)

module Sample =
    /// Basic data form of IQ point.
    let empty = {
        Sample.I = 0s
        Sample.Q = 0s
        Sample.SampleMarkers = Markers.empty }

    /// Set value of the I sample.
    let withI value sample = { sample with I = value }
    /// Set value of the Q sample.
    let withQ value sample = { sample with Q = value }

    /// Create a new sample with the given I and Q values, and all markers turned off.
    let create i q = empty |> withI i |> withQ q

    /// Set value of the first marker.
    let withMarker1 value (sample : Sample) =
        { sample with SampleMarkers = Markers.withMarker1 value sample.SampleMarkers }
    /// Set value of the second marker.
    let withMarker2 value (sample : Sample) =
        { sample with SampleMarkers = Markers.withMarker2 value sample.SampleMarkers }
    /// Set value of the third marker.
    let withMarker3 value (sample : Sample) =
        { sample with SampleMarkers = Markers.withMarker3 value sample.SampleMarkers }
    /// Set value of the fourth marker.
    let withMarker4 value (sample : Sample) =
        { sample with SampleMarkers = Markers.withMarker4 value sample.SampleMarkers }

    /// Set value of all markers at once.
    let withMarkers markers (sample: Sample) =
        { sample with SampleMarkers = markers }

    /// Get the I value of a sample.
    let i sample = sample.I
    /// Get the Q value of a sample.
    let q sample = sample.Q
    /// Get the markers of a sample.
    let markers sample = sample.SampleMarkers

    /// The phase offset to apply to phases before being passed through the trig functions.  Pi/4 sets
    /// I and Q to be equal amplitude and both positive when phase = 0.
    let private phaseOffset_rad = Math.PI / 4.0

    /// Convert a Phase type into a float value of radians for use in the mathematical functions.
    let private phaseToRadians = function
        // We want IQ to be equal at 0 phase, so rotate phases by pi/4
        | Phase_rad (angle) -> (angle / 1.0<rad>) + phaseOffset_rad
        | Phase_deg (angle) -> (angle * (Math.PI * 2.0 / 360.0) * 1.0<1/deg>) + phaseOffset_rad

    /// The maximum amplitude in arbitrary units that the machine can take for an IQ point amplitude.
    let private maximumMachineAmplitude = Int16.MaxValue

    /// Generate a sample at the given amplitude and phase.  The amplitude is relative to the
    /// maximum amplitude available with the current scaling setting on the machine.
    /// I and Q are equal when phase is 0.
    let withAmplitudeAndPhase relativeAmplitude phase sample =
        let phaseAngle = phaseToRadians phase
        let amplitude = relativeAmplitude * float maximumMachineAmplitude
        sample
        |> withI (int16 (amplitude * Math.Cos phaseAngle))
        |> withQ (int16 (amplitude * Math.Sin phaseAngle))

    /// Get a four-byte array of the IQ data in the correct endianness.
    let internal iqBytes sample =
        /// Convert a 16-bit integer to an array of bytes in machine order.
        let int16ToBytes (number : int16) =
            [| byte ((number &&& 0xFF00s) >>> 8); byte (number &&& 0xFFs) |]
        let i = int16ToBytes (i sample)
        let q = int16ToBytes (q sample)
        [| i.[0]; i.[1]; q.[0]; q.[1] |]

    /// Get a unique representation of a sample as a byte array.
    let internal toBytes sample =
        let arr = Array.create 5 0uy
        arr.[0 .. 3] <- iqBytes sample
        arr.[4] <- Markers.toByte (markers sample)
        arr

    /// Get a hash of a sample.
    let internal hash = toBytes >> Hash.bytes

module Segment =
    /// An empty segment, ready to have samples added to it.
    let empty = {
        SegmentSamples = Array.empty
        SegmentLength  = 0us }

    /// Minimum length a segment may be for playback on the ARB.
    let minimumLength = 60u

    /// Get the samples associated with a segment.
    let internal samples (segment : Segment) = segment.SegmentSamples

    /// Get a unique representation of a segment as a byte array.
    let internal toBytes segment =
        let length = segment.SegmentSamples.Length
        let arr = Array.create (length * 9) 0uy // 5 bytes per sample, 4 bytes per count
        for i in 0 .. length - 1 do
            let (sample, SampleCount reps) = segment.SegmentSamples.[i]
            arr.[(i * 9) + 0 .. (i * 9) + 4] <- Sample.toBytes sample
            arr.[(i * 9) + 5 .. (i * 9) + 8] <- BitConverter.GetBytes reps
            // endianness doesn't matter here
        arr // return the byte array we just created

    /// Get the hash of a segment.
    let internal hash = toBytes >> Hash.bytes

    /// Get the length of a segment.
    let length (segment : Segment) = segment.SegmentLength

    /// Add a sample and a number of repeats to a waveform.
    let add sample count segment = {
        SegmentSamples = Array.append (samples segment) [| (sample, SampleCount <| uint32 count) |]
        SegmentLength  = length segment + count }

    /// Add a sequence of (sample, count) onto a segment.
    let addSeq sequence segment = {
        SegmentSamples =
            sequence
            |> Seq.map (fun (x, y) -> (x, SampleCount y))
            |> Array.ofSeq
            |> Array.append (samples segment)
        SegmentLength =
            sequence
            |> Seq.sumBy snd
            |> uint16
            |> (+) (length segment) }

    /// Complete a segment, creating a waveform to write to the machine.
    let toWaveform segment = Segment (hash segment, segment)

module Sequence =
    /// An empty sequence, ready to have waveforms added to it.
    let empty = SequenceType List.empty

    /// Get the waveforms associated with the sequence.
    let internal waveforms (SequenceType sequence) = sequence

    /// Add a waveform sequence and count to a sequence.
    let add waveform count (SequenceType sequence) =
        let id =
            match waveform with
            | Segment (id, _) -> SegmentId id
            | Sequence (id, _) -> SequenceId id
        SequenceType ((id, count) :: sequence)

    /// Get a bytes representation of a sequence.
    let private toBytes sequence =
        sequence
        |> waveforms
        |> List.map (fun (id, reps) -> sprintf "%s%d" (waveformIdString id) reps)
        |> String.concat ""
        |> System.Text.Encoding.ASCII.GetBytes

    /// Get a hash of the sequence.
    let internal hash sequence = toBytes sequence |> Hash.bytes

    /// Complete a sequence, creating a waveform to write to the machine.
    let toWaveform sequence =
        let id = hash sequence
        let waveforms =
            sequence
            |> waveforms
            |> List.rev
            |> SequenceType
        Sequence (id, waveforms)
