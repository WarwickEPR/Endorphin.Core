namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open IQData.Control
open CRC
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module RfPulse =
    module internal Translate =
        [<AutoOpen>]
        module Encode =
            open IQData.Configure
            open IQData.Translate.Encode

            /// Create a string of hex values out of binary data
            let toHexString = sprintf "%016x"

            /// Get the ASCII string representation of a PendingSequence's id
            let getPendingSequenceASCIIString (sequence : PendingSequence) =
                sequence.Name
                |> extractSequenceId
                |> toASCIIString

            /// Create the name of an experiment to store in the machine and to use as an internal
            /// reference point.
            let makeExperimentName segments sequences =
                let bytesSegments =
                    segments
                    |> Seq.map getSegmentASCIIString
                    |> Seq.fold Array.append [||]
                let bytesSequences =
                    sequences
                    |> Seq.map getPendingSequenceASCIIString
                    |> Seq.fold Array.append [||]
                bytesSequences
                |> Array.append bytesSegments
                |> crc64
                |> toHexString
                |> ExperimentId

            /// Check if a particular pulse is an RF pulse or not
            let private isRfPulse =
                function
                | Rf (_, _, _) -> true
                | Delay (_, _)
                | Trigger (_)
                | Marker (_, _, _) -> false

            /// Get the length of an RF pulse's cycle
            let private rfPulseCycleLength =
                function
                | Rf (PhaseCycle cycle, _, _) -> cycle.Length
                | _ -> failwith "Non-RF pulse made it through the RF-pulse filter"

            /// Compare two pulse cycle lengths, propagating their length if true and returning a
            /// failure if not.  TODO: this is ugly as sin and not very "functional"
            let private compareCycleLength length1 length2 =
                if length1 = length2 then length1
                else -1

            /// Check that the length of all the phase cycles in a sequence of pulses are the same.
            /// If they are, then return that number.  If not, then return a failure.
            let private phaseCycleLength pulses =
                let filtered = pulses |> Seq.filter isRfPulse
                if Seq.length filtered = 0 then
                    succeed 0
                else
                    let result =
                        filtered
                        |> Seq.map rfPulseCycleLength
                        |> Seq.reduce compareCycleLength
                    if result < 1 then fail ()
                    else succeed result

            /// Create a sequence with n copies of the original sequence concatenated
            let private makeSequenceCopies n sequence =
                seq {for _ in 1 .. n -> sequence }
                |> Seq.concat

            /// Convert a Pulse into a StaticPhasePulse, i.e. pick the correct phase out of the phase
            /// cycle for each iteration.
            let private toStaticPhasePulse length i =
                function
                // use integer division to find which index of the pulse cycle we should pick
                | Rf (PhaseCycle cycle, dur, inc) -> StaticPhaseRf (cycle.[i/length], dur, inc)
                | Delay (dur, inc) -> StaticPhaseDelay (dur, inc)
                | Trigger (markers) -> StaticPhaseTrigger (markers)
                | Marker (markers, dur, inc) -> StaticPhaseMarker (markers, dur, inc)

            /// Expand an Experiment into a sequence of StaticPhasePulses, with the relevant phase
            /// in each RfPulse
            let private expandPhaseCycle (Experiment (pulses, reps)) =
                let cycleLength =
                    match phaseCycleLength pulses with
                    | Success n -> n
                    | Failure -> failwith "Phase cycles were not all the same length, or all 0"
                let pulsesLength = Seq.length pulses
                let sequence =
                    pulses
                    |> makeSequenceCopies cycleLength
                    |> Seq.mapi (toStaticPhasePulse pulsesLength)
                StaticPhaseExperiment (sequence, reps)

            /// Get the new duration in SampleCount for the nth repetition
            let private duration (SampleCount dur) (SampleCount inc) n =
                SampleCount (dur + inc * n)

            /// Convert a StaticPhasePulse into a StaticPulse, using integer division of the pulse index
            /// and the length of the original sequence to find the total duration
            let private toStaticPulse length i pulse =
                let n = i / length
                match pulse with
                | StaticPhaseRf (phase, dur, inc) -> StaticRf (phase, duration dur inc n)
                | StaticPhaseDelay (dur, inc) -> StaticDelay (duration dur inc n)
                | StaticPhaseTrigger (markers) -> StaticTrigger (markers)
                | StaticPhaseMarker (markers, dur, inc) -> StaticMarker (markers, duration dur inc n)

            let private expandRepetitions (StaticPhaseExperiment (pulses, reps)) =
                let length = Seq.length pulses
                pulses
                |> makeSequenceCopies (int reps)
                |> Seq.mapi (toStaticPulse length)

            /// Convert an experiment with pulses, increments and a phase cycle into a list of
            /// static pulses where each element is standalone
            let private toStaticPulses experiment =
                experiment
                |> expandPhaseCycle
                |> expandRepetitions

            /// A phase for use when we don't care what the phase actually is
            let private noPhase = PhaseInRad 0.0<rad>

            let private expandStaticPulse =
                function
                | StaticRf (phase, dur)       -> ((generateSample 1.0 phase noMarkers), dur)
                | StaticDelay (dur)           -> ((generateSample 0.0 noPhase noMarkers), dur)
                | StaticTrigger (markers)     -> ((generateSample 0.0 noPhase markers), SampleCount 1)
                | StaticMarker (markers, dur) -> ((generateSample 0.0 noPhase markers), dur)

            /// Convert a list of static pulses into a list of samples
            let private toSamples sequence =
                sequence
                |> Seq.map expandStaticPulse

            /// Compile an experiment into a direct list of samples which could (if they were written
            /// into one segment of a storable size) play back the entire experiment.
            let compile experiment =
                experiment
                |> toStaticPulses
                |> toSamples
                |> CompiledExperiment

            /// TODO!!!
            let compress compiled : CompressedExperiment = {
                Segments = []
                Sequences = []
                Experiment = { Name = SequenceId ""; Sequence = [] } }

            /// Encode an experiment into a writeable form TODO!!
            let toEncodedExperiment (experiment : Experiment) =
                let compressed =
                    experiment
                    |> compile
                    |> compress
                let name = makeExperimentName compressed.Segments compressed.Sequences
                { Name = name
                  Segments = compressed.Segments
                  Sequences = compressed.Sequences
                  Experiment = compressed.Experiment }

            let getExperimentId encoded =
                encoded.Experiment.Name
                |> extractSequenceId
                |> ExperimentId

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let private toRegularSequenceElement =
                function
                | PendingSegment (name, reps) -> Segment (StoredSegment name, reps)
                | PendingSequence (name, reps) -> Sequence (StoredSequence name, reps)

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let internal toRegularSequence (pending : PendingSequence) : Sequence = {
                Name = pending.Name
                Sequence = pending.Sequence |> List.map toRegularSequenceElement }

    [<AutoOpen>]
    module Control =
        open Translate
        /// Store an experiment onto the machine as a set of necessary sequences and samples
        let storeExperiment instrument experiment =
            let encoded = toEncodedExperiment experiment
            asyncChoice {
                let! storedSegments = storeSegmentSequence instrument encoded.Segments
                let! storedSequences =
                    encoded.Sequences
                    |> Seq.map toRegularSequence
                    |> storeSequenceSequence instrument
                return {
                    Id = getExperimentId encoded
                    Segments = storedSegments
                    Sequences = storedSequences } }