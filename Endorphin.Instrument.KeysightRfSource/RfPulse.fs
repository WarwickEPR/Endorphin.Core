namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Waveform
open CRC
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module RfPulse =
    module internal Translate =
        /// Create a string of hex values out of binary data
        let toHexString = sprintf "%016x"

        /// Create a sequence with n copies of the original sequence concatenated
        let private makeSequenceCopies n sequence = seq {for _ in 1 .. n do yield! sequence }

        [<AutoOpen>]
        module private Verify =
            /// Check if a particular pulse is an RF pulse or not
            let private isRfPulse = function
                | Rf _ -> true
                | _ -> false

            /// Get the length of an RF pulse's cycle
            let private rfPulseCycleLength = function
                | Rf (PhaseCycle cycle, _, _) -> cycle.Length
                // if the rf filter has done its job, this should never trigger
                | _ -> failwith "Non-RF pulse made it through the RF-pulse filter"

            /// Check if a phase cycle length matches the passed length parameter
            let private incorrectCycleLength length pulse =
                if length = rfPulseCycleLength pulse then false
                else true

            /// Check if a phase cycle has a non-zero length
            let private nonZero length =
                if length = 0 then
                    fail "Phase cycle has no phases in it"
                else
                    succeed length

            /// Check that the length of all the phase cycles in a sequence of pulses are the same.
            /// If they are, then return that number.  If not, then return a failure.
            let private checkPhaseCycles pulses = choice {
                let! length =
                    pulses
                    |> Seq.head
                    |> rfPulseCycleLength
                    |> nonZero
                return!
                    if Seq.exists (incorrectCycleLength length) pulses then
                        fail "Not all phase cycles are the same length"
                    else
                        succeed length }

            /// Make a Choice<'a,'b> into a Choice<'a option,'b>
            let private liftOption = function
                | Success s -> succeed (Some s)
                | Failure f -> fail f

            /// Convert a pulse into a VerifiedPulse type.  The calling function is assumed to be
            let private toVerifiedPulse = function
                | Rf pulse -> VerifiedRf pulse
                | Delay pulse -> VerifiedDelay pulse
                | Trigger pulse -> VerifiedTrigger pulse
                | Marker pulse -> VerifiedMarker pulse

            /// Verify that the user-input experiment is valid and accumulate metadata.  Some examples
            /// of invalid experiments might be ones where phase cycles have different lengths.
            let verify (Experiment (pulses, reps)) = choice {
                let  rfPulses     =   pulses |> Seq.filter isRfPulse
                let! rfPhaseCount =
                    if rfPulses |> Seq.length = 0 then
                        succeed None
                    else
                        rfPulses
                        |> checkPhaseCycles
                        |> liftOption

                return {
                    Pulses = Seq.map toVerifiedPulse pulses
                    Metadata =
                    { ExperimentRepetitions = int reps
                      PulsesCount = Seq.length pulses
                      RfPhaseCount = rfPhaseCount } } }

        [<AutoOpen>]
        module private Compile =
            /// For each rf pulse, turn its PhaseCycle into a length 1 list including only the correct
            /// phase for that iteration of the sequence
            let private chooseCorrectPhase pulseCount index pulse =
                match pulse with
                | VerifiedRf (PhaseCycle phases, dur, inc) ->
                    let phase =
                        // Use integer division to find which phase we should select.
                        // This assumes that the verification step would have put us on the failure
                        // track if there aren't enough phases in any of the cycles
                        List.nth phases (index/pulseCount)
                        |> List.cons []
                        |> PhaseCycle
                    VerifiedRf (phase, dur, inc)
                | _ -> pulse

            /// Expand an Experiment into a sequence of StaticPhasePulses, with the relevant phase
            /// in each RfPulse
            let private expandPhaseCycle experiment =
                match experiment.Metadata.RfPhaseCount with
                | None -> experiment
                | Some n ->
                    let pulses =
                        experiment.Pulses
                        |> makeSequenceCopies n
                        |> Seq.mapi (chooseCorrectPhase experiment.Metadata.PulsesCount)
                    { experiment with Pulses = pulses}

            /// Get the new duration in SampleCount for the nth repetition
            let private duration (SampleCount dur) (SampleCount inc) n =
                SampleCount (dur + inc * n)

            /// Set the duration in a pulse to the correct duration for the repetition that we're on.
            /// The increment remains, but is safe to discard after this function.
            let private chooseCorrectDuration count index pulse =
                // Use integer division to find which repetition of the experiment we're on
                let n = index/count
                match pulse with
                | VerifiedRf (phases, dur, inc) ->
                    VerifiedRf (phases, duration dur inc n, inc)
                | VerifiedDelay (dur, inc) ->
                    VerifiedDelay (duration dur inc n, inc)
                | VerifiedTrigger (markers) ->
                    VerifiedTrigger (markers)
                | VerifiedMarker (markers, dur, inc) ->
                    VerifiedMarker (markers, duration dur inc n, inc)

            let private expandRepetitions experiment =
                let pulses =
                    experiment.Pulses
                    |> makeSequenceCopies experiment.Metadata.ExperimentRepetitions
                    |> Seq.mapi (chooseCorrectDuration experiment.Metadata.PulsesCount)
                { experiment with Pulses = pulses }

            /// Convert an experiment with pulses, increments and a phase cycle into a list of
            /// static pulses where each element is standalone
            let private expandVariables experiment =
                experiment
                |> expandPhaseCycle
                |> expandRepetitions

            /// Return only the pulse sequence of a verified experiment
            let private pulseSequence (experiment : VerifiedExperiment) = experiment.Pulses

            /// Convert a verified pulse to a static pulse.  Assumes that the phase cycle is of length
            /// 1, but this should have been ensured by the expandVariables step.
            let private toStaticPulse = function
                | VerifiedRf (PhaseCycle phases, duration, _) ->
                    StaticRf ((List.exactlyOne phases), duration)
                | VerifiedDelay (duration, _) ->
                    StaticDelay (duration)
                | VerifiedTrigger (markers) ->
                    StaticTrigger (markers)
                | VerifiedMarker (markers, duration, _) ->
                    StaticMarker (markers, duration)

            /// Convert a verified experiment into a sequence of static pulses
            let private toStaticPulses experiment =
                experiment
                |> expandVariables
                |> pulseSequence
                |> Seq.map toStaticPulse

            /// A phase for use when we don't care what the phase actually is
            let private noPhase = PhaseInRad 0.0<rad>

            /// Expand a static pulse into a sample and a count of how many repetitions that sample has
            let private expandStaticPulse =
                function
                | StaticRf (phase, dur)       -> ((generateSample 1.0 phase noMarkers), dur)
                | StaticDelay (dur)           -> ((generateSample 0.0 noPhase noMarkers), dur)
                | StaticTrigger (markers)     -> ((generateSample 0.0 noPhase markers), SampleCount 1)
                | StaticMarker (markers, dur) -> ((generateSample 0.0 noPhase markers), dur)

            /// Convert a list of static pulses into a list of samples
            let private toSamples = Seq.map expandStaticPulse

            /// Compile an experiment into a direct list of samples which could (if they were written
            /// into one segment of a storable size) play back the entire experiment.
            let compile experiment =
                experiment
                |> toStaticPulses
                |> toSamples
                |> CompiledExperiment

        [<AutoOpen>]
        module private Compress =
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

            /// TODO!!!
            let compress compiled = {
                EncodedExperimentId = makeExperimentName [] []
                Segments = []
                Sequences = []
                Experiment = { Name = SequenceId ""; Sequence = [] } }

        [<AutoOpen>]
        module internal Encode =
            /// Encode an experiment into a writeable form
            let toEncodedExperiment experiment = choice {
                let! verified = experiment |> verify
                return
                    verified
                    |> compile
                    |> compress }

            /// Get the experiment ID of an encoded experiment
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
            let toRegularSequence (pending : PendingSequence) : Sequence = {
                Name = pending.Name
                Sequence = pending.Sequence |> List.map toRegularSequenceElement }

    [<AutoOpen>]
    module Control =
        open Translate

        /// Store an experiment onto the machine as a set of necessary sequences and samples
        let storeExperiment instrument experiment = asyncChoice {
            let! encoded = toEncodedExperiment experiment
            let! storedSegments = storeSegmentSequence instrument encoded.Segments
            let! storedSequences =
                encoded.Sequences
                |> Seq.map toRegularSequence
                |> storeSequenceSequence instrument
            return {
                StoredExperiment = StoredExperimentId (getExperimentId encoded)
                StoredSegments   = storedSegments
                StoredSequences  = storedSequences } }