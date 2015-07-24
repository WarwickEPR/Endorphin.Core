namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Waveform
open CRC
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

module RfPulse =
    /// Functions for translating human-readable experiment data into a machine-readable form
    module internal Translate =
        /// Create a hash of a byte array as a hexadecimal string 16 characters long
        let hexHash arr = sprintf "%016x" (crc64 arr)

        /// Create a list with n copies of the original list concatenated
        let private makeListCopies n list =
            let rec loop list acc = function
                | 0 -> acc
                | i -> loop list (list @ acc) (i - 1)
            loop list [] n

        /// Functions to check that a user-input experiment is in a valid form, and collect metadata
        /// for use in the compilation step.
        [<AutoOpen>]
        module private Metadata =
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
            let private incorrectCycleLength length pulse = not (length = rfPulseCycleLength pulse)

            /// Check if a phase cycle has a non-zero length
            let private nonZero length =
                if length = 0 then
                    fail "Phase cycle has no phases in it"
                else
                    succeed length

            /// Check that the length of all the phase cycles in a sequence of pulses are the same.
            /// If they are, then return that number.  If not, then return a failure.
            let private checkPhaseCycles (pulses : List<Pulse>) = choice {
                let! length =
                    pulses.Head
                    |> rfPulseCycleLength
                    |> nonZero
                return!
                    if List.exists (incorrectCycleLength length) pulses then
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
            let metadata (Experiment (pulses, reps)) = choice {
                // Check that there's a non-zero number of experiment repetitions
                do! if reps = 0us then fail "Must do the experiment at least once!" else succeed ()
                let  pulses'  = List.ofSeq pulses
                let  rfPulses = pulses' |> List.filter isRfPulse
                let! rfPhaseCount =
                    if rfPulses.Length = 0 then
                        succeed None
                    else
                        rfPulses
                        |> checkPhaseCycles
                        |> liftOption
                return {
                    Pulses = List.map toVerifiedPulse pulses'
                    Metadata =
                    { ExperimentRepetitions = int reps
                      PulsesCount = pulses'.Length
                      RfPhaseCount = rfPhaseCount } } }

        /// Functions for compiling experiments into a form which can be more easily compressed
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

                        // TODO: due to random access, might be better as an array
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
                        |> makeListCopies n
                        |> List.mapi (chooseCorrectPhase experiment.Metadata.PulsesCount)
                    { experiment with Pulses = pulses}

            /// Get the new duration in SampleCount for the nth repetition
            let private duration (SampleCount dur) (SampleCount inc) n = SampleCount (dur + inc * n)

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

            /// Expand a sequence of pulses which still have increment data attached into a sequence of
            /// pulses with ignorable increments.  The increments are now encoded into the durations.
            let private expandRepetitions experiment =
                let reps = experiment.Metadata.ExperimentRepetitions
                let phaseCount = experiment.Metadata.RfPhaseCount
                let pulsesPerRep =
                    match phaseCount with
                    | Some s -> s * experiment.Metadata.PulsesCount
                    | None -> experiment.Metadata.PulsesCount
                let pulses =
                    experiment.Pulses
                    |> makeListCopies reps
                    |> List.mapi (chooseCorrectDuration pulsesPerRep)
                { experiment with Pulses = pulses }

            /// Convert an experiment with pulses, increments and a phase cycle into a list of
            /// static pulses where each element is standalone
            let private expandVariables experiment =
                experiment
                |> expandPhaseCycle
                |> expandRepetitions

            /// Return only the pulse sequence of a verified experiment
            let private pulseSequence (experiment : VerifiedExperiment) = experiment.Pulses

            /// Convert a verified pulse to a static pulse. Assumes that the phase cycle is of length
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
                |> List.map toStaticPulse

            /// A phase for use when we don't care what the phase actually is
            let private noPhase = PhaseInRad 0.0<rad>

            /// Expand a static pulse into a sample and a count of how many repetitions that sample has
            let private expandStaticPulse = function
                | StaticRf (phase, dur)       -> ((generateSample 1.0 phase noMarkers), dur)
                | StaticDelay (dur)           -> ((generateSample 0.0 noPhase noMarkers), dur)
                | StaticTrigger (markers)     -> ((generateSample 0.0 noPhase markers), SampleCount 1)
                | StaticMarker (markers, dur) -> ((generateSample 0.0 noPhase markers), dur)

            /// Add two SampleCounts together
            let private addDurations (_, SampleCount one) (_, SampleCount two) = SampleCount (one + two)

            /// Given a list of samples and their durations, group any adjaacent samples which are
            /// equal and update the durations accordingly
            // TODO: de-ugly this
            let private groupEqualSamples samples =
                let rec loop list acc =
                    match list with
                    | [] -> acc |> List.rev
                    | x :: xs ->
                        match acc with
                        | [] -> loop xs [x]
                        | y :: ys when fst x = fst y ->
                            let acc' = (fst y, addDurations x y) :: ys
                            loop xs acc'
                        | _ -> loop xs (x :: acc)
                loop samples []

            /// Convert a list of static pulses into a list of samples
            let private toSamples pulses =
                pulses
                |> List.map expandStaticPulse
                |> groupEqualSamples

            /// Count the number of pulses in the experiment, and return the completed CompiledExperiment
            let private toCompiledExperiment list =
                let rec loop n = function
                    | [] -> n
                    | (_, SampleCount dur) :: tail -> loop (n+dur) tail
                { Data = list; Length = loop 0 list }

            /// Compile an experiment into a direct list of samples which could (if they were written
            /// into one segment of a storable size) play back the entire experiment.
            let compile experiment =
                experiment
                |> toStaticPulses
                |> toSamples
                |> toCompiledExperiment

        /// Functions to compress streams of pulses into waveform and sequence files
        [<AutoOpen>]
        module private Compress =
            /// Convert a sample into an array of bytes
            let private sampleToByteArray sample =
                Array.concat [
                    BitConverter.GetBytes sample.I
                    BitConverter.GetBytes sample.Q
                    BitConverter.GetBytes sample.Markers.M1
                    BitConverter.GetBytes sample.Markers.M2
                    BitConverter.GetBytes sample.Markers.M3
                    BitConverter.GetBytes sample.Markers.M4 ]

            /// Make a segment name string by hashing the samples
            let private hashSegment (samples : SegmentData) =
                samples
                |> List.map sampleToByteArray
                |> List.fold Array.append [||]
                |> hexHash

            /// An empty compressed experiment, ready to be added to
            let private emptyCompressedExperiment = {
                Segments = Map.empty
                Sequences = Map.empty
                SampleCount = 0
                CompressedExperiment = [] }

            /// Split a compiled experiment into a head of SegmentData, and a tail of
            /// CompiledExperiment, based on the passed number of samples to pick
            // TODO: rather ugly
            let private splitCompiledExperiment n compiled : (SegmentData * CompiledExperiment) =
                let rec loop list acc = function
                    | 0 -> List.rev acc, { Data = list; Length = compiled.Length - n }
                    | i ->
                        let (head, tail) = List.take 1 list
                        let (sample, SampleCount dur) = List.exactlyOne head
                        let (list', taken) =
                            if i >= dur then
                                (tail, dur)
                            else
                                ((sample, SampleCount(dur-i))::tail, i)
                        let acc' = [for _ in 1 .. taken -> sample] @ acc
                        loop list' acc' (i - taken)
                loop compiled.Data [] n

            /// Split a CompiledExperiment into a 60 sample segment (if possible), and the remainder
            /// of the experiment
            let private splitCompiledAtNextSegment compiled =
                let segmentLength = if compiled.Length < 120 then compiled.Length else 60
                splitCompiledExperiment segmentLength compiled

            /// Get the last segment ID from a sequence element.  Current compression doesn't create
            /// any subsequences, so if we find one, something has gone very wrong!
            let private lastSegmentIdFromSequenceElement = function
                | PendingSegment (SegmentId id, _) -> id
                | _ -> failwith "Compression doesn't support writing subsequences in experiments"

            /// Find the most recent SegmentId - during the compression, the most recent one is
            /// first in the list, since the list is built up in reverse order
            let private lastSegmentId compressed =
                match List.tryHead compressed.CompressedExperiment with
                | None -> None
                | Some s -> Some (lastSegmentIdFromSequenceElement s)

            /// Increase the number of reps on the last segment in a CompressedExperiment
            let private increaseLastSegmentReps compressed =
                let data =
                    match compressed.CompressedExperiment with
                    | PendingSegment (id, count) :: tail ->
                        if count = UInt16.MaxValue then
                            PendingSegment (id, 1us) :: compressed.CompressedExperiment
                        else
                            PendingSegment (id, count + 1us) :: tail
                    | _ -> failwith "Compression doesn't support writing subsequences in experiments"
                { compressed with CompressedExperiment = data }

            /// Prepend a different segment onto the passed compressed experiment.
            // Map.add is safe even if the segment already exists because it just doesn't do
            // anything in that case.
            let private consDifferentSegment id samples (compressed : CompressedExperiment) =
                { Segments = Map.add id samples compressed.Segments
                  Sequences = compressed.Sequences
                  SampleCount = compressed.SampleCount + samples.Length
                  CompressedExperiment =
                      (PendingSegment (SegmentId id, 1us)) :: compressed.CompressedExperiment }

            /// Prepend some SegmentData onto a CompressedExperiment
            let private consToExperiment compressed samples =
                let id = hashSegment samples
                if Some id = lastSegmentId compressed then
                    increaseLastSegmentReps compressed
                else
                    consDifferentSegment id samples compressed

            /// One step in the compression
            let rec private compressionLoop output input =
                match input.Length with
                | 0 ->
                    { output with CompressedExperiment = List.rev output.CompressedExperiment }
                | _ ->
                    let (segment, input') = splitCompiledAtNextSegment input
                    let output' = consToExperiment output segment
                    compressionLoop output' input'

            /// Compress the experiment down using a basic 60-sample dictionary method
            let compress = compressionLoop emptyCompressedExperiment

        /// Internal functions for encoding experiments from the user-input form to the writeable
        /// machine form of repeatable files.
        [<AutoOpen>]
        module Encode =
            /// Zip a tupled segment back into an actual segment
            let private zipSegment (id, compressed : SegmentData) : Segment = {
                Name = SegmentId id
                Data = compressed }

            /// Zip a tupled sequence back into an actual sequence
            let private zipPendingSequence (id, compressed : PendingSequenceData) = {
                Name = SequenceId id
                PendingSequence = compressed }

            /// Convert a PendingSequence element into a byte array of the name followed by the
            /// number of repetitions
            let private elementToByteArray = function
                | PendingSegment (SegmentId name, reps) ->
                    Array.concat [Text.Encoding.ASCII.GetBytes name; BitConverter.GetBytes reps]
                | PendingSequence (SequenceId name, reps) ->
                    Array.concat [Text.Encoding.ASCII.GetBytes name; BitConverter.GetBytes reps]

            /// Create the name of an experiment to store in the machine and to use as an internal
            /// reference point.  Returns a SequenceId, because an experiment is just a sequence.
            let private makeExperimentName (compressed : CompressedExperiment) =
                compressed.CompressedExperiment
                |> List.map elementToByteArray
                |> List.fold Array.append [||]
                |> hexHash
                |> SequenceId

            /// Convert a compressed experiment into an encoded one suitable for writing
            /// to the machine
            let private encode (compressed : CompressedExperiment) =
                let segments =
                    compressed.Segments
                    |> Map.toList
                    |> List.map zipSegment
                let sequences =
                    compressed.Sequences
                    |> Map.toList
                    |> List.map zipPendingSequence
                { Segments = segments
                  Sequences = sequences
                  Experiment =
                  { Name = makeExperimentName compressed
                    PendingSequence = compressed.CompressedExperiment } }

            /// Encode an experiment into a writeable form
            let toEncodedExperiment experiment = choice {
                let! verified = experiment |> metadata
                return
                    verified
                    |> compile
                    |> compress
                    |> encode }

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let private toRegularSequenceElement = function
                | PendingSegment (name, reps) -> Segment (StoredSegment name, reps)
                | PendingSequence (name, reps) -> Sequence (StoredSequence name, reps)

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let toRegularSequence (pending : PendingSequence) : Sequence = {
                Name = pending.Name
                Sequence = pending.PendingSequence |> List.map toRegularSequenceElement }

    /// Public functions for writing user-input experiments to the machine
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
            let! storedExperimentAsSequence =
                encoded.Experiment
                |> toRegularSequence
                |> storeSequence instrument
            let storedExperiment = StoredExperimentId storedExperimentAsSequence
            return {
                StoredExperiment = storedExperiment
                StoredSegments   = storedSegments
                StoredSequences  = storedSequences } }

        /// Play a stored experiment on the machine
        let playStoredExperiment instrument experiment = asyncChoice {
            let sequence = experiment |> experimentToStoredSequence
            do! playStoredSequence instrument sequence }

        /// Store an experiment on the machine, then begin playing it as soon as possible. Returns
        /// the stored experiment
        let playExperiment instrument experiment = asyncChoice {
            let! stored = storeExperiment instrument experiment
            do! playStoredExperiment instrument stored
            return stored }