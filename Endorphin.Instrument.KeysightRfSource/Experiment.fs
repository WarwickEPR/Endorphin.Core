﻿namespace Endorphin.Instrument.Keysight

open ARB
open Hashing
open ExtCore.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

module Experiment =
    /// Pre-computed coefficients for the FIR filter.
    let internal riseCoefficients =
        [| 0.028406470015011301; 0.26541468360571452; 0.73458531639428548; 0.9715935299849886 |]

    /// How many samples the FIR filter looks ahead when applying.
    let internal riseCount = Array.length riseCoefficients

    [<AutoOpen>]
    module Configure =
        /// The minimum spacing between two RF pulses, so that the low-pass filter has space to do its job
        /// without affecting the timings of pulses.
        let minimumRfPulseSeparation =
            riseCount
            |> (*) 2
            |> uint32
            |> SampleCount

        /// An experiment with no pulses, ready to be added to.
        let emptyExperiment = {
            Pulses = Seq.empty
            Repetitions = 1
            ShotRepetitionTime = DurationInSec 0.0<s> }

        /// A phase for use when we don't care what the phase actually is.
        let internal noPhase = PhaseInRad 0.0<rad>

        /// A phase cycle with no phases in it.
        let emptyPhaseCycle = PhaseCycle Array.empty

        /// Append a phase to a phase cycle.
        let addPhase phase (PhaseCycle cycle) =
            let cycle' = Array.create (cycle.Length + 1) noPhase
            cycle'.[0 .. cycle.Length - 1] <- cycle
            cycle'.[cycle.Length]          <- phase
            PhaseCycle cycle'

        /// Append a sequence of phases to a phase cycle.
        let addPhaseSequence phases (PhaseCycle cycle) =
            cycle
            |> Array.append (Array.ofSeq phases)
            |> PhaseCycle

        /// Append a pulse to an experiment.
        let private appendPulse pulse (experiment : Experiment) =
            { experiment with Pulses = Seq.appendSingleton pulse experiment.Pulses }

        /// Add an RF pulse to an experiment with an increment each repetition.
        let addRfPulseWithIncrement phases duration increment =
            appendPulse (Rf (phases, SampleCount duration, SampleCount increment))

        /// Add an RF pulse to an experiment with no increment.
        let addRfPulse phases duration = addRfPulseWithIncrement phases duration 0u

        /// Add a delay between pulses to the experiment, with an increment each repetition.
        let addDelayWithIncrement duration increment =
            appendPulse (Delay (SampleCount duration, SampleCount increment))

        /// Add a single delay pulse to an experiment, the same length each repetition.
        let addDelay duration = addDelayWithIncrement duration 0u

        /// Add a marker pulse with set markers and an incremement each repetition to an experiment.
        let addMarkerPulseWithIncrement markers duration increment =
            appendPulse (Marker (markers, SampleCount duration, SampleCount increment))

        /// Add a marker pulse with set markers to an experiment, which is the same length each repetition.
        let addMarkerPulse markers duration = addMarkerPulseWithIncrement markers duration 0u

        /// Add a single-sample trigger pulse on the given markers to an experiment.
        let addTrigger markers = addMarkerPulse markers 1u

        /// Set the number of repetitions of the experiment (i.e. how many times to apply each increment.
        let withRepetitions reps (experiment : Experiment) =
            { experiment with Repetitions = reps }

        /// Set the shot repetition time of the experiment.
        let withShotRepetitionTime time (experiment : Experiment) =
            { experiment with ShotRepetitionTime = DurationInSec time }

    /// Functions for translating human-readable experiment data into a machine-readable form.
    module internal Translate =
        /// Functions to check that a user-input experiment is in a valid form, and collect metadata
        /// for use in the compilation step.
        [<AutoOpen>]
        module private Verify =
            /// Check if a particular pulse is an RF pulse or not.
            let private isRfPulse = function
                | Rf _ -> true
                | _ -> false

            /// Get the length of an RF pulse's cycle.
            let private rfPulseCycleLength = function
                | Rf (PhaseCycle cycle, _, _) -> cycle.Length
                // if the rf filter has done its job, this should never trigger
                | _ -> failwith "Non-RF pulse made it through the RF-pulse filter"

            /// Check if a phase cycle length matches the passed length parameter.
            let private incorrectCycleLength length pulse = not (length = rfPulseCycleLength pulse)

            /// Check if a phase cycle has a non-zero length.
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

            /// Convert a pulse into a VerifiedPulse type. The calling function is assumed to have
            /// actually done the verification.
            let private toVerifiedPulse = function
                | Rf pulse -> VerifiedRf pulse
                | Delay pulse -> VerifiedDelay pulse
                | Marker pulse -> VerifiedMarker pulse

            /// Check that there is a valid number of phases in each cycle, and return their count
            /// as an option.
            let private countRfPhases (rfPulses : Pulse list) =
                if rfPulses.Length = 0 then
                    succeed None
                else
                    rfPulses
                    |> checkPhaseCycles
                    |> Choice.liftInsideOption

            /// Convert a duration in seconds into a SampleCount.
            let private shotRepetitionSampleCount (DurationInSec time) =
                if time < 0.0<s> then
                    fail "Must have a non-negative shot repetition time!"
                else
                    time / shortestPulseDuration
                    |> uint32
                    |> SampleCount
                    |> succeed

            /// Check that a valid number of repetitions of the experiment is set.
            let private checkRepetitions experiment =
                if experiment.Repetitions < 1 then fail "Must do the experiment at least once!"
                else succeed ()

            /// Get the duration of a pulse.
            let private pulseLength = function
                | Rf (_, SampleCount dur, _) -> dur
                | Delay (SampleCount dur, _) -> dur
                | Marker (_, SampleCount dur, _) -> dur

            /// Check that RF pulses are more than a set distance apart, so the low pass filter will
            /// work.  We only need to check the first iteration of the experiment, since the pulses
            /// can only get longer (SampleCount is unsigned).
            let private checkRfSpacing (experiment : Experiment) =
                let minimum = extractSampleCount minimumRfPulseSeparation
                let rec loop space = function
                    | [] -> succeed ()
                    | hd :: tl when isRfPulse hd ->
                        let minimum = extractSampleCount minimumRfPulseSeparation
                        if space < minimum then
                            minimum
                            |> sprintf "RF pulses must be at least %u samples apart for the low-pass filter."
                            |> fail
                        else
                            loop 0u tl
                    | hd :: tl ->
                        loop (space + (pulseLength hd)) tl
                loop minimum (List.ofSeq experiment.Pulses)

            /// Count the number of SampleCounts until the first RF pulse triggers.
            let private countUntilFirstRf pulses =
                let rec loop count = function
                    | [] -> None
                    | head :: _ when isRfPulse head -> Some count
                    | head :: tail -> loop (count + pulseLength head) tail
                loop 0u pulses

            /// Convert an experiment into a VerifiedExperiment.
            let private toVerifiedExperiment (experiment : Experiment) = choice {
                let! shotRepetitionTime = shotRepetitionSampleCount experiment.ShotRepetitionTime
                let shotRepPulse = Delay (shotRepetitionTime, SampleCount 0u)
                let pulses =
                    experiment.Pulses
                    |> Seq.appendSingleton shotRepPulse
                    |> List.ofSeq
                let rfPulses = pulses |> List.filter isRfPulse
                let! rfPhaseCount = countRfPhases rfPulses
                let pulses' =
                    match countUntilFirstRf pulses with
                    | None -> pulses
                    | Some count when count >= uint32 riseCount -> pulses
                    | Some count ->
                        let deadCount = SampleCount <| uint32 riseCount - count
                        Delay (deadCount, SampleCount 0u) :: pulses
                return {
                    Pulses = [ (pulses' |> List.map toVerifiedPulse) ]
                    Metadata =
                    { ExperimentRepetitions = experiment.Repetitions
                      PulseCount = pulses'.Length
                      RfPulseCount = rfPulses.Length
                      RfPhaseCount = rfPhaseCount
                      ShotRepetitionTime = shotRepetitionTime } } }

            /// Verify that the user-input experiment is valid and accumulate metadata.  Some examples
            /// of invalid experiments might be ones where phase cycles have different lengths.
            let verify experiment = choice {
                do! checkRepetitions experiment
                do! checkRfSpacing experiment
                return! toVerifiedExperiment experiment }

        /// Functions for compiling experiments into a form which can be more easily compressed.
        [<AutoOpen>]
        module private Compile =
            /// Create a list with n copies of the original list, where each list is mapped according
            /// to some mapping function.
            let private mapiListCopies n mapi list =
                let rec loop acc = function
                    | 0 -> acc
                    | i ->
                        let copy = List.map (List.map (mapi (i - 1))) list
                        loop (copy @ acc) (i - 1)
                loop [] n

            /// For each rf pulse, turn its PhaseCycle into a length 1 list including only the correct
            /// phase for that iteration of the sequence.
            let private chooseCorrectPhase index = function
                | VerifiedRf (PhaseCycle phases, dur, inc) ->
                    let phase = PhaseCycle [| phases.[index] |]
                    VerifiedRf (phase, dur, inc)
                | pulse -> pulse

            /// Expand an Experiment into a sequence of StaticPhasePulses, with the relevant phase
            /// in each RfPulse.
            let private expandPhaseCycle (experiment : VerifiedExperiment) =
                match experiment.Metadata.RfPhaseCount with
                | None -> experiment
                | Some n ->
                    let pulses =
                        experiment.Pulses
                        |> mapiListCopies n chooseCorrectPhase
                    { experiment with Pulses = pulses }

            /// Get the new duration in SampleCount for the nth repetition.
            let private duration (SampleCount dur) (SampleCount inc) n = SampleCount (dur + inc * n)

            /// Set the duration in a pulse to the correct duration for the repetition that we're on.
            /// The increment remains, but is safe to discard after this function.
            let private chooseCorrectDuration index pulse =
                let n = uint32 index
                match pulse with
                | VerifiedRf (phases, dur, inc) ->
                    VerifiedRf (phases, duration dur inc n, inc)
                | VerifiedDelay (dur, inc) ->
                    VerifiedDelay (duration dur inc n, inc)
                | VerifiedMarker (markers, dur, inc) ->
                    VerifiedMarker (markers, duration dur inc n, inc)

            /// Expand a sequence of pulses which still have increment data attached into a sequence of
            /// pulses with ignorable increments.  The increments are now encoded into the durations.
            let private expandRepetitions (experiment : VerifiedExperiment) =
                let reps = experiment.Metadata.ExperimentRepetitions
                let pulses =
                    experiment.Pulses
                    |> mapiListCopies reps chooseCorrectDuration
                { experiment with Pulses = pulses }

            /// Convert an experiment with pulses, increments and a phase cycle into a list of
            /// static pulses where each element is standalone.
            let private expandVariables experiment =
                experiment
                |> expandPhaseCycle
                |> expandRepetitions

            /// Return only the pulse sequence of a verified experiment.
            let private pulseSequence (experiment : VerifiedExperiment) = experiment.Pulses

            /// Convert a verified pulse to a static pulse. Assumes that the phase cycle is of length
            /// 1, but this should have been ensured by the expandVariables step.
            let private toStaticPulse = function
                | VerifiedRf (PhaseCycle phases, duration, _) ->
                    StaticRf (phases.[0], duration)
                | VerifiedDelay (duration, _) ->
                    StaticDelay (duration)
                | VerifiedMarker (markers, duration, _) ->
                    StaticMarker (markers, duration)

            /// Convert a verified experiment into a sequence of static pulses.
            let private toStaticPulses experiment =
                experiment
                |> expandVariables
                |> pulseSequence
                |> List.map (List.map toStaticPulse)

            /// A set of markers all turned off.
            let private noMarkers = { M1 = false; M2 = false; M3 = false; M4 = false }

            /// Expand a static pulse into a sample and a count of how many repetitions that sample has.
            let private expandStaticPulse pulse =
                let (amplitude, phase, markers, duration) =
                    match pulse with
                    | StaticRf (phase, dur)       -> (1.0, phase,   noMarkers, dur)
                    | StaticDelay (dur)           -> (0.0, noPhase, noMarkers, dur)
                    | StaticMarker (markers, dur) -> (0.0, noPhase, markers, dur)
                let sample =
                    defaultIqSample
                    |> withAmplitudeAndPhase amplitude phase
                    |> withMarkers markers
                (sample, duration)

            /// Count how many samples must be played until I or Q is next set to a non-zero value.
            let private countUntilRf samples =
                let rec loop acc = function
                    | [] -> None
                    | (sample, SampleCount dur) :: tail when sample.I = 0s && sample.Q = 0s ->
                        loop (acc + dur) tail
                    | _ -> Some acc
                loop 0u samples

            /// Take `count` number of samples off the list of sample * count.
            let private splitSamples count samples =
                let rec loop rem acc = function
                    | tail when rem = 0u -> ((List.rev acc), tail)
                    | [] -> failwith "Tried to split off more samples than were left."
                    | (sample, SampleCount dur) :: tail when dur > rem ->
                        let acc' = (sample, SampleCount rem) :: acc
                        let tail' = (sample, SampleCount (dur - rem)) :: tail
                        loop 0u acc' tail'
                    | (sample, SampleCount dur) :: tail ->
                        loop 0u ((sample, SampleCount dur) :: acc) tail
                loop count [] samples

            /// Separate a list of (sample, count) into a list where each sample has a count of 1.
            let private separateSampleList samples =
                let rec loop acc = function
                    | [] -> List.rev acc
                    | (sample, SampleCount dur) :: tail when dur > 1u ->
                        loop ((sample, SampleCount 1u) :: acc) ((sample, SampleCount (dur - 1u)) :: tail)
                    | (sample, _) :: tail ->
                        loop ((sample, SampleCount 1u) :: acc) tail
                loop [] samples

            let private updateSampleFirRise i q idx (sample, dur) =
                let sample' =
                    sample
                    |> withI (int16 (float i * riseCoefficients.[idx]))
                    |> withQ (int16 (float q * riseCoefficients.[idx]))
                (sample', dur)

            let private updateSampleFirFall i q idx (sample, dur) =
                let sample' =
                    sample
                    |> withI (int16 (float i * riseCoefficients.[riseCount - 1 - idx]))
                    |> withQ (int16 (float q * riseCoefficients.[riseCount - 1 - idx]))
                (sample', dur)

            let private applyRiseFir rise i q =
                rise
                |> separateSampleList
                |> List.mapi (updateSampleFirRise i q)

            let private applyFallFir fall i q =
                fall
                |> separateSampleList
                |> List.mapi (updateSampleFirFall i q)

            let private splitRise samples =
                let count = countUntilRf samples
                let (head, tail) =
                    match count with
                    | None   -> failwith "Tried to apply the FIR filter to too many RF pulses."
                    | Some s when s < uint32 riseCount ->
                        failwith "Got too close to an RF pulse without splitting it."
                    | Some s -> splitSamples (s - uint32 riseCount) samples
                let (rise, tail') = splitSamples (uint32 riseCount) tail
                (head, rise, tail')

            let private splitFall = function
                | [] -> failwith "Unexpectedly reached the last sample when applying the FIR filter."
                | pulse :: tail ->
                    let (fall, tail') = splitSamples (uint32 riseCount) tail
                    ([pulse], fall, tail')

            /// Apply an FIR filter to all IQ values in the pulse sequence.
            let private firFilter rfCount samples =
                let rec loop acc list = function
                    | 0 -> List.rev (list @ acc)
                    | idx ->
                        let (head, rise, tail) = splitRise list
                        let (pulse, fall, tail') = splitFall tail
                        let i = (fst (List.exactlyOne pulse)).I
                        let q = (fst (List.exactlyOne pulse)).Q
                        let rise' = applyRiseFir rise i q
                        let fall' = applyFallFir fall i q
                        let acc' =
                            [ fall'; pulse; rise'; head ]
                            |> List.map List.rev
                            |> List.concat
                            |> (fun list -> list @ acc)
                        loop acc' tail' (idx - 1)
                loop [] samples rfCount

            /// Add two SampleCounts together.
            let private addDurations (_, SampleCount one) (_, SampleCount two) = SampleCount (one + two)

            /// Check whether two samples are equal by comparing their hashes.
            let private equalSamples a b = (hexHash sampleToBytes a) = (hexHash sampleToBytes b)

            /// Given a list of samples and their durations, group any adjaacent samples which are
            /// equal and update the durations accordingly.
            let private groupEqualSamples samples =
                let folder acc el =
                    match acc with
                    | [] -> [el]
                    | y :: ys when equalSamples (fst el) (fst y) -> (fst y, addDurations y el) :: ys
                    | _ -> el :: acc
                samples
                |> List.fold folder []
                |> List.rev

            /// Convert a list of static pulses into a list of samples.
            let private toSamples rfCount pulses =
                pulses
                |> List.map expandStaticPulse
                |> firFilter rfCount
                |> groupEqualSamples

            /// Count the number of pulses in the experiment, and return the completed
            /// CompiledExperiment.
            let private toCompiledPoints list =
                let rec loop n = function
                    | [] -> n
                    | (_, SampleCount dur) :: tail -> loop (n + dur) tail
                { CompiledData = list; CompiledLength = loop 0u list }

            /// Compile an experiment into a direct list of samples which could (if they were written
            /// into one segment of a storable size) play back the entire experiment.
            let compile experiment =
                experiment
                |> toStaticPulses
                |> List.map (toSamples experiment.Metadata.RfPulseCount)
                |> List.map toCompiledPoints
                |> (fun points -> { ExperimentPoints = points; Metadata = experiment.Metadata })

        /// Functions to compress streams of pulses into waveform and sequence files.
        [<AutoOpen>]
        module private Compress =
            /// An empty compressed experiment, ready to be added to.
            let private emptyCompressedExperiment metadata = {
                Segments = Map.empty
                Sequences = Map.empty
                CompressedExperiments = List.empty
                Metadata = metadata }

            /// Get the quotient and remainder of how many times an integer may be divided by the
            /// maximum value in a uint16.
            let private intByUint16 num =
                (num / int UInt16.MaxValue, uint16 (num % int UInt16.MaxValue))

            /// Split an `int` SampleCount into parts of uint16, constructing enough sequence elements
            /// along the way to make up a whole integer.
            let private listToPrepend construct id curCount newCount =
                let (quotient, remainder) = intByUint16 newCount
                let maxReps = [ for _ in 1 .. quotient -> (construct id, UInt16.MaxValue) ]
                let overflow =
                    if int curCount + int remainder < int UInt16.MaxValue then
                        [ (construct id, curCount + remainder) ]
                    else
                        [ (construct id, curCount); (construct id, remainder) ]
                overflow @ maxReps

            /// Attempt to take "count" number of samples from the next repetition of samples in a
            /// compiled experiment.  If there aren't enough, then take as many as possible.  Returns
            /// the sample taken, the remaining compiled experiment, and the number of counts taken.
            let private takeCountFromNextElement list (count : uint32) (reps : uint32) =
                match list with
                | [] -> failwith "Tried to read too many elements during compression!"
                | (sample, SampleCount dur) :: tail when (count * reps) >= dur ->
                    (sample, tail, dur)
                | (sample, SampleCount dur) :: tail ->
                    (sample, (sample, SampleCount(dur - (count * reps))) :: tail, (count * reps))

            /// Get how many times the next sample in a compiled experiment is repeated.
            let nextSampleCount (list : (Sample * SampleCount) list) =
                list
                |> List.tryHead
                |> (function | Some s -> snd s; | None -> SampleCount 0u)
                |> (fun (SampleCount s) -> s)

            /// Split a compiled experiment into a CompressedElement, and a tail of
            /// CompiledExperiment, based on the passed number of samples to pick.
            let private splitCompiledExperiment count reps compiled =
                let rec loop acc input = function
                    | 0u ->
                        let segment = { Samples = Array.ofList <| List.rev acc; Length = (uint16 count) }
                        let id = hexHash segmentToBytes segment
                        ({ Element = (PendingSegment (SegmentId id), uint16 reps)
                           Segments = Map.add id segment Map.empty
                           Sequences = Map.empty },
                         { CompiledData = input
                           CompiledLength = compiled.CompiledLength - (count * reps) })
                    | rem ->
                        let (sample, input', taken) = takeCountFromNextElement input rem reps
                        let acc' = (sample, SampleCount (taken / reps)) :: acc
                        loop acc' input' (rem - (taken / reps))
                loop [] compiled.CompiledData count

            /// Choose how many points to take to make up the next CompressedElement.
            let private chooseNextLength compiled =
                if compiled.CompiledLength > minimumSegmentLength
                   && compiled.CompiledLength < (minimumSegmentLength * 2u)
                then
                    (compiled.CompiledLength, 1u)
                else
                    let nextLength = nextSampleCount compiled.CompiledData
                    let reps = nextLength / minimumSegmentLength
                    if reps <= 1u then
                        (minimumSegmentLength, 1u)
                    elif compiled.CompiledLength < (minimumSegmentLength * (reps + 1u)) then
                        (minimumSegmentLength, (reps - 1u))
                    else
                        (minimumSegmentLength, reps)

            /// Split a CompiledExperiment into a next CompressedElement, and an accordingly shortened
            /// CompiledExperiment.
            let private splitCompiledAtNextElement compiled =
                let (count, reps) = chooseNextLength compiled
                splitCompiledExperiment count reps compiled

            /// Get the string of an id from an element.
            let elementId (el, _) =
                match el with
                | PendingSegment (SegmentId id) -> id
                | PendingSequence (SequenceId id) -> id

            /// Find the most recent id - during the compression, the most recent one is
            /// first in the list, since the list is built up in reverse order.
            let private lastId experiment =
                match List.tryHead experiment with
                | None -> None
                | Some s ->
                    match List.tryHead s with
                    | None -> None
                    | Some el -> Some (elementId el)

            /// Increase the number of reps on the last segment in a CompressedExperiment.
            let private increaseLastReps compressed reps =
                let (curExperiment, tailExperiments) =
                    match compressed.CompressedExperiments with
                    | hd :: tl -> (hd, tl)
                    | _ -> failwith "Tried to increase the repetitions of a null sequence element!"
                let curExperiment' =
                    match curExperiment with
                    | (PendingSegment id, count) :: tail ->
                        (listToPrepend PendingSegment id count (int reps)) @ tail
                    | (PendingSequence id, count) :: tail ->
                        (listToPrepend PendingSequence id count (int reps)) @ tail
                    | _ -> failwith "Tried to increase the repetitions of a null sequence element!"
                { compressed with CompressedExperiments = (curExperiment' :: tailExperiments) }

            /// A joiner function for Map.join which just ignores clashes of keys, and returns the first
            /// value passed to it.
            let private ignoreKeyClash (_ : 'Key) (value : 'T) (_ : 'T) = value

            /// Prepend a different segment onto the passed compressed experiment.
            // Map.add is safe even if the segment already exists because it just doesn't do
            // anything in that case.
            let private consDifferentElement (compressed : CompressedExperiment) (element : CompressedElement) =
                let experiments =
                    match compressed.CompressedExperiments with
                    | [] -> [[element.Element]]
                    | hd :: tl -> (element.Element :: hd) :: tl
                { Segments = Map.join ignoreKeyClash compressed.Segments element.Segments
                  Sequences = Map.join ignoreKeyClash compressed.Sequences element.Sequences
                  CompressedExperiments = experiments
                  Metadata = compressed.Metadata }

            /// Get the number of repetitions from the given PendingSequenceElement.
            let private elementReps = snd

            /// Prepend some Segment onto a CompressedExperiment.
            let private consToExperiment compressed element =
                let id = elementId element.Element
                let reps = elementReps element.Element
                if Some id = lastId compressed.CompressedExperiments then
                    increaseLastReps compressed reps
                else
                    consDifferentElement compressed element

            /// One step in the compression.
            let rec private compressionLoop output input =
                match input.CompiledLength with
                | 0u ->
                    let experiments =
                        output.CompressedExperiments
                        |> List.map List.rev
                        |> List.rev
                    { output with CompressedExperiments = experiments }
                | _ ->
                    let (element, input') = splitCompiledAtNextElement input
                    let output' = consToExperiment output element
                    compressionLoop output' input'

            /// Join two compressed experiments into one by concatenating the experiment sequences
            /// and joining the maps.
            let private joinCompressedExperiments (acc : CompressedExperiment) (input : CompressedExperiment) =
                { Segments = Map.join ignoreKeyClash acc.Segments input.Segments
                  Sequences = Map.join ignoreKeyClash acc.Sequences input.Sequences
                  CompressedExperiments = input.CompressedExperiments @ acc.CompressedExperiments
                  Metadata = acc.Metadata }

            /// Compress the experiment down using a basic 60-sample dictionary method.
            let compress compiled =
                compiled.ExperimentPoints
                |> List.map (compressionLoop (emptyCompressedExperiment compiled.Metadata))
                |> List.reduce joinCompressedExperiments
                |> (fun a -> { a with CompressedExperiments = List.rev a.CompressedExperiments })

        /// Internal functions for encoding experiments from the user-input form to the writeable
        /// machine form of repeatable files.
        [<AutoOpen>]
        module Encode =
            /// Convert a PendingSequence element into a byte array of the name followed by the
            /// number of repetitions.
            let private elementToByteArray (el, reps : uint16) =
                Array.concat [Text.Encoding.ASCII.GetBytes (elementId (el, reps)); BitConverter.GetBytes reps]

            /// Get the number of bytes needed to encode one element.  Since names are created by
            /// the program, and the maximum size of the number of reps is constant, this function
            /// will return a constant.
            let private elementByteCount (el, reps : uint16) =
                (Text.Encoding.ASCII.GetByteCount (elementId (el, reps)))
                + (BitConverter.GetBytes reps).Length

            /// Create the name of an experiment to store in the machine and to use as an internal
            /// reference point.  Returns a SequenceId, because an experiment is just a sequence.
            let internal makeExperimentName (compressed : PendingSequence) =
                let byteCount = elementByteCount compressed.Head
                let arrLength = compressed.Length * byteCount
                let array = Array.create arrLength 0uy
                let rec loop n = function
                    | [] -> array
                    | el :: tail ->
                        let elementArray = elementToByteArray el
                        for i in 0 .. (byteCount - 1) do array.[n * byteCount + i] <- elementArray.[i]
                        loop (n + 1) tail
                compressed
                |> hexHash (loop 0)
                |> SequenceId

            /// Convert a compressed experiment into an encoded one suitable for writing
            /// to the machine.
            let private encode (compressed : CompressedExperiment) =
                let segments =
                    compressed.Segments
                    |> Map.toList
                    |> List.map (fun (str, seg) -> (SegmentId str, seg))
                let sequences =
                    compressed.Sequences
                    |> Map.toList
                    |> List.map (fun (str, sqn) -> (SequenceId str, sqn))
                let experiments =
                    compressed.CompressedExperiments
                    |> List.map makeExperimentName
                    |> (fun a -> List.zip a compressed.CompressedExperiments)
                { Segments = segments
                  Sequences = sequences
                  Experiments = experiments
                  Metadata = compressed.Metadata }

            /// Encode an experiment into a writeable form.
            let toEncodedExperiment experiment = choice {
                let! verified = experiment |> verify
                return
                    verified
                    |> compile
                    |> compress
                    |> encode }

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let private toRegularSequenceElement (el, reps) =
                match el with
                | PendingSegment  s -> (StoredSegment  s, reps)
                | PendingSequence s -> (StoredSequence s, reps)

            /// Convert a pending sequence element to a regular sequence element.  Should not be
            /// exposed to users because it's quite unsafe - we assume that the caller will be
            /// storing all of the dependencies at the same time.
            let toRegularSequence pending : Sequence =
                pending |> List.map toRegularSequenceElement

#if DEBUG
        /// Debugging functions for printing out experiments after they've been compiled.
        [<AutoOpen>]
        module Print =
            /// Depth of an indent.
            let private depth = 4

            /// Get a compiled experiment.
            let toCompiledExperiment experiment = asyncChoice {
                let! verified = verify experiment
                return verified |> compile }

            /// Get a compressed experiment.
            let toCompressedExperiment experiment = asyncChoice {
                let! verified = verify experiment
                return verified
                    |> compile
                    |> compress }

            /// Get a string of the indent level.
            let private getIndent indent = String.replicate indent " "

            /// Pretty-print out a sample.
            let printSample (indent : int) sample =
                printf "%s(%6d; %6d; %d%d%d%d)"
                    (getIndent indent)
                    sample.I
                    sample.Q
                    (Convert.ToInt32 sample.Markers.M1)
                    (Convert.ToInt32 sample.Markers.M2)
                    (Convert.ToInt32 sample.Markers.M3)
                    (Convert.ToInt32 sample.Markers.M4)

            /// Print out a (Sample * SampleCount) tuple.
            let private printSampleCount indent (smp, SampleCount count) =
                printSample indent smp
                printfn " * %d" count

            /// Pretty-print out a segment.
            let printSegment indent segment =
                segment.Samples |> Array.iter (printSampleCount indent)

            /// Pretty print a pending sequence.
            let rec printPendingSequence indent segMap seqMap sequence =
                let printEl = printPendingSequenceElement indent segMap seqMap
                sequence |> List.iter printEl
            and private printPendingSequenceElement indent segMap seqMap = function
                | (PendingSegment (SegmentId id), reps) ->
                    printfn "%s%s * %d" (getIndent indent) id reps
                    printSegment (indent + depth) (Map.find id segMap)
                | (PendingSequence (SequenceId id), reps) ->
                    printfn "%s%s * %d" (getIndent indent) id reps
                    printPendingSequence (indent + depth) segMap seqMap (Map.find id seqMap)

            /// Pretty-print out a compiled experiment.
            let printCompiledExperiment (compiled : CompiledExperiment) =
                let helper item = List.iter (printSampleCount 0) item.CompiledData
                List.iter helper compiled.ExperimentPoints

            /// Pretty-print out a compressed experiment.
            let printCompressedExperiment (compressed : CompressedExperiment) =
                let helper item =
                    printfn "\n%s" ((makeExperimentName item) |> (fun (SequenceId id) -> id))
                    printPendingSequence depth compressed.Segments compressed.Sequences item
                List.iter helper compressed.CompressedExperiments
#endif