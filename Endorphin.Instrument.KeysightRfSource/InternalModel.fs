﻿namespace Endorphin.Instrument.Keysight

[<AutoOpen>]
module internal InternalModel =
    [<AutoOpen>]
    module Waveform =
        /// Internal record of an entire recorded segment before being transformed into
        /// machine-readable strings.  Lists are in reverse order for speed.
        type EncodedSegment = {
            EncodedName    : string
            EncodedIQ      : byte array
            EncodedMarkers : byte array }

        /// Segment data after it has been encoded, including the lengths and data indicator '#'.
        /// Ready to write to machine.
        type EncodedSegmentFiles = {
            Waveform : byte array
            Markers  : byte array
            Header   : byte array }

    [<AutoOpen>]
    module RfPulse =
        /// A verified pulse, identical to the regular pulse, but we're sure that (for example)
        /// the number of pulses in each cycle are the same.
        type VerifiedPulse =
            | VerifiedRf of RfPulse
            | VerifiedDelay of DelayPulse
            | VerifiedTrigger of TriggerPulse
            | VerifiedMarker of MarkerPulse

        /// Metadata about the experiment gathered during verification, for use during the
        /// compilation step.
        type ExperimentMetadata = {
            /// How many times the experiment is repeated.
            ExperimentRepetitions : int
            /// How many pulses there are per phase.
            PulsesCount : int
            /// How many phases there are in the phase cycle.
            RfPhaseCount : int option }

        /// An experiment after it has been passed through the user-input verifier.
        type VerifiedExperiment = {
            Pulses : VerifiedPulse list
            Metadata : ExperimentMetadata }

        // No need for type aliases here because there's no other step which uses similar types
        /// A single pulse which can be easily converted into a single segment, for use after the
        /// compilation of the experiment and optimisation phases.
        type StaticPulse =
            | StaticRf      of phase : Phase * duration : SampleCount
            | StaticDelay   of duration : SampleCount
            | StaticTrigger of markers : Markers
            | StaticMarker  of markers : Markers * duration : SampleCount

        /// A list of samples and their repetitions, which could be easily written onto the
        /// machine, but likely with a lot of redundancy.
        type CompiledExperiment = {
            Data : (Sample * SampleCount) list
            Length : int }

        /// An element of a sequence where the dependencies are not yet written to the machine.
        /// Elements may still be pending writing, and not available for playback yet.  This should
        /// not be exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequenceElement =
            | PendingSegment of name : SegmentId * repetitions : uint16
            | PendingSequence of name : SequenceId * repetitions : uint16

        /// The data portion of a pending sequence.
        type PendingSequenceData = PendingSequenceElement list

        /// A sequence where the dependencies are not yet written to the machine. Elements may
        /// still be pending writing, and not available for playback yet.  This should not be
        /// exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequence = {
            Name : SequenceId
            PendingSequence : PendingSequenceData }

        /// An experiment inside the compression step.
        type CompressedExperiment = {
            Segments : Map<string, SegmentData>
            Sequences : Map<string, PendingSequenceData>
            SampleCount : int
            CompressedExperiment : PendingSequenceData }

        /// An assembled experiment, ready for storing onto the machine.
        type EncodedExperiment = {
            Segments : Segment list
            Sequences : PendingSequence list
            Experiment : PendingSequence }