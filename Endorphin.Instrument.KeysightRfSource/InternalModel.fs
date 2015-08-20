namespace Endorphin.Instrument.Keysight

[<AutoOpen>]
module internal InternalModel =
    [<AutoOpen>]
    module ARB =
        /// Internal record of an entire recorded segment before being transformed into
        /// machine-readable strings.  Lists are in reverse order for speed.
        type EncodedSegment = {
            EncodedIQ      : byte array
            EncodedMarkers : byte array }

        /// Segment data after it has been encoded, including the lengths and data indicator '#'.
        /// Ready to write to machine.
        type EncodedSegmentFiles = {
            Waveform : byte array
            Markers  : byte array }

    [<AutoOpen>]
    module Experiment =
        /// A verified pulse, identical to the regular pulse, but we're sure that (for example)
        /// the number of pulses in each cycle are the same.
        type VerifiedPulse =
            | VerifiedRf of RfPulse
            | VerifiedDelay of DelayPulse
            | VerifiedMarker of MarkerPulse

        /// Metadata about the experiment gathered during verification, for use during the
        /// compilation step.
        type ExperimentMetadata = {
            /// How many times the experiment is repeated.
            ExperimentRepetitions : int
            /// How many pulses there are per phase.
            PulseCount : int
            /// How many pulses are RF pulses.
            RfPulseCount : int
            /// How many phases there are in the phase cycle.
            RfPhaseCount : int option
            /// How long to wait before firing the next shot.
            ShotRepetitionTime : SampleCount }

        // No need for type aliases here because there's no other step which uses similar types
        /// A single pulse which can be easily converted into a single segment, for use after the
        /// compilation of the experiment and optimisation phases.
        type StaticPulse =
            | StaticRf      of phase : Phase * duration : SampleCount
            | StaticDelay   of duration : SampleCount
            | StaticMarker  of markers : Markers * duration : SampleCount

        /// An experiment after it has been passed through the user-input verifier.
        type VerifiedExperiment = {
            Pulses : VerifiedPulse list list
            // double list because we're going to expand into individual lists
            Metadata : ExperimentMetadata }

        type CompiledExperimentPoint = {
            CompiledData : (Sample * SampleCount) list
            CompiledLength : uint32 }

        /// A list of samples and their repetitions, which could be easily written onto the
        /// machine, but likely with a lot of redundancy.
        type CompiledExperiment = {
            ExperimentPoints : CompiledExperimentPoint list
            Metadata : ExperimentMetadata }

        /// A waveform identifier pointing to a waveform which has not yet been written to disk.
        type PendingWaveform =
            | PendingSegment of name : SegmentId
            | PendingSequence of name : SequenceId

        /// An element of a sequence where the dependencies are not yet written to the machine.
        /// Elements may still be pending writing, and not available for playback yet.  This should
        /// not be exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequenceElement = PendingWaveform * uint16

        /// A sequence where the dependencies are not yet written to the machine. Elements may
        /// still be pending writing, and not available for playback yet.  This should not be
        /// exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequence = PendingSequenceElement list

        /// One element of compression - usually analagous to a single point in the experiment,
        /// with one phase and one duration for the pulses.
        type CompressedElement = {
            Element : PendingSequenceElement
            Segments : Map<string, Segment>
            Sequences : Map<string, PendingSequence> }

        /// An experiment inside the compression step.
        type CompressedExperiment = {
            Segments : Map<string, Segment>
            Sequences : Map<string, PendingSequence>
            CompressedExperiments : PendingSequence list
            Metadata : ExperimentMetadata }

        /// An assembled experiment, ready for storing onto the machine.
        type EncodedExperiment = {
            Segments : (SegmentId * Segment) list
            Sequences : (SequenceId * PendingSequence) list
            Experiments : (SequenceId * PendingSequence) list
            Metadata : ExperimentMetadata }