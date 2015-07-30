namespace Endorphin.Instrument.Keysight

[<AutoOpen>]
module internal InternalModel =
    [<AutoOpen>]
    module Waveform =
        /// Internal record of an entire recorded segment before being transformed into
        /// machine-readable strings.  Lists are in reverse order for speed.
        type EncodedSegment = {
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

        /// Where the next point of the experiment should be triggered from.
        type ExperimentTriggerSource =
            | SourceInternal
            | SourceExternal

        /// Metadata about the experiment gathered during verification, for use during the
        /// compilation step.
        type ExperimentMetadata = {
            /// How many times the experiment is repeated.
            ExperimentRepetitions : int
            /// How many pulses there are per phase.
            PulsesCount : int
            /// How many phases there are in the phase cycle.
            RfPhaseCount : int option
            /// Where the next point of the experiment should be triggered from.
            TriggerSource : ExperimentTriggerSource
            /// How many times to run the experiment at each increment and phase.
            ShotsPerPoint : int }

        // No need for type aliases here because there's no other step which uses similar types
        /// A single pulse which can be easily converted into a single segment, for use after the
        /// compilation of the experiment and optimisation phases.
        type StaticPulse =
            | StaticRf      of phase : Phase * duration : SampleCount
            | StaticDelay   of duration : SampleCount
            | StaticTrigger of markers : Markers
            | StaticMarker  of markers : Markers * duration : SampleCount

        /// An experiment after it has been passed through the user-input verifier.
        type VerifiedExperiment = {
            Pulses : VerifiedPulse list
            Separator : Option<StaticPulse list>
            Metadata : ExperimentMetadata }

        /// A list of samples and their repetitions, which could be easily written onto the
        /// machine, but likely with a lot of redundancy.
        type CompiledExperiment = {
            CompiledData : (Sample * SampleCount) list
            CompiledLength : int }

        /// An element of a sequence where the dependencies are not yet written to the machine.
        /// Elements may still be pending writing, and not available for playback yet.  This should
        /// not be exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequenceElement =
            | PendingSegment of name : SegmentId * repetitions : uint16
            | PendingSequence of name : SequenceId * repetitions : uint16

        /// A sequence where the dependencies are not yet written to the machine. Elements may
        /// still be pending writing, and not available for playback yet.  This should not be
        /// exposed publically, to prevent accidentally depending on an unwritten file.
        type PendingSequence = PendingSequenceElement list

        /// An experiment inside the compression step.
        type CompressedExperiment = {
            Segments : Map<string, Segment>
            Sequences : Map<string, PendingSequence>
            SampleCount : int
            CompressedExperiment : PendingSequence }

        /// An assembled experiment, ready for storing onto the machine.
        type EncodedExperiment = {
            Segments : (SegmentId * Segment) list
            Sequences : (SequenceId * PendingSequence) list
            Experiment : (SequenceId * PendingSequence) }