namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Endorphin.Core.NationalInstruments
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// Model of the possible configurations of a Keysight RF source

[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Instrument =
        type RfSource = internal RfSource of Visa.IVisa

        type DeviceId =
            { Manufacturer : string
              ModelNumber : string
              SerialNumber : string
              Version : string }

        type ModelNumber = N5172B

        type Error = { Code : int ; Message : string }
   
    [<AutoOpen>]
    module Quantities =
        type Amplitude = PowerInDbm of float<dBm>
        type Frequency = FrequencyInHz of float<Hz>

        type Phase =
            | PhaseInRad of float<rad>
            | PhaseInDeg of float<deg>

        /// A list of phases to cycle through
        type PhaseCycle = PhaseCycle of Phase list

        type Duration = DurationInSec of float<s>
        type Percentage = Percentage of float<pct>
        type DecibelRatio = DecibelRatio of float<dB>
        
        type Impedance =
            | Impedance_50Ohm
            | Impedance_600Ohm
            | Impedance_1MOhm

    [<AutoOpen>]
    module General = 
        type Direction = Up | Down
        type Coupling = AC | DC
        type OnOffState = On | Off
        type AutoManualState = Auto | Manual
        type Polarity = Positive | Negative
        type FunctionShape =
            | Sine
            | Triangle
            | Square
            | Ramp of polarity : Polarity

    [<AutoOpen>]
    module Triggering =
        type ExternalTriggerSource =
            | Trigger1
            | Trigger2
            | Pulse

        type InternalTriggerSource =
            | PulseVideo
            | PulseSync

        type TriggerSourceType =
            | ImmediateType
            | TriggerKeyType
            | BusType
            | ExternalType
            | InternalType
            | TimerType

        type TriggerSource =
            | Immediate
            | TriggerKey
            | Bus
            | External of source : ExternalTriggerSource * polarity : Polarity
            | Internal of source : InternalTriggerSource
            | Timer of period : Duration

        type TriggerType = StepTrigger | ListTrigger

    [<AutoOpen>]
    module Modulation =
        // Unique Sources
        type ExternalInput = EXT1 | EXT2
        type FunctionGenerator = Function1

        // Modulation paths
        type AmPath = AM1 | AM2
        type FmPath = FM1 | FM2

        type Depth =
            | Linear of depth : Percentage
            | Exponential of depth : DecibelRatio

        let depthInPercentage (depth : Percentage) = Linear depth
        let depthInDecibels (depth : DecibelRatio) = Exponential depth

        type AmSettings = { Depth : Depth }
        type FmSettings = { Deviation : Frequency }

        // Source Settings
        type ExternalSettings =
            { Coupling : Coupling
              Impedance : Impedance }

        type FunctionSettings =
            { Shape : FunctionShape
              Frequency : Frequency
              PhaseOffset : Phase }
        
        type Source = 
            | ExternalSource of port : ExternalInput * settings : ExternalSettings
            | InternalSource of generator : FunctionGenerator * settings : FunctionSettings

        // Modulations have a set path, settings and source which will have its own settings

        type Modulation =
            | AmplitudeModulation of path : AmPath * settings : AmSettings * source : Source
            | FrequencyModulation of path : FmPath * settings : FmSettings * source : Source

        type ModulationSettings = Modulation list
 
         // Extract just the modulation channel from a Modulation
        type ModulationChannel =
            | AmChannel of path : AmPath
            | FmChannel of path : FmPath
   
        let modulationChannel =
            function
            | AmplitudeModulation (path, _, _) -> AmChannel path
            | FrequencyModulation (path, _, _) -> FmChannel path

        // Extract just the signal source
        type SourceProvider =
            | ExternalPort of port : ExternalInput
            | InternalGenerator of generator : FunctionGenerator

        let modulationSource =
            function
            | AmplitudeModulation (_, _, source)
            | FrequencyModulation (_, _, source) -> source

        let sourceProvider =
            function
            | ExternalSource (port,_) -> ExternalPort port
            | InternalSource (generator,_) -> InternalGenerator generator



    [<AutoOpen>]
    module Waveforms =
        type WaveformId = WaveformId of string 

        let internal waveformIdString (WaveformId name) = name
        let internal parseWaveformId = WaveformId

    [<AutoOpen>]
    module Sweep =
        type SweepMode = Fixed | Swept

        type StepSpacing = LinearStepSpacing | LogarithmicStepSpacing
        type SweepType = List | Step

        // Sweep ranges can be in both directions. Direction toggles whether start or end comes first.
        type Range<'T> = { Start : 'T; Stop : 'T }
        let range a b = { Range.Start=a ; Range.Stop=b }

        type FrequencySweep =
            | FrequencySweep of range : Range<Frequency>
            | FixedFrequency of frequency : Frequency

        type AmplitudeSweep =
            | AmplitudeSweep of range : Range<Amplitude>
            | FixedAmplitude of amplitude : Amplitude

        type SweepOptions = {
            Direction : Direction
            StepTrigger : TriggerSource
            ListTrigger : TriggerSource
            DwellTime : Duration option
            Retrace : OnOffState
            AttentuationProtection : OnOffState
            Mode : AutoManualState }

        type StepSweep = {
            Frequency : FrequencySweep
            Amplitude : AmplitudeSweep
            Points : int
            Spacing : StepSpacing
            Options : SweepOptions }

        type Sweep =
            | NoSweep of frequency : Frequency * amplitude : Amplitude
            | StepSweep of sweep : StepSweep

    [<AutoOpen>]
    module Waveform =
        /// A record of the 4 markers' states
        type Markers = {
            M1 : bool
            M2 : bool
            M3 : bool
            M4 : bool }

        /// A single IQ point with associated markers
        type Sample = {
            I       : int16
            Q       : int16
            Markers : Markers }

        /// The identifier of a segment, stored as a string
        type SegmentId = SegmentId of string
        /// The identifier of a sequence
        type SequenceId = SequenceId of string

        /// A single segment in the machine.  Must be at least 60 samples long
        type Segment = {
            Name : SegmentId
            Data : Sample seq } // Sequence of points

        /// Representation of the stored segments on the machine
        type StoredSegment = internal StoredSegment of name : SegmentId
        /// Representation of the stored sequences on the machine
        type StoredSequence = internal StoredSequence of name : SequenceId

        /// An element in a machine sequence can either be a segment (waveform or markers),
        /// or another sequence.  Both can have a number of repetitions associated with them.
        type SequenceElement =
            | Segment of segment : StoredSegment * repetitions : uint16
            | Sequence of sequence : StoredSequence * repetitions : uint16

        /// A full sequence to be stored in the machine
        type Sequence = {
            Name : SequenceId
            Sequence : SequenceElement list }

        [<AutoOpen>]
        module internal Translate =
            [<AutoOpen>]
            module Encode =
                /// A four-byte array for each encoded IQ point
                type EncodedIQ = byte []
                /// A byte with the four markers encoded in
                type EncodedMarkers = byte

                /// A single point encoded into the four-byte array of IQ points and the marker byte
                type EncodedSample = {
                    IQ      : EncodedIQ
                    Markers : EncodedMarkers }

                /// Internal record of an entire recorded segment before being transformed into
                /// machine-readable strings.  Lists are in reverse order for speed.
                type EncodedSegment = {
                    Name    : byte []
                    IQ      : EncodedIQ list
                    Markers : EncodedMarkers list }

                /// Segment data after it has been encoded, including the lengths and data indicator '#'.
                /// Ready to write to machine.
                type EncodedSegmentFiles = {
                    Waveform : byte []
                    Markers  : byte []
                    Header   : byte [] }

    [<AutoOpen>]
    module RfPulse =
        [<AutoOpen>]
        module Configure =
            /// A number of samples, generally used as a pulse duration
            type SampleCount = SampleCount of int

            // Define some type aliases for the pulse types so that it's simple to update the model
            // when new pulses are added, particularly with regards to Pulse/VerifiedPulse system.
            /// A single rf pulse as a tuple of (phases, duration, increment)
            type RfPulse = PhaseCycle * SampleCount * SampleCount
            /// A single delay pulse as a tuple of (duration, increment)
            type DelayPulse = SampleCount * SampleCount
            /// A single trigger pulse trigger a set of markers
            type TriggerPulse = Markers
            /// A single marker pulse as a tuple of (markers, duration, increment)
            type MarkerPulse = Markers * SampleCount * SampleCount

            /// A pulse with its varying parameters also attached, for use in defining experiments
            type Pulse =
                | Rf      of RfPulse
                | Delay   of DelayPulse
                | Trigger of TriggerPulse
                | Marker  of MarkerPulse

            /// A whole experiment, ready to be compiled and optimised
            type Experiment = Experiment of pulses : Pulse seq * repetitions : uint16

            /// ID string of an experiment
            type ExperimentId = ExperimentId of string

        [<AutoOpen>]
        module internal Encode =
            /// A verified pulse, identical to the regular pulse, but we're sure that (for example)
            /// the number of pulses in each cycle are the same.
            type VerifiedPulse =
                | VerifiedRf of RfPulse
                | VerifiedDelay of DelayPulse
                | VerifiedTrigger of TriggerPulse
                | VerifiedMarker of MarkerPulse

            /// Metadata about the experiment gathered during verification, for use during the
            /// compilation step
            type ExperimentMetadata = {
                ExperimentRepetitions : uint16
                RfCount : int
                RfPhaseCount : int }

            /// An experiment after it has been passed through the user-input verifier.
            type VerifiedExperiment = {
                Pulses : VerifiedPulse seq
                Metadata : ExperimentMetadata }

            // No need for type aliases here because there's no other step which uses similar types
            /// A single pulse which can be easily converted into a single segment, for use after the
            /// compilation of the experiment and optimisation phases
            type StaticPulse =
                | StaticRf      of phase : Phase * duration : SampleCount
                | StaticDelay   of duration : SampleCount
                | StaticTrigger of markers : Markers
                | StaticMarker  of markers : Markers * duration : SampleCount

            /// A list of samples and their repetitions, which could be easily written onto the
            /// machine, but likely with a lot of redundancy.
            type CompiledExperiment = CompiledExperiment of (Sample * SampleCount) seq

            /// An element of a sequence where the dependencies are not yet written to the machine.
            /// Elements may still be pending writing, and not available for playback yet.  This should
            /// not be exposed publically, to prevent accidentally depending on an unwritten file.
            type PendingSequenceElement =
                | PendingSegment of name : SegmentId * repetitions : uint16
                | PendingSequence of name : SequenceId * repetitions : uint16

            /// A sequence where the dependencies are not yet written to the machine. Elements may
            /// still be pending writing, and not available for playback yet.  This should not be
            /// exposed publically, to prevent accidentally depending on an unwritten file.
            type PendingSequence = {
                Name : SequenceId
                Sequence : PendingSequenceElement list }

            /// An assembled experiment, ready for storing onto the machine
            type EncodedExperiment = {
                Name : ExperimentId
                Segments : Segment seq
                Sequences : PendingSequence seq
                Experiment : PendingSequence }

        [<AutoOpen>]
        module Control =
            /// The data associated with a stored experiment - its name and dependencies
            type StoredExperiment = {
                Id : ExperimentId
                Segments : StoredSegment []
                Sequences : StoredSequence [] }

    type KeysightRfSettings = {
        Sweep : Sweep
        Modulation : Modulation list }