namespace Endorphin.Instrument.Keysight

open Endorphin.Core.NationalInstruments
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Model of the possible configurations of a Keysight RF source
[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Instrument =
        /// An opened and connected RF source, which can have commands written to it.
        type RfSource = internal RfSource of VisaInstrument

        /// A record of the identification information a device provides.
        type DeviceId = {
            Manufacturer : string
            ModelNumber : string
            SerialNumber : string
            Version : string }

        /// Model numbers that are recognised by the program.
        type ModelNumber = N5172B

        /// A returned error, including its code and the associated message.
        type Error = { Code : int ; Message : string }
   
    [<AutoOpen>]
    module Quantities =
        /// An absolute amplitude of a signal, given as a float type with units of dBm.
        type Amplitude = PowerInDbm of float<dBm>
        /// A frequency for a signal, given as a float type with units of Hz.
        type Frequency = FrequencyInHz of float<Hz>

        /// A phase for an I/Q signal, either in radians or degrees.
        type Phase =
            | PhaseInRad of float<rad>
            | PhaseInDeg of float<deg>

        /// A list of phases to cycle through.
        type PhaseCycle = PhaseCycle of Phase list

        /// A duration that something lasts for.
        type Duration = DurationInSec of float<s>
        /// A percentage of a total.
        type Percentage = Percentage of float<pct>
        /// A relative amplitude, measured in dB rather than dBm.
        type DecibelRatio = DecibelRatio of float<dB>
        
        /// Impedances that the machine can operate at.
        type Impedance =
            | Impedance_50Ohm
            | Impedance_600Ohm
            | Impedance_1MOhm

    [<AutoOpen>]
    module General = 
        /// A direction of something, often a range in a sweep.
        type Direction = Up | Down
        /// A state of coupling, either to AC or to DC.
        type Coupling = AC | DC
        /// A toggle state, where something can either be On or Off.
        type OnOffState = On | Off
        /// An automatic or manual state, for different types of control.
        type AutoManualState = Auto | Manual
        /// Polarity of something, either Positive or Negative.
        type Polarity = Positive | Negative
        /// The shape of a function, for use in the function generator.
        type FunctionShape =
            | Sine
            | Triangle
            | Square
            | Ramp of polarity : Polarity

    [<AutoOpen>]
    module Triggering =
        /// Where to source an external trigger from.
        type ExternalTriggerSource =
            | Trigger1
            | Trigger2
            | Pulse

        /// Where to source an internal trigger from.
        type InternalTriggerSource =
            | PulseVideo
            | PulseSync

        /// What type the trigger source should be.
        type TriggerSourceType =
            | ImmediateType
            | TriggerKeyType
            | BusType
            | ExternalType
            | InternalType
            | TimerType

        /// A complete type of a trigger source.
        type TriggerSource =
            | Immediate
            | TriggerKey
            | Bus
            | External of source : ExternalTriggerSource * polarity : Polarity
            | Internal of source : InternalTriggerSource
            | Timer of period : Duration

        /// The type of trigger, either step or list.
        type TriggerType = StepTrigger | ListTrigger

    [<AutoOpen>]
    module Modulation =
        // Unique Sources
        /// Which channel to listen for external input on.
        type ExternalInput = EXT1 | EXT2
        /// Which channel to use for the function generator.
        type FunctionGenerator = Function1

        /// Which amplitude modulation path to use.
        type AmPath = AM1 | AM2
        /// Which frequency modulation path to use.
        type FmPath = FM1 | FM2

        /// A depth of something, either linear as a percentage, or exponential, as a decibel ratio.
        type Depth =
            | Linear of depth : Percentage
            | Exponential of depth : DecibelRatio

        /// Create a depth measured linearly as a percentage.
        let depthInPercentage (depth : Percentage) = Linear depth
        /// Create a depth measured exponentially as a decibel ratio.
        let depthInDecibels (depth : DecibelRatio) = Exponential depth

        /// Settings for amplitude modulation.
        type AmSettings = { Depth : Depth }
        /// Settings for frequency modulation.
        type FmSettings = { Deviation : Frequency }

        /// Settings for an external source.
        type ExternalSettings = {
            Coupling : Coupling
            Impedance : Impedance }

        /// Settings for a function generator source.
        type FunctionSettings = {
            Shape : FunctionShape
            Frequency : Frequency
            PhaseOffset : Phase }

        /// A source of a signal, either external or internal.
        type Source = 
            | ExternalSource of port : ExternalInput * settings : ExternalSettings
            | InternalSource of generator : FunctionGenerator * settings : FunctionSettings

        /// Modulations have a set path, settings and source which will have its own settings
        type Modulation =
            | AmplitudeModulation of path : AmPath * settings : AmSettings * source : Source
            | FrequencyModulation of path : FmPath * settings : FmSettings * source : Source

        /// A list of modulations to apply as settings.
        type ModulationSettings = Modulation list
 
        /// Extract just the modulation channel from a Modulation
        type ModulationChannel =
            | AmChannel of path : AmPath
            | FmChannel of path : FmPath
 
        /// Get the channel which is being modulated.
        let modulationChannel = function
            | AmplitudeModulation (path, _, _) -> AmChannel path
            | FrequencyModulation (path, _, _) -> FmChannel path

        /// The location of a source.
        type SourceProvider =
            | ExternalPort of port : ExternalInput
            | InternalGenerator of generator : FunctionGenerator

        /// Get the source of a modulation.
        let modulationSource = function
            | AmplitudeModulation (_, _, source)
            | FrequencyModulation (_, _, source) -> source

        /// Find the location which is providing a source.
        let sourceProvider = function
            | ExternalSource (port,_) -> ExternalPort port
            | InternalSource (generator,_) -> InternalGenerator generator

    [<AutoOpen>]
    module Sweep =
        /// The mode to operate the sweep in, either fixed or swept.
        type SweepMode = Fixed | Swept

        /// The type of spacing to use between sweep steps, either linear or logarithmic.
        type StepSpacing = LinearStepSpacing | LogarithmicStepSpacing
        /// The type of sweep to use, either list or step.
        type SweepType = List | Step

        /// A range of values, with a begin value and an end value.
        type Range<'T> = { Start : 'T; Stop : 'T }
        /// Create a range between two values.
        let range a b = { Range.Start = a ; Range.Stop = b }

        /// A sweep through some set frequencies, either through a range of frequencies, or just at
        /// a constant frequency.
        type FrequencySweep =
            | FrequencySweep of range : Range<Frequency>
            | FixedFrequency of frequency : Frequency

        /// A sweep through some set amplitudes, either through a range of amplitudes, or just at
        /// a constant amplitude.
        type AmplitudeSweep =
            | AmplitudeSweep of range : Range<Amplitude>
            | FixedAmplitude of amplitude : Amplitude

        /// Settings for a sweep, encapsulating all options understandable by the machine.
        type SweepOptions = {
            Direction : Direction
            StepTrigger : TriggerSource
            ListTrigger : TriggerSource
            DwellTime : Duration option
            Retrace : OnOffState
            AttentuationProtection : OnOffState
            Mode : AutoManualState }

        /// A completely represented sweep, including the frequency to sweep, the amplitude to sweep,
        /// how many points to sweep across, the spacings between them, and any associated options.
        type StepSweep = {
            Frequency : FrequencySweep
            Amplitude : AmplitudeSweep
            Points : int
            Spacing : StepSpacing
            Options : SweepOptions }

        /// A sweep to run, either a full sweep, or a simple constant signal at a set frequency and
        /// amplitude.
        type Sweep =
            | NoSweep of frequency : Frequency * amplitude : Amplitude
            | StepSweep of sweep : StepSweep

    [<AutoOpen>]
    module Waveform =
        /// A record of the 4 markers' states.
        type Markers = {
            M1 : bool
            M2 : bool
            M3 : bool
            M4 : bool }

        /// A single IQ point with associated markers.
        type Sample = {
            I       : int16
            Q       : int16
            Markers : Markers }

        /// The identifier of a segment, stored as a string.
        type SegmentId = SegmentId of string
        /// The identifier of a sequence.
        type SequenceId = SequenceId of string

        /// The data portion of a Segment.
        // Can just be a type alias, but allows nice semantic use in Map<>
        type SegmentData = Sample array

        /// A single segment in the machine.  Must be at least 60 samples long.
        type Segment = {
            Name : SegmentId
            Data : SegmentData } // Sequence of points

        /// Representation of the stored segments on the machine.
        type StoredSegment = internal StoredSegment of name : SegmentId
        /// Representation of the stored sequences on the machine.
        type StoredSequence = internal StoredSequence of name : SequenceId

        /// An element in a machine sequence can either be a segment (waveform or markers),
        /// or another sequence.  Both can have a number of repetitions associated with them.
        type SequenceElement =
            | Segment of segment : StoredSegment * repetitions : uint16
            | Sequence of sequence : StoredSequence * repetitions : uint16

        /// The data portion of a sequence.
        type SequenceData = SequenceElement list

        /// A full sequence to be stored in the machine.
        type Sequence = {
            Name : SequenceId
            Sequence : SequenceData }

    [<AutoOpen>]
    module RfPulse =
        /// A number of samples, generally used as a pulse duration.
        type SampleCount = SampleCount of int

        // Define some type aliases for the pulse types so that it's simple to update the model
        // when new pulses are added, particularly with regards to Pulse/VerifiedPulse system.
        /// A single rf pulse as a tuple of (phases, duration, increment).
        type RfPulse = PhaseCycle * SampleCount * SampleCount
        /// A single delay pulse as a tuple of (duration, increment).
        type DelayPulse = SampleCount * SampleCount
        /// A single trigger pulse trigger a set of markers.
        type TriggerPulse = Markers
        /// A single marker pulse as a tuple of (markers, duration, increment).
        type MarkerPulse = Markers * SampleCount * SampleCount

        /// A pulse with its varying parameters also attached, for use in defining experiments.
        type Pulse =
            | Rf      of RfPulse
            | Delay   of DelayPulse
            | Trigger of TriggerPulse
            | Marker  of MarkerPulse

        /// A whole experiment, ready to be compiled and optimised.
        type Experiment = Experiment of pulses : Pulse seq * repetitions : uint16

        /// The id of a stored experiment.
        type StoredExperimentId = StoredExperimentId of StoredSequence

        /// The data associated with a stored experiment - its name and dependencies.
        type StoredExperiment = {
            StoredExperiment : StoredExperimentId
            StoredSegments   : StoredSegment []
            StoredSequences  : StoredSequence [] }

    /// A complete record of settings for the Keysight box, based on the sweep/modulation model.
    type KeysightRfSettings = {
        Sweep : Sweep
        Modulation : Modulation list }