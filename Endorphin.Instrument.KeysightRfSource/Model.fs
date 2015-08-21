namespace Endorphin.Instrument.Keysight

open Endorphin.Core.NationalInstruments
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Model of the possible configurations of a Keysight RF source.
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
        type PhaseCycle = internal PhaseCycle of Phase array

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

        /// A completely represented step sweep, including the frequency to sweep, the amplitude to
        /// sweep, how many points to sweep across, the spacings between them, and any associated
        /// options.
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

        /// A sweep file that has been stored in the machine.
        type StoredSweep = StoredSweep of string

    [<AutoOpen>]
    module Route =
        /// Generic interface inherited by all other output signals.
        type ISignal = interface end

        /// An output signal which is able to be sent through the user output BNCs.
        type IUserSignal = interface inherit ISignal end
        /// An output signal which is able to be sent through the Sweep Out BNC.
        type ISweepOutSignal = interface inherit ISignal end
        /// An output signal which is able to be sent through the Trigger1 or Trigger2 BNCs.
        type ITriggerSignal = interface inherit ISignal end
        /// A signal which is either a marker channel, or no channel.
        type IMarkerSignal = interface inherit ISignal end
        /// A signal input from one of the specifically user-controlled BNCs.
        type IUserBncSignal = interface inherit ISignal end

        /// Type to denote that no output signal is being sent through a connector.
        type NoSignal =
            | NoSignal
            interface IUserSignal
            interface ISweepOutSignal
            interface ITriggerSignal
            interface IMarkerSignal
            interface IUserBncSignal

        /// A marker signal output.
        type UserSignalMarker =
            | RouteMarker1
            | RouteMarker2
            | RouteMarker3
            | RouteMarker4
            interface IUserSignal
            interface IMarkerSignal

        /// The signal present on the 29th pin of the auxiliary connector.
        type UserSignalAux =
            | RouteAux29
            interface IUserSignal

        /// Output signals automatically generated by the system, all of which may be sent through
        /// the system output BNCs.
        type SystemSignalCommon =
            | RouteSourceSettled
            | RoutePulseVideo
            | RoutePulseSync
            | RouteSweptFunctionDone
            interface ISweepOutSignal
            interface ITriggerSignal

        /// A signal which may only be routed through the Sweep Out BNC.
        type SystemSignalSweepOut =
            | RouteSweepOut
            | RouteSweepRun
            interface ISweepOutSignal

        /// A signal which may only be routed through the two Trigger BNCs.
        type SystemSignalTrigger =
            | RouteSweepTriggerOut
            | RouteLxi
            | RoutePulseBnc
            | RouteOtherTrigger
            interface ITriggerSignal

        /// A signal coming from one of the user-controlled BNCs.
        type UserBnc =
            | RouteBasebandTrigger1
            | RouteBasebandTrigger2
            | RouteEvent1
            | RoutePatternTrigger
            interface IUserBncSignal

        /// A complete set of output routings to write to the machine.
        type internal OutputRouting = {
            BbTrig1  : IUserSignal
            BbTrig2  : IUserSignal
            Event1   : IUserSignal
            PatTrig  : IUserSignal
            SweepOut : ISweepOutSignal
            Trig1    : ITriggerSignal
            Trig2    : ITriggerSignal }

        /// A complete set of input routings to write to the machine.
        type internal InputRouting = {
            PatTrig1 : IUserBncSignal
            PatTrig2 : IUserBncSignal }

        /// A set of internal routings for marker channels.  The same marker can be routed both
        /// internally and externally simultaneously.
        type internal InternalRouting = {
            AltAmplitude : IMarkerSignal
            AlcHold      : IMarkerSignal
            RfBlank      : IMarkerSignal }

        /// A complete set of routings for the machine.
        type Routing = internal {
            Output   : OutputRouting
            Input    : InputRouting
            Internal : InternalRouting }

    [<AutoOpen>]
    module ARB =
        /// A record of the 4 markers' states.
        type Markers = internal {
            M1 : bool
            M2 : bool
            M3 : bool
            M4 : bool }

        /// A single IQ point with associated markers.
        type Sample = internal {
            I       : int16
            Q       : int16
            Markers : Markers }

        /// A number of samples, generally used as a pulse duration.
        type internal SampleCount = SampleCount of uint32

        /// The identifier of a segment, before it has been written to the machine.
        type internal SegmentId = SegmentId of string
        /// The identifier of a sequence, before it has been written to the machine.
        type internal SequenceId = SequenceId of string

        /// A waveform that has been stored in the machine.
        type StoredWaveform = 
            internal
            | StoredSegment of SegmentId
            | StoredSequence of SequenceId

        /// A single segment in the machine.  Must be at least 60 samples long.
        type Segment = internal {
            Samples : (Sample * SampleCount) array
            Length  : uint16 }

        /// An element in a machine sequence can either be a segment (waveform or markers),
        /// or another sequence.  Both can have a number of repetitions associated with them.
        type internal SequenceElement = StoredWaveform * uint16

        /// A full sequence to be stored in the machine.
        type Sequence = internal SequenceType of SequenceElement list

        /// A unified type representing some playable waveform on the machine.
        type Waveform =
            internal
            | Segment of Segment
            | Sequence of Sequence

        /// A state which can either be low or high.
        type LowHighState = Low | High

        /// The mode of a continuous trigger in the dual ARB system.  "Free" means that playback begins
        /// immediately once the ARB is turned on, without waiting for a trigger, then repeats that
        /// waveform until something tells it to stop, and ignore subsequent triggers.  "Trigger" does
        /// the same, but waits for an initial trigger.  "Reset" is like "Trigger", but subsequent triggers
        /// reset the waveform to the beginning.
        type ArbContinuousMode =
            | ArbContinuousFree
            | ArbContinuousTrigger
            | ArbContinuousReset

        /// The mode of a segment advance type trigger in the dual ARB system.  "Single" means that on
        /// trigger, the next segment in the sequence plays once, ignoring the repetition count listed in
        /// its sequence.  Sequence repetitions are NOT ignored.  "Continuous" means that the segment plays
        /// in a loop until the next trigger moves it on.  This also ignores the repetition count of the
        /// segment.
        type ArbSegmentAdvanceMode =
            | ArbSegmentAdvanceSingle
            | ArbSegmentAdvanceContinuous

        /// The behaviour of the system when a second trigger is received while in single trigger mode.
        /// "No retrigger" ignores all subsequent triggers. "Buffered retrigger" plays the segment again
        /// once it has finished. "Restart retrigger" immediately resets the segment to the beginning and
        /// starts it.
        type ArbRetriggerMode =
            | NoRetrigger
            | BufferedRetrigger
            | RestartRetrigger

        /// The type of triggering to use for the dual ARB system.
        type ArbTriggerMode =
            | ArbContinuous of mode : ArbContinuousMode
            | ArbSingle of repeats : uint16 * retrigger : ArbRetriggerMode
            | ArbGate of polarity : LowHighState
            | ArbSegmentAdvance of mode : ArbSegmentAdvanceMode

        /// Physical location of the external source for the dual ARB system triggering.
        type ArbExternalConnector = ArbBnc | ArbAux

        /// The source to use to trigger the dual ARB system.
        type ArbTriggerSource =
            | ArbKey
            | ArbBus
            | ArbExternal of
                connector : ArbExternalConnector * polarity : Polarity option * delay : Duration option

        /// Complete triggering information for the dual ARB system.
        type ArbTrigger = ArbTrigger of mode : ArbTriggerMode * source : ArbTriggerSource

    [<AutoOpen>]
    module Experiment =
        // Define some type aliases for the pulse types so that it's simple to update the model
        // when new pulses are added, particularly with regards to Pulse/VerifiedPulse system.
        /// A single rf pulse as a tuple of (phases, duration, increment).
        type internal RfPulse = PhaseCycle * SampleCount * SampleCount
        /// A single delay pulse as a tuple of (duration, increment).
        type internal DelayPulse = SampleCount * SampleCount
        /// A single marker pulse as a tuple of (markers, duration, increment).
        type internal MarkerPulse = Markers * SampleCount * SampleCount

        /// A pulse with its varying parameters also attached, for use in defining experiments.
        type Pulse =
            internal
            | Rf      of RfPulse
            | Delay   of DelayPulse
            | Marker  of MarkerPulse

        /// A whole experiment, ready to be compiled and optimised.
        type Experiment = internal {
            Pulses : Pulse seq
            Repetitions : int
            ShotRepetitionTime : Duration }

        /// The data associated with a stored experiment - its name and dependencies.
        type StoredExperiment = {
            StoredExperiments: StoredWaveform array
            StoredWaveforms  : StoredWaveform array
            RfBlankRoute     : UserSignalMarker }

    /// A complete record of settings for the Keysight box, based on the sweep/modulation model.
    type KeysightRfSettings = {
        Sweep : Sweep
        Modulation : Modulation list }