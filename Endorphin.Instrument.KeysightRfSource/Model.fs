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
    module IQData =
        type Endianness =
            | LittleEndian
            | BigEndian

        /// A single IQ point with associated markers and endianness
        type Point = {
            I : int16
            Q : int16
            Marker1 : bool
            Marker2 : bool
            Marker3 : bool
            Marker4 : bool
            Order : Endianness } // Included the order field so we don't introduce problems by assuming the host is a certain order

        // Sample doesn't need a filename associated with it because it should always be in a sequence
        /// A single sample of a waveform - can be constant or part of e.g. a sin wave, or any other form.
        type Sample = Point array

        /// A total waveform can be either a sequence of samples or of subsequences.  A waveform must have
        /// at least 60 samples in it, but since these could be repeated, it is not necessary to store them all.
        type WaveformElement =
            internal
            | Sample of sample : Sample * repetitions : uint16
            | Element of element : WaveformElement * repetitions : uint16

        type Waveform = internal {
            Name : byte array // ASCII string of file name
            Elements : WaveformElement list }

        /// Bitwise representation of the markers associated with an IQ point
        type Markers = byte

        /// Sample data after it has been encoded, with no lengths or filenames in the order
        /// IQ * Markers
        type EncodedSample = byte array * byte array

        type EncodedElement = byte array * byte array

        /// Temporary type for collections of encoded samples
        type EncodedWaveform = {
            Name : byte array // ASCII string of file name
            IQ : byte array
            Markers : byte array }

        /// Sequence data after it has been encoded, including the lengths and data indicator '#'.
        /// Ready to write to machine.
        type EncodedWaveformFile = {
            WaveformFileString : byte array
            MarkerFileString : byte array
            HeaderFileString : byte array }

    type KeysightRfSettings = {
        Sweep : Sweep
        Modulation : Modulation list }