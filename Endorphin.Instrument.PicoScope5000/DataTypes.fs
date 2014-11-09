namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices

type PicoInfo = 
    | DriverVersion = 0
    | UsbVersion = 1
    | HardwareVersion = 2
    | VariantInfo = 3
    | BatchAndSerial = 4
    | CalibrationDate = 5
    | KernelVersion = 6
    | DigitalHardwareVersion = 7
    | AnalogueHardwareVersion = 8
    | FirmwareVersion1 = 9
    | FirmwareVersion2 = 10

type Channel =
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | Ext = 4
    | Aux = 5
    | None = 6

type Coupling = 
    | DC = 0
    | AC = 1

type Range =
    | _10mV = 0
    | _20mV = 1
    | _50mV = 2
    | _100mV = 3
    | _200mV = 4
    | _500mV = 5
    | _1V = 6
    | _2V = 7
    | _5V = 8
    | _10V = 9
    | _20V = 10
    | _50V = 11

type TimeUnit =
    | Femtoseconds = 0
    | Picoseconds = 1
    | Nanoseconds = 2
    | Microseconds = 3
    | Milliseconds = 4
    | Seconds = 5

type SignalGeneratorWaveType =
    | Sine = 0
    | Square = 1
    | Triangle = 2
    | RampUp = 3
    | RampDown = 4
    | Sinc = 5
    | Gaussian = 6
    | HalfSine = 7
    | DcVoltage = 8
    | MAX_WAVE_TYPES = 9

type SignalGeneratorSweepType = 
    | Up = 0
    | Down = 1
    | UpDown = 2
    | DownUp = 3

type SignalGeneratorExtras =
    | None = 0
    | WhiteNoise = 1
    | PseudoRandomBitStream = 2

type SignalGeneratorTriggerType =
    | Rising = 0
    | Falling = 1
    | GateHigh = 2
    | GateLow = 3

type SignalGeneratorTriggerSource =
    | None = 0
    | Scope = 1
    | Auxiliary = 2
    | External = 3
    | Software = 4

type SignalGeneratorIndexMode =
    | Single = 0
    | Dual = 1
    | Quad = 2

type EtsMode =
    | Off = 0
    | Fast = 1
    | Slow = 2

type ThresholdMode =
    | Level = 0
    | Window = 1

type ThresholdDirection = 
    // Values for level threshold mode
    | Above = 0
    | Below = 1
    | Rising = 2
    | Falling = 3
    | RisingOrFalling = 4
    // Values for window threshold mode
    | Inside = 0
    | Outside = 1
    | Enter = 2
    | Exit = 3
    | EnterOrExit = 4
    // None
    | None = 2

type PulseWidthType =
    | None = 0
    | LessThan = 1
    | GreaterThan = 2
    | InRange = 3
    | OutOfRange = 4

type TriggerState =
    | DontCare = 0
    | True = 1
    | False = 2

type Resolution =
    | _8bit = 0
    | _12bit = 1
    | _14bit = 2
    | _15bit = 3
    | _16bit = 4

type ChannelInfo =
    | VoltageOffsetRanges = 0

type Downsampling =
    | None = 0
    | Aggregate = 1
    | Decimated = 2
    | Averaged = 4
    | Distribution = 8

type BandwidthLimit =
    | Full = 0
    | _20MHz = 1

type TriggerPosition =
    | NotTriggered
    | Triggered of uint32

    static member FromTriggeredAndPosition(triggered, position) =
        if triggered <> 0s then Triggered(position)
        else NotTriggered

type StreamStop =
    | ManualStop
    | AutoStop of maxPreTriggerSamples : uint32 * maxPostTriggerSamples : uint32

// this sequential StructLayout may result in non-verifiable IL code 
// when FieldOffset attributes are also used but not in this case
#nowarn "9"

open System.Runtime.CompilerServices
open System

[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type TriggerChannelProperties =
    struct 
        val ThresholdMajor : int16 
        val ThresholdMinor : int16
        val Hysteresis : uint16
        val Channel : Channel
        val ThresholdMode : ThresholdMode

        new(thresholdMajor : int16,
            thresholdMinor : int16,
            hysteresis : uint16,
            channel : Channel,
            thresholdMode : ThresholdMode) = 
            { ThresholdMajor = thresholdMajor
              ThresholdMinor = thresholdMinor
              Hysteresis = hysteresis
              Channel = channel
              ThresholdMode = thresholdMode }
    end

[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type TriggerConditions = 
    struct
        val ChannelA : TriggerState
        val ChannelB : TriggerState
        val ChannelC : TriggerState
        val ChannelD : TriggerState;
        val External : TriggerState
        val Auxiliary : TriggerState
        val PulseWidthQualifier : TriggerState

        new(channelA : TriggerState,
            channelB : TriggerState,
            channelC : TriggerState,
            channelD : TriggerState,
            external : TriggerState,
            auxiliary : TriggerState,
            pulseWidthQualifier : TriggerState) =
            { ChannelA = channelA
              ChannelB = channelB
              ChannelC = channelC
              ChannelD = channelD
              External = external
              Auxiliary = auxiliary
              PulseWidthQualifier = pulseWidthQualifier }
    end

[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type PulseWidthQualifierConditions =
    struct
        val ChannelA : TriggerState
        val ChannelB : TriggerState
        val ChannelC : TriggerState
        val ChannelD : TriggerState
        val External : TriggerState
        val Auxiliary : TriggerState

      new(channelA : TriggerState,
          channelB : TriggerState,
          channelC : TriggerState,
          channelD : TriggerState,
          external : TriggerState,
          auxiliary : TriggerState) =
          { ChannelA = channelA
            ChannelB = channelB
            ChannelC = channelC
            ChannelD = channelD
            External = external
            Auxiliary = auxiliary }
    end

type InputSettings = 
    { coupling : Coupling
      range : Range
      analogueOffset : float<V> }

type ChannelSettings =
    | Enabled of InputSettings
    | Disabled 

type SimpleTriggerSettings =
    { channel : Channel
      adcThreshold : int16
      thresholdDirection : ThresholdDirection
      delaySampleCount : uint32
      autoTriggerDelay : float<s> }

type TriggerSettings =
    | Simple of settings : SimpleTriggerSettings
    | Auto of autoTriggerDelay : float<s>

type PicoScopeBlockReady = 
    // handle, status, state -> unit 
    delegate of int16 * int16 * nativeint -> unit

type StreamingValuesReady = 
    { numberOfSamples : int
      startIndex : uint32
      voltageOverflows : Channel list
      triggerPosition : TriggerPosition
      didAutoStop : bool }

type PicoScopeStreamingReady =
    // handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

type PicoScopeDataReady =
    // handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit

[<Extension>]
type Methods() =
    static let MaximumNumberOfChannelsForResolution =
        function
        | Resolution._8bit | Resolution._12bit | Resolution._14bit -> 4
        | Resolution._15bit -> 2
        | Resolution._16bit -> 1
        | _ -> failwith "Unexpected resolution."

    static let GetFastestStreamingIntervalInNanosec(resolution : Resolution, channelCount : int) = 
        match (resolution, channelCount) with
        | (Resolution._12bit, 4)
        | (Resolution._12bit, 3)
        | (Resolution._14bit, 4)
        | (Resolution._14bit, 3) -> 256.0
        | (Resolution._8bit, 4) 
        | (Resolution._8bit, 3) 
        | (Resolution._12bit, 2) 
        | (Resolution._14bit, 2) 
        | (Resolution._15bit, 2) -> 128.0
        | (Resolution._8bit, 3)
        | (Resolution._12bit, 1)
        | (Resolution._14bit, 1)
        | (Resolution._15bit, 1)
        | (Resolution._16bit, 2) -> 64.0
        | (Resolution._8bit, 1) -> 32.0
        | (resolution, channelCount) when channelCount > MaximumNumberOfChannelsForResolution resolution ->
            failwith "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
        | parameters -> failwith "Unexpected number of channels or resolution: %A." parameters

    [<Extension>]
    static member ToVolts(range : Range) =
        match range with
        | Range._10mV -> 0.010<V>
        | Range._20mV -> 0.020<V>
        | Range._50mV -> 0.050<V>
        | Range._100mV -> 0.100<V>
        | Range._200mV -> 0.200<V>
        | Range._500mV -> 0.500<V>
        | Range._1V -> 1.0<V>
        | Range._2V -> 2.0<V>
        | Range._5V -> 5.0<V>
        | Range._10V -> 10.0<V>
        | Range._20V -> 20.0<V>
        | Range._50V -> 50.0<V>
        | _ -> failwith "Unexpected range."

    [<Extension>]
    static member FastestTimebase(resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 0u
        | Resolution._12bit -> 1u
        | Resolution._14bit
        | Resolution._15bit -> 3u
        | Resolution._16bit -> 4u
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member MaximumNumberOfChannels(resolution : Resolution) =
        MaximumNumberOfChannelsForResolution resolution

    [<Extension>]
    static member FastestStreamingIntervalInNanosec(resolution : Resolution, channelCount : int) =
        GetFastestStreamingIntervalInNanosec(resolution, channelCount)

    [<Extension>]
    static member IntegerValue(resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 8
        | Resolution._12bit -> 12
        | Resolution._14bit -> 14
        | Resolution._15bit -> 15
        | Resolution._16bit -> 16
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member BitMask(resolution : Resolution) =
        // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
        // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
        // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
        // correspond to the device resolution are 1s and the lowest order bits are 0s.
        ~~~ ((1s <<< (16 - Methods.IntegerValue(resolution))) - 1s)