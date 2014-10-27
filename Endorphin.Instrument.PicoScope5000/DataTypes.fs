﻿namespace Endorphin.Instrument.PicoScope5000

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

type DownSamplingMode =
    | None = 0 
    | Aggregate = 1

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

type Triggered =
    | NotTriggered
    | Triggered of uint32

// this sequential StructLayout may result in non-verifiable IL code 
// when FieldOffset attributes are also used but not in this case
#nowarn "9"

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

type PicoScopeBlockReady = 
    // handle, status, state -> void
    delegate of int16 * int16 * nativeint -> unit

type PicoScopeStreamingReady =
    // handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

type PicoScopeDataReady =
    // handle, numberOfSamples, overflows, triggeredAt, triggered, state
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit