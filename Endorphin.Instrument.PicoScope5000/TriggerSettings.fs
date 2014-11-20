namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

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

type ThresholdMode =
    | Level = 0
    | Window = 1

type TriggerPosition =
    | NotTriggered
    | Triggered of uint32

    static member FromTriggeredAndPosition(triggered, position) =
        if triggered <> 0s then Triggered(position)
        else NotTriggered


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

type SimpleTriggerSettings =
    { channel : Channel
      adcThreshold : int16
      thresholdDirection : ThresholdDirection
      delaySamplesAfterTrigger : uint32
      autoTrigger : int16<ms> option }

type TriggerSettings =
    | SimpleTrigger of settings : SimpleTriggerSettings
    | AutoTrigger of delay : int16<ms>
