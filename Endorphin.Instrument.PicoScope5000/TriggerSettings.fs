namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices

// this sequential StructLayout may result in non-verifiable IL code 
// when FieldOffset attributes are also used but not in this case
#nowarn "9"

[<AutoOpen>]
module TriggerSettings =
    /// Enumeration indicating the direction of singal change which is required for a trigger event to
    /// occur on a PicoScope 5000 series channel.
    type internal ThresholdDirectionEnum = 
        // values for level threshold mode
        | Above           = 0
        | Below           = 1
        | Rising          = 2
        | Falling         = 3
        | RisingOrFalling = 4
        // values for window threshold mode
        | Inside          = 0
        | Outside         = 1
        | Enter           = 2
        | Exit            = 3
        | EnterOrExit     = 4
        // none
        | None            = 2

    type LevelThreshold =
        | Above
        | Below
        | Rising
        | Falling
        | RisingOrFalling

    type WindowThreshold =
        | Inside
        | Outside
        | Enter
        | Exit
        | EnterOrExit

    let internal (|LevelThreshold|) =
        function
        | Above           -> ThresholdDirectionEnum.Above          
        | Below           -> ThresholdDirectionEnum.Below          
        | Rising          -> ThresholdDirectionEnum.Rising         
        | Falling         -> ThresholdDirectionEnum.Falling        
        | RisingOrFalling -> ThresholdDirectionEnum.RisingOrFalling
                
    let internal (|WindowThreshold|) = 
        function
        | Inside      -> ThresholdDirectionEnum.Inside     
        | Outside     -> ThresholdDirectionEnum.Outside    
        | Enter       -> ThresholdDirectionEnum.Enter      
        | Exit        -> ThresholdDirectionEnum.Exit       
        | EnterOrExit -> ThresholdDirectionEnum.EnterOrExit

    // the remaining enumerations in this file are present for completeness but not used in any of the PicoScope
    // API features implemented so far

    type internal PulseWidthTypeEnum =
        | None = 0
        | LessThan = 1
        | GreaterThan = 2
        | InRange = 3
        | OutOfRange = 4

    type internal TriggerStateEnum =
        | DontCare = 0
        | True = 1
        | False = 2

    type internal ThresholdModeEnum =
        | Level = 0
        | Window = 1

    [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
    type internal TriggerChannelProperties =
        struct 
            val ThresholdMajor : int16 
            val ThresholdMinor : int16
            val Hysteresis : uint16
            val Channel : ChannelEnum
            val ThresholdMode : ThresholdModeEnum

            new(thresholdMajor : int16,
                thresholdMinor : int16,
                hysteresis : uint16,
                channel : ChannelEnum,
                thresholdMode : ThresholdModeEnum) = 
                { ThresholdMajor = thresholdMajor
                  ThresholdMinor = thresholdMinor
                  Hysteresis = hysteresis
                  Channel = channel
                  ThresholdMode = thresholdMode }
        end

    [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
    type internal TriggerConditions = 
        struct
            val ChannelA : TriggerStateEnum
            val ChannelB : TriggerStateEnum
            val ChannelC : TriggerStateEnum
            val ChannelD : TriggerStateEnum;
            val External : TriggerStateEnum
            val Auxiliary : TriggerStateEnum
            val PulseWidthQualifier : TriggerStateEnum

            new(channelA : TriggerStateEnum,
                channelB : TriggerStateEnum,
                channelC : TriggerStateEnum,
                channelD : TriggerStateEnum,
                external : TriggerStateEnum,
                auxiliary : TriggerStateEnum,
                pulseWidthQualifier : TriggerStateEnum) =
                { ChannelA = channelA
                  ChannelB = channelB
                  ChannelC = channelC
                  ChannelD = channelD
                  External = external
                  Auxiliary = auxiliary
                  PulseWidthQualifier = pulseWidthQualifier }
        end

    [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
    type internal PulseWidthQualifierConditions =
        struct
            val ChannelA : TriggerStateEnum
            val ChannelB : TriggerStateEnum
            val ChannelC : TriggerStateEnum
            val ChannelD : TriggerStateEnum
            val External : TriggerStateEnum
            val Auxiliary : TriggerStateEnum

          new(channelA : TriggerStateEnum,
              channelB : TriggerStateEnum,
              channelC : TriggerStateEnum,
              channelD : TriggerStateEnum,
              external : TriggerStateEnum,
              auxiliary : TriggerStateEnum) =
              { ChannelA = channelA
                ChannelB = channelB
                ChannelC = channelC
                ChannelD = channelD
                External = external
                Auxiliary = auxiliary }
        end

    type AutoTriggerDelay = AutoTriggerDelayInMilliseconds of delay : int16<ms>

    let (|AutoTriggerDelay|) = 
        function
        | Some (AutoTriggerDelayInMilliseconds delay) -> int16 delay
        | None                                        -> 0s

    type SimpleTriggerSettings =
        { Channel            : InputChannel
          AdcThreshold       : AdcCount
          ThresholdDirection : LevelThreshold
          StartSample        : SampleIndex
          AutoTrigger        : AutoTriggerDelay option }

    type TriggerSettings =
        | SimpleTrigger of settings : SimpleTriggerSettings
        | AutoTrigger of delay : AutoTriggerDelay
    
    type TriggerStatus =
        { Trigger             : ToggleState
          PulseWidthQualifier : ToggleState }