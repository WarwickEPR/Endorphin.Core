namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core.CommandRequestAgent

[<AutoOpen>]
module Model =
    type ToggleState = Enabled | Disabled
    type MinMax = Minimum | Maximum
    type internal Availability = Available | Busy
    
    type Interval =
        internal
        | IntervalInFemtoseconds of interval : int<fs>
        | IntervalInPicoseconds  of interval : int<ps>
        | IntervalInNanoseconds  of interval : int<ns>
        | IntervalInMicroseconds of inverval : int<us>
        | IntervalInMilliseconds of interval : int<ms>
        | IntervalInSeconds      of interval : int<s>

    type Voltage = internal VoltageInVolts of voltage : float32<V>
    type AdcCount = int16 // only alias int16 for performance
                
    type SampleCount = int
    type SampleIndex = uint32
    
    [<AutoOpen>]
    module ChannelSettings =
        type InputChannel =
            | ChannelA
            | ChannelB
            | ChannelC
            | ChannelD

        type Range =
            | Range_10mV
            | Range_20mV
            | Range_50mV
            | Range_100mV
            | Range_200mV
            | Range_500mV
            | Range_1V
            | Range_2V
            | Range_5V
            | Range_10V
            | Range_20V
            | Range_50V

        type Coupling = AC | DC
        type Bandwidth = FullBandwidth | Bandwidth_20MHz

        type InputSettings = 
            internal { Coupling       : Coupling
                       Range          : Range
                       AnalogueOffset : Voltage
                       BandwidthLimit : Bandwidth }

        type ChannelSettings =
            internal
            | EnabledChannel of inputSettings : InputSettings
            | DisabledChannel

    [<AutoOpen>]
    module Device =
        type internal PicoScope5000Identity = { SerialNumber : string ; Handle : int16 }
        type PicoScope5000 = internal PicoScope5000 of agent : CommandRequestAgent<PicoScope5000Identity>
        type PowerSource = MainsPower | UsbPower

        type DeviceInfo =
            | DriverVersion           
            | UsbVersion              
            | HardwareVersion         
            | ModelNumber             
            | SerialNumber            
            | CalibrationDate         
            | KernelVersion           
            | DigitalHardwareVersion  
            | AnalogueHardwareVersion 
            | FirmwareVersion1        
            | FirmwareVersion2        

        type LedFlash =
            internal
            | LedOff
            | LedRepeat of counts : int16
            | LedIndefiniteRepeat

    [<AutoOpen>]
    module Triggering =
        type TriggerChannel =
            internal
            | InputChannelTrigger of inputChannel : InputChannel
            | ExternalTrigger
            | AuxiliaryTrigger

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

        type AutoTriggerDelay = AutoTriggerDelayInMilliseconds of delay : int16<ms>

        type SimpleTriggerSettings =
            internal { TriggerChannel     : TriggerChannel
                       AdcThreshold       : AdcCount
                       ThresholdDirection : LevelThreshold
                       StartSample        : SampleIndex
                       AutoTrigger        : AutoTriggerDelay option }

        type TriggerSettings =
            internal
            | SimpleTrigger of settings : SimpleTriggerSettings
            | AutoTrigger of delay : AutoTriggerDelay
    
        type TriggerStatus =
            internal { TriggerState             : ToggleState
                       PulseWidthQualifierState : ToggleState }

    [<AutoOpen>]
    module Acquisition =
        type Resolution =
            | Resolution_8bit
            | Resolution_12bit
            | Resolution_14bit
            | Resolution_15bit
            | Resolution_16bit

        type DownsamplingRatio = uint32
        type MemorySegment = uint32

        type Timebase = uint32

        type TimebaseParameters =
            internal { Timebase       : Timebase
                       Resolution     : Resolution
                       MaximumSamples : SampleCount 
                       SampleInterval : Interval } 
                
        type DownsamplingMode =
            | NoDownsampling
            | Averaged
            | Decimated
            | Aggregate

        type EtsMode = 
            | NoEts
            | FastEts
            | SlowEts
                        
        type InputSampling =
            internal { InputChannel     : InputChannel
                       DownsamplingMode : DownsamplingMode }

        type AcquisitionInputs = 
            internal { InputSettings : Map<InputChannel, InputSettings>
                       InputSampling : Set<InputSampling> }
        
        type BufferDownsampling =  
            | NoDownsamplingBuffer
            | AveragedBuffer
            | DecimatedBuffer
            | AggregateBuffer of minMax : MinMax

        [<AutoOpen>]
        module internal Buffers =
            type Buffer =
                | SingleBuffer of buffer : AdcCount array
                | BufferPair   of bufferMax : AdcCount array * bufferMin : AdcCount array

            type AcquisitionBuffers =
                { Buffers       : Map<InputChannel * BufferDownsampling, AdcCount array>
                  MemorySegment : MemorySegment }
                  
            type TriggerPosition =
                internal
                | NotTriggered
                | Triggered of triggerSample : SampleIndex
    
        [<AutoOpen>]
        module Streaming =
            type StreamStop =
                internal
                | ManualStop
                | AutoStop of maxPreTriggerSamples : SampleIndex * maxPostTriggerSamples : SampleIndex

            type StreamingParameters = 
                internal { Resolution        : Resolution
                           SampleInterval    : Interval
                           BufferLength      : SampleIndex
                           MemorySegment     : MemorySegment
                           TriggerSettings   : TriggerSettings
                           StreamStop        : StreamStop
                           DownsamplingRatio : DownsamplingRatio option
                           Inputs            : AcquisitionInputs }
            
            type internal StreamingValuesReady = 
                { NumberOfSamples  : SampleCount
                  StartIndex       : SampleIndex
                  VoltageOverflows : Set<InputChannel>
                  TriggerPosition  : TriggerPosition
                  DidAutoStop      : bool }
