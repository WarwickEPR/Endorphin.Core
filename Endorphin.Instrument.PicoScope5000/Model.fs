// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core.CommandRequestAgent

[<AutoOpen>]
/// PicoScope 5000 series high level model types.
module Model =
    
    /// Contains the serial number and API device handle for a PicoScope 5000 series.
    type internal PicoScope5000Identity = { SerialNumber : string ; Handle : int16 }

    /// Contains details of a PicoScope 5000 series connection and allows the user to
    /// perform commands and send requests to the device.
    type PicoScope5000 = internal PicoScope5000 of agent : CommandRequestAgent<PicoScope5000Identity>

    /// Toggle state indicating whether an option is enabled or disabled.
    type ToggleState = Enabled | Disabled

    /// Either minimum or maximum.
    type MinMax = Minimum | Maximum

    /// Indicates whether the driver thread was busy during an API call.
    type internal Availability = Available | Busy
    
    /// Time interval with unit of measure.
    type Interval =
        internal
        | Interval_fs of interval : int<fs>
        | Interval_ps of interval : int<ps>
        | Interval_ns of interval : int<ns>
        | Interval_us of inverval : int<us>
        | Interval_ms of interval : int<ms>
        | Interval_s  of interval : int<s>

        override interval.ToString() =
            match interval with
            | Interval_fs i -> sprintf "%d fs" i
            | Interval_ps i -> sprintf "%d ps" i
            | Interval_ns i -> sprintf "%d ns" i
            | Interval_us i -> sprintf "%d us" i
            | Interval_ms i -> sprintf "%d ms" i
            | Interval_s  i -> sprintf "%d s"  i

    /// Voltage with unit of measure.
    type Voltage = float32<V>
    
    /// Type alias for a 16-bit integer which is the format of all samples returned by the
    /// hardware.
    type AdcCount = int16
    
    /// Number of samples.
    type SampleCount = int

    /// Sample index in a buffer.
    type SampleIndex = uint32
    
    [<AutoOpen>]
    /// Model types related to channel settings.
    module ChannelSettings =

        /// Input channel.
        type InputChannel =
            | ChannelA
            | ChannelB
            | ChannelC
            | ChannelD

        /// Input channel peak-to-peak voltage range.
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

        /// Input channel coupling.
        type Coupling = AC | DC

        /// Input channel bandwidth.
        type Bandwidth = FullBandwidth | Bandwidth_20MHz

        /// All input settings for an enabled input channel.
        type InputSettings = 
            internal { Coupling       : Coupling
                       Range          : Range
                       AnalogueOffset : Voltage
                       BandwidthLimit : Bandwidth }

        /// Indicates whether a channel is enabled and contains its input settings if so.
        type ChannelSettings =
            internal
            | EnabledChannel of inputSettings : InputSettings
            | DisabledChannel

    [<AutoOpen>]
    /// Model types related to the hardware state.
    module Device =
        
        /// Power source used for a PicoScope 5000 series device.
        type PowerSource = MainsPower | UsbPower

        /// Front panel LED flash state for a PicoScope 5000 series device.
        type LedFlash =
            internal
            | LedOff
            | LedRepeat of counts : int16
            | LedIndefiniteRepeat

    [<AutoOpen>]
    /// Model types related to trigger settings.
    module Triggering =

        /// A channel which can be used to trigger acquisition.
        type TriggerChannel =
            internal
            | InputChannelTrigger of inputChannel : InputChannel
            | ExternalTrigger
            | AuxiliaryTrigger

        /// Trigger level threshold crossing direction required to trigger acquisition.
        type LevelThreshold =
            | Above
            | Below
            | Rising
            | Falling
            | RisingOrFalling

        /// Window threshold state required to trigger acquisition.
        type WindowThreshold =
            | Inside
            | Outside
            | Enter
            | Exit
            | EnterOrExit

        /// Auto-trigger delay.
        type AutoTriggerDelay = internal AutoTriggerDelay_ms of delay : int16<ms>

        /// Defines simple trigger settings, which are able to trigger acquisition on a voltage
        /// threshold crossing on a specified trigger channel, or optionally also trigger
        /// automatically after a specified delay.
        type SimpleTriggerSettings =
            internal { TriggerChannel     : TriggerChannel
                       AdcThreshold       : AdcCount
                       ThresholdDirection : LevelThreshold
                       StartSample        : SampleIndex
                       AutoTrigger        : AutoTriggerDelay option }

        /// Captures possible trigger settings for a PicoScope 5000 series device. Note that
        /// compound (advanced) trigger settings are not currently implemented.
        type TriggerSettings =
            internal
            | SimpleTrigger of settings : SimpleTriggerSettings
            | AutoTrigger of delay : AutoTriggerDelay
    
        /// Indicates whether triggering or pulse width qualifier mode are enabled.
        type TriggerStatus =
            internal { TriggerState             : ToggleState
                       PulseWidthQualifierState : ToggleState }

    [<AutoOpen>]
    /// Model types related to signal sampling and acquisition.
    module Acquisition =
        
        /// Vertical resolution, set for all input channels on the device. The PicoScope 5000
        /// series uses a variable resolution architecture which allows it to change the
        /// resolution between 8 and 16 bit in exchange for having fewer channels and/or a
        /// lower maximum sampling rate. 
        type Resolution =
            | Resolution_8bit
            | Resolution_12bit
            | Resolution_14bit
            | Resolution_15bit
            | Resolution_16bit

        /// Downsampling ratio used to reduce the data sampled by the oscilloscope before
        /// sending it to the computer.
        type DownsamplingRatio = uint32

        /// Device memory segment. Memory can be segmented to allow multiple captures to be
        /// stored on the oscilloscope before transferring them to the computer.
        type MemorySegment = uint32

        /// Timebase index, indicating the sample interval for acquisition. The relatinship
        /// between timebase index and sample interval depends on the current device
        /// resolution.
        type Timebase = uint32

        /// Timebase parameters, containing the maximum sample count which can be acquired
        /// from the device and the sample interval corresponding to a given timebase and
        /// resolution.
        type TimebaseParameters =
            internal { Timebase       : Timebase
                       Resolution     : Resolution
                       MaximumSamples : SampleCount 
                       SampleInterval : Interval } 
        
        /// Downsampling mode used to reduce data captured by the oscilloscope before sending
        /// it to the computer.
        type DownsamplingMode =
            | NoDownsampling
            | Averaged
            | Decimated
            | Aggregate

        /// Equivalent time sampling mode setting. In ETS mode, the PicoScope samples a repetitive
        /// signal by interleaving multiple captures to achieve smaller sample intervals.
        type EtsMode = 
            | NoEts
            | FastEts
            | SlowEts
        
        /// Specifies the sampling settings used to capture a particular input channel and transfer
        /// it to the computer.
        type InputSampling =
            internal { InputChannel     : InputChannel
                       DownsamplingMode : DownsamplingMode }

        /// Contains sampling settings for all input channels used in an acquisition.
        type AcquisitionInputs = 
            internal { InputSettings : Map<InputChannel, InputSettings>
                       InputSampling : Set<InputSampling> }
        
        /// The downsampling mode which was used to reduce the samples stored in a data buffer
        /// before sending them to the computer.
        type BufferDownsampling =  
            | NoDownsamplingBuffer
            | AveragedBuffer
            | DecimatedBuffer
            | AggregateBuffer of minMax : MinMax

        [<AutoOpen>]
        /// Model types relating to acquisition buffers.
        module internal Buffers =

            /// Single data buffer or buffer pair. The latter is used in aggregate downsampling mode
            /// which writes to two data buffers, containing the maximum and minimum samples within
            /// each downsampling window.
            type Buffer =
                | SingleBuffer of buffer : AdcCount array
                | BufferPair   of bufferMax : AdcCount array * bufferMin : AdcCount array

            /// Contains all buffers used during an acquisition.
            type AcquisitionBuffers =
                { Buffers       : Map<InputChannel * BufferDownsampling, AdcCount array>
                  MemorySegment : MemorySegment }
                 
            /// Indicates whether a trigger fired during a block of samples transferred from the device
            /// to the computer, and if so, its trigger position.
            type TriggerPosition =
                internal
                | NotTriggered
                | Triggered of triggerSample : SampleIndex
    
        [<AutoOpen>]
        /// Model types related to streaming acquisition.
        module Streaming =
            
            /// Stream stop settings: either manual or automatic.
            type StreamStop =
                internal
                | ManualStop
                | AutoStop of maxPreTriggerSamples : SampleIndex * maxPostTriggerSamples : SampleIndex

            /// Contains all parameters for a streaming acquisition.
            type StreamingParameters = 
                internal { Resolution        : Resolution
                           SampleInterval    : Interval
                           BufferLength      : SampleIndex
                           MemorySegment     : MemorySegment
                           TriggerSettings   : TriggerSettings
                           StreamStop        : StreamStop
                           DownsamplingRatio : DownsamplingRatio option
                           Inputs            : AcquisitionInputs }
            
            /// Contains information from a driver callback which indicates the location of the latest
            /// streaming acquisition samples in the data buffer.
            type internal StreamingValuesReady = 
                { NumberOfSamples  : SampleCount
                  StartIndex       : SampleIndex
                  VoltageOverflows : Set<InputChannel>
                  TriggerPosition  : TriggerPosition
                  DidAutoStop      : bool }
        
    /// Model types related to the integrated signal generator
    module SignalGenerator =
            
        /// Signal generator sweep direction.
        type SweepDirection = Up | Down | UpDown | DownUp

        /// Signal generator frequency sweep parameters.
        type FrequencySweepParameters =
            internal { StartFrequency     : float32<Hz>
                       StopFrequency      : float32<Hz>
                       FrequencyIncrement : float32<Hz>
                       DwellTime          : float32<s>
                       SweepDirection     : SweepDirection }
            
        /// Signal generator output frequency (either fixed or swept).
        type Frequency =
            internal 
            | FixedFrequency of frequency : float32<Hz>
            | FrequencySweep of parameters : FrequencySweepParameters

        /// Built-in signal generator waveform options.
        type BuiltInWaveform = 
            internal
            | Sine                  of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | Square                of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | Triangle              of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | DCVoltage             of                        offset : Voltage
            | RampUp                of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | RampDown              of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | Sinc                  of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | Gaussian              of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | HalfSine              of peakToPeak : Voltage * offset : Voltage * frequency : Frequency
            | WhiteNoise            of peakToPeak : Voltage * offset : Voltage
            | PseudoRandomBitStream of peakToPeak : Voltage * offset : Voltage * bitRate : float32<Hz>

        /// Signal generator playback mode.
        type PlaybackMode =
            internal
            | ContinuousPlayback
            | NumberOfCycles of cycles : uint32
            | NumberOfSweeps of sweeps : uint32

        /// Signal generator trigger.
        type SignalGeneratorTriggerType = Rising | Falling | GateHigh | GateLow

        /// Signal generator trigger source.
        type SignalGeneratorTriggerSource = 
            internal 
            | ScopeTrigger
            | ExternalTrigger of threshold : Voltage
            | SoftwareTrigger

        /// Signal generator trigger settings.
        type SignalGeneratorTriggerSettings =
            | AutoTrigger
            | SignalGeneratorTrigger of triggerSource : SignalGeneratorTriggerSource * triggerType : SignalGeneratorTriggerType

        /// Signal generator built-in waveform parameters.
        type BuiltInWaveformSettings =
            internal { Waveform        : BuiltInWaveform
                       PlaybackMode    : PlaybackMode
                       TriggerSettings : SignalGeneratorTriggerSettings }