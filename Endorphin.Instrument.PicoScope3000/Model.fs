namespace Endorphin.Instrument.PicoScope3000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core.CommandRequestAgent

[<AutoOpen>]
module Model =
    /// Contains the serial number and API device handle for a PicoScope 3000 series.
    type internal PicoScope3000Identity = { SerialNumber : string ; Handle : int16}

    /// Contains details of a PicoScope 3000 series connection and allows the user to
    /// perform commands and send requests to the device.
    type PicoScope3000 = internal PicoScope3000 of agent : CommandRequestAgent<PicoScope3000Identity>

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

    /// Voltage with unit of measure.
    type Voltage = internal Voltage_V of voltage : float32<V>

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
        /// Digital port selection.  Port 0 corresponds to digital channels 0 to 7, and port 1 corresponds
        /// to channels 8 to 15.
        type DigitalPort =
            | DigitalPort0
            | DigitalPort1

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
        /// Power source used for a PicoScope 3000 series device.
        type PowerSource = MainsPower | UsbPower

        /// Front panel LED flash state for a PicoScope 3000 series device.
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

        /// Which threshold to use on a level threshold trigger.
        type LevelThresholdType = Upper | Lower

        /// Trigger level threshold crossing direction required to trigger acquisition.
        type LevelThreshold =
            | Above   of LevelThresholdType
            | Below   of LevelThresholdType
            | Rising  of LevelThresholdType
            | Falling of LevelThresholdType
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

        /// Captures possible trigger settings for a PicoScope 3000 series device. Note that
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
                internal { SampleInterval    : Interval
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