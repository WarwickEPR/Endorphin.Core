// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

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
    [<CustomEquality; CustomComparison>]
    type Interval =
        internal
        | Interval_fs of interval : int<fs>
        | Interval_ps of interval : int<ps>
        | Interval_ns of interval : int<ns>
        | Interval_us of inverval : int<us>
        | Interval_ms of interval : int<ms>
        | Interval_s  of interval : int<s>
        member internal this.InFemtoseconds =
            let toFs m x = (int64 x) * (int64 m) * 1L<fs>
            match this with
            | Interval_fs interval -> interval * 1</fs> |> toFs 1e0<fs>
            | Interval_ps interval -> interval * 1</ps> |> toFs 1e3<fs>
            | Interval_ns interval -> interval * 1</ns> |> toFs 1e6<fs>
            | Interval_us interval -> interval * 1</us> |> toFs 1e9<fs>
            | Interval_ms interval -> interval * 1</ms> |> toFs 1e12<fs>
            | Interval_s  interval -> interval * 1</s>  |> toFs 1e15<fs>
        override x.GetHashCode() = hash x.InFemtoseconds
        override x.Equals(other) =
            match other with | :? Interval as y -> x.InFemtoseconds = y.InFemtoseconds | _ -> false
        interface System.IComparable<Interval> with
            member x.CompareTo y = compare x.InFemtoseconds y.InFemtoseconds
        interface System.IComparable with
            member x.CompareTo other =
                match other with | :? Interval as y -> compare x.InFemtoseconds y.InFemtoseconds | _ -> -1
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

        type AnalogueChannel =
            | ChannelA
            | ChannelB
            | ChannelC
            | ChannelD

        // Port 0 corresponds to pins 0 to 7, and port 1 corresponds to pins 8 to 15.
        type DigitalPort =
            | Port0
            | Port1

        type DigitalChannel =
            |D0 |D1 |D2 |D3 |D4 |D5 |D6 |D7
            |D8 |D9 |D10|D11|D12|D13|D14|D15

        /// Input channel.
        type InputChannel =
            | Analogue of channel : AnalogueChannel
            | Digital  of port    : DigitalPort

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
        type AnalogueSettings =
            internal { Coupling       : Coupling
                       Range          : Range
                       AnalogueOffset : Voltage
                       BandwidthLimit : Bandwidth }

        type DigitalSettings =
            internal { LogicLevel : Voltage }

        type InputSettings =
            internal
            | AnalogueSettings of settings : AnalogueSettings
            | DigitalSettings  of settings : DigitalSettings

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
    module Triggering =

        type Channel =
            | AnalogueTrigger of channel : AnalogueChannel
            | ExternalTrigger
            | AuxiliaryTrigger

        module Simple =

            type Direction =
                | Above
                | Below
                | Rising
                | Falling
                | RisingOrFalling

            type Trigger =
                internal { Source     : Channel
                           Threshold  : AdcCount
                           Direction  : Direction }

        module Complex =

            type Source =
                | Channel of channel : Channel
                | DigitalTrigger
                | PulseWidthQualifierTrigger

            type internal AdcThreshold =
                { AdcThreshold  : AdcCount
                  Hysteresis    : AdcCount option }
            let internal noHysteresis:AdcCount = 0s

            type Threshold =
                { Threshold  : Voltage
                  Hysteresis : Voltage option }

            type Condition =
                internal
                | Above           of level : Threshold   // for gated triggers: above the upper threshold
                | Below           of level : Threshold   // for gated triggers: below the upper threshold
                | Rising          of level : Threshold   // for threshold triggers: rising edge, using upper threshold
                | Falling         of level : Threshold   // for threshold triggers: falling edge, using upper threshold
                | RisingOrFalling of level : Threshold   // for threshold triggers: either edge
                | Inside          of lower : Threshold * upper : Threshold  // for window-qualified triggers: inside window
                | Outside         of lower : Threshold * upper : Threshold  // for window-qualified triggers: outside window
                | Enter           of lower : Threshold * upper : Threshold  // for window triggers: entering the window
                | Exit            of lower : Threshold * upper : Threshold  // for window triggers: leaving the window
                | EnterOrExit     of lower : Threshold * upper : Threshold  // for window triggers: either entering or leaving the window
                | PositiveRunt    of lower : Threshold * upper : Threshold  // for window-qualified triggers
                | NegativeRunt    of lower : Threshold * upper : Threshold  // for window-qualified triggers

            let (|LevelCondition|WindowCondition|) = function
                | Above level
                | Below level
                | Rising level
                | Falling level
                | RisingOrFalling level -> LevelCondition level
                | Inside (lower, upper)
                | Outside (lower, upper)
                | Enter (lower, upper)
                | Exit (lower, upper)
                | EnterOrExit (lower, upper)
                | PositiveRunt (lower, upper)
                | NegativeRunt (lower, upper) -> WindowCondition (lower, upper)

            type DigitalCondition =
                internal
                | High
                | Low
                | RisingEdge
                | FallingEdge
                | RisingOrFallingEdge

            // Complex triggers can be a boolean combination of the states of the triggering
            // conditions on all the channels.
            type State =
                | Require of Source * bool
                | And of State * State
                | Or of State * State

                static member (*) (a,b) =
                    And <| (a,b)

                static member (/) (a,b) =
                    Or <| (a,b)

            type Trigger =
                { TriggerState       : State                 // Boolean combinations of trigger conditions required
                  AnalogueConditions : Map<Source,Condition> // At most one trigger condition per channel
                  DigitalConditions  : Map<DigitalChannel,DigitalCondition> } // ANDed together to make a digital trigger

        module Advanced =
            () // with PulseWidthQualifiers
            // PulseWidthQualifiers can be used with or without triggering configuration
            // but are not fully independent. For instance if the upper threshold used triggering on a channel
            // the lower threshold must be used for PWQ. However the direction used to "trigger" PWQ can only
            // be set across all PWQ, whereas the direction and thresholds can be set per channel.

        /// Auto-trigger delay.
        type AutoTriggerDelay = internal AutoTriggerDelay_ms of delay : int16<ms>
        let noAutoTrigger = (AutoTriggerDelay_ms 0s<ms>)

        let noDelay : SampleCount = 0

        type Trigger =
        | AutoTrigger
        | SimpleTrigger of trigger : Simple.Trigger
        | ComplexTrigger of trigger : Complex.Trigger

        type TriggerSetting = { Trigger : Trigger
                                Delay   : SampleCount option
                                Auto    : AutoTriggerDelay option }







    [<AutoOpen>]
    /// Model types related to signal sampling and acquisition.
    module Acquisition =
        /// Downsampling ratio used to reduce the data sampled by the oscilloscope before
        /// sending it to the computer.
        type DownsamplingRatio = uint32

        /// Device memory segment. Memory can be segmented to allow multiple captures to be
        /// stored on the oscilloscope before transferring them to the computer.
        type MemorySegment = uint32

        /// Index of capture in rapid block acquisition
        type Capture = uint32

        /// Timebase index, indicating the sample interval for acquisition. The relationship
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

        /// Specify how much memory to allocate for block mode acquisitions
        /// Streaming mode allocates minimal memory specified by BufferLength
        /// SingleCapture will transfer one whole capture at a time
        /// MultipleCapture will transfer up to n captures at a time
        /// AllCaptures will get all the data in one bulk transfer, which could require several hundred megabytes
        /// depending on the number of samples and captures requested
        type BufferAllocation =
            | Streaming
            | SingleCapture
            | MultipleCapture of n : Capture
            | AllCaptures

        /// Contains parameters for a general acquisition.
        type AcquisitionParameters =
            { Inputs            : AcquisitionInputs
              SampleInterval    : Interval  // requested, check timebase actually applied
              Trigger           : TriggerSetting
              DownsamplingRatio : DownsamplingRatio option
              BufferLength      : SampleCount}

        /// Model types related to streaming acquisition.
        [<AutoOpen>]
        module Streaming =
            /// Stream stop settings: either manual or automatic.
            type StreamStop =
                internal
                | ManualStop
                | AutoStop of maxPreTriggerSamples : SampleIndex * maxPostTriggerSamples : SampleIndex

            /// Contains all parameters for a streaming acquisition
            type StreamingParameters =
                internal { Acquisition : AcquisitionParameters
                           StreamStop  : StreamStop }
        
        [<AutoOpen>]
        module Block =
            // Start with regular block mode, extend for rapid block
            // Leave out oversampling for now, not supported on 3000
            type BlockParameters =
                internal { Acquisition        : AcquisitionParameters
                           PreTriggerSamples  : SampleCount
                           PostTriggerSamples : SampleCount
                           Buffering          : BufferAllocation }

            type NumberOfCaptures = uint32

        type Parameters =
            | StreamingParameters of StreamingParameters
            | BlockParameters of BlockParameters
            | RapidBlockParameters of NumberOfCaptures * BlockParameters












    [<AutoOpen>]
    module Runtime =

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
            type AcquisitionBuffers = Map<InputChannel * BufferDownsampling, AdcCount array> array

            /// Indicates whether a trigger fired during a block of samples transferred from the device
            /// to the computer, and if so, its trigger position.
            type TriggerPosition =
                internal
                | NotTriggered
                | Triggered of triggerSample : SampleIndex

        /// Contains a block of samples obtained from the device by streaming or block acquisition
        type AcquiredSamples =
            internal { Samples          : Map<InputChannel * BufferDownsampling, AdcCount array>
                       Capture          : MemorySegment
                       Length           : SampleCount
                       VoltageOverflows : Set<AnalogueChannel> }

        /// Contains information about the way an acquisition was stopped, indicating whether
        /// it was stopped manually or automatically.
        type StopStatus = 
            internal { StoppedAutomatically : bool
                       Failed               : exn option }
    
        /// Status of an acquisition emitted through a status event while the acquisition is in progress.
        type AcquisitionStatus =
            | PreparingAcquisition
            | Acquiring of sampleInterval : Interval
            | FinishedCapture of index : Capture
            | FinishedAcquisition of stoppedAutomatically : bool
            | CancelledAcquisition

        type AcquisitionCommon =
            internal { Parameters      : AcquisitionParameters
                       PicoScope       : PicoScope3000
                       DataBuffers     : AcquisitionBuffers
                       SamplesObserved : Event<AcquiredSamples>
                       StopCapability  : CancellationCapability<StopStatus>
                       StatusChanged   : NotificationEvent<AcquisitionStatus> }

        type ValuesReady =
            { Capture          : Capture
              StartIndex       : SampleIndex
              NumberOfSamples  : SampleCount
              VoltageOverflows : Set<AnalogueChannel> }
        
        type Expecting = MoreData | NoMoreData

        /// Model types related to streaming acquisition.
        [<AutoOpen>]
        module Streaming =

            /// Contains information from a driver callback which indicates the location of the latest
            /// streaming acquisition samples in the data buffer.
            type StreamingValuesReady =
                internal { ValuesReady      : ValuesReady
                           TriggerPosition  : TriggerPosition
                           DidAutoStop      : bool }

            /// Defines a streaming acquisition which has been set up on a PicoScope 3000 series device. This
            /// is created once after the streaming parameters are defined and a connection to the device is
            /// established.
            type StreamingAcquisition =
                internal { Common     : AcquisitionCommon
                           Parameters : StreamingParameters }

        [<AutoOpen>]
        module Block =

            type BlockAcquisition =
                internal { Common     : AcquisitionCommon
                           Parameters : BlockParameters }
               
            type RapidBlockAcquisition =
                internal { Acquisition : BlockAcquisition
                           Count       : NumberOfCaptures }
            

        // Describes an acquisition that is ready to run, which can be used in computational expressions describing
        // processing of acquisition events from this acquisition
        type Acquisition =
        | StreamingAcquisition of Streaming.StreamingAcquisition
        | BlockAcquisition of Block.BlockAcquisition
        | RapidBlockAcquisition of Block.RapidBlockAcquisition

        /// Contains the result of an acquisition, indicating whether it finished successfully, failed
        /// or was cancelled.
        type AcquisitionResult =
            | AcquisitionCompleted
            | AcquisitionError of exn
            | AcquisitionCancelled

        /// Represents a handle to a streaming acquisition which has been started on a PicoScope 3000 series
        /// device. This can be used to stop the acquisition manually and/or wait for it to finish.
        type AcquisitionHandle =
            internal { Acquisition  : Acquisition 
                       WaitToFinish : Async<AcquisitionResult> }
           
        // Finding the correct timebase - fastest few are special cases which may have restrictions.
        // The rest of the uint is linear multiple of some small base unit, which differs between models
        // Use one of the GetTimebase calls to check the timebase selected is closest to the one requested
