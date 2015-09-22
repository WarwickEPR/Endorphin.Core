namespace Endorphin.Instrument.PicoScope3000

open System.Runtime.InteropServices
open StatusCodes

// This module uses the StructLayout attribute to define a struct used for interoperation with
// a native C library. The sequential StructLayout may result in non-verifiable IL code when
// FieldOffset attributes are also used (but not in this case), so suppress the warning.
#nowarn "9"

/// Contains model types required for making calls to the PicoScope 3000 series native C API.
module internal NativeModel =
    /// Information types which can be requested from the PicoScope 3000 series C API about
    /// the device.
    type DeviceInfoEnum =
        | DriverVersion           = 0
        | UsbVersion              = 1
        | HardwareVersion         = 2
        | ModelNumber             = 3
        | SerialNumber            = 4
        | CalibrationDate         = 5
        | KernelVersion           = 6
        | DigitalHardwareVersion  = 7
        | AnalogueHardwareVersion = 8

    [<AutoOpen>]
    /// Native model types related to channel settings.
    module ChannelSettings =
        /// Device channel (includes input, trigger, and output channels).
        type ChannelEnum =
            | A    = 0
            | B    = 1
            | C    = 2
            | D    = 3
            | Ext  = 4
            | Aux  = 5
            | None = 6

        type ChannelBufferIndexEnum =
            | AMax = 0
            | AMin = 1
            | BMax = 2
            | BMin = 3
            | CMax = 4
            | CMin = 5
            | DMax = 6
            | DMin = 7

        /// The digital port to use.  Port `n` corresponds to channels `8 * n` to `8 * n + 7`.
        type DigitalPortEnum =
            /// Channels 0--7
            | _0  = 0x80
            /// Channels 8--15
            | _1  = 0x81
            /// Channels 16--23
            | _2  = 0x82
            /// Channels 24-31
            | _3  = 0x83

        /// A selector for the digital channel of the device.
        type DigitalChannelEnum =
            | _0  = 0
            | _1  = 1
            | _2  = 2
            | _3  = 3
            | _4  = 4
            | _5  = 5
            | _6  = 6
            | _7  = 7
            | _8  = 8
            | _9  = 9
            | _10 = 10
            | _11 = 11
            | _12 = 12
            | _13 = 13
            | _14 = 14
            | _15 = 15
            | _16 = 16
            | _17 = 17
            | _18 = 18
            | _19 = 19
            | _20 = 20
            | _21 = 21
            | _22 = 22
            | _23 = 23
            | _24 = 24
            | _25 = 25
            | _26 = 26
            | _27 = 27
            | _28 = 28
            | _29 = 29
            | _30 = 30
            | _31 = 31

        /// Input channel voltage range.
        type RangeEnum =
            | _10mV  = 0
            | _20mV  = 1
            | _50mV  = 2
            | _100mV = 3
            | _200mV = 4
            | _500mV = 5
            | _1V    = 6
            | _2V    = 7
            | _5V    = 8
            | _10V   = 9
            | _20V   = 10
            | _50V   = 11

        /// Input channel coupling.
        type CouplingEnum =
            | AC = 0
            | DC = 1

        /// Input channel bandwidth.
        type BandwidthLimitEnum =
            | Full   = 0
            | _20MHz = 1

        /// Information types which can be requested from the PicoScope 3000 series driver
        /// about an input channel.
        type ChannelInfoEnum =
            | ConfidenceIntervalRanges = 0

    [<AutoOpen>]
    /// Native model types related to triggering.
    module Triggering =
        /// Trigger channel threshold mode.
        type ThresholdModeEnum =
            | Level  = 0
            | Window = 1

        /// Trigger channel threshold direction.
        type ThresholdDirectionEnum =
            // values for upper level threshold mode
            | AboveUpper      = 0
            | BelowUpper      = 1
            | RisingUpper     = 2
            | FallingUpper    = 3
            // uses both level threshold
            | RisingOrFalling = 4
            // values for lower level threshold mode
            | AboveLower      = 5
            | BelowLower      = 6
            | RisingLower     = 7
            | FallingLower    = 8
            // values for window threshold mode
            | Inside          = 0
            | Outside         = 1
            | Enter           = 2
            | Exit            = 3
            | EnterOrExit     = 4
            | PositiveRunt    = 9
            | NegativeRunt    = 10
            // none
            | None            = 2


        // The types below are included for completeness but not currently in use and only required
        // for more advanced types of triggering.

        type DigitalDirectionEnum =
            | DontCare        = 0
            | Low             = 1
            | High            = 2
            | Rising          = 3
            | Falling         = 4
            | RisingOrFalling = 5

        type RatioModeEnum =
            | None      = 0
            | Aggregate = 1
            | Decimate  = 2
            | Average   = 4

        type PulseWidthTypeEnum =
            | None        = 0
            | LessThan    = 1
            | GreaterThan = 2
            | InRange     = 3
            | OutOfRange  = 4

        type HoldOffTypeEnum =
            | Time = 0
            | Event = 1

        type TriggerStateEnum =
            | DontCare = 0
            | True = 1
            | False = 2

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type internal TriggerChannelProperties =
            struct
                val ThresholdUpper  : int16
                val HysteresisUpper : uint16
                val ThresholdLower  : int16
                val HysteresisLower : uint16
                val Channel         : ChannelEnum
                val ThresholdMode   : ThresholdModeEnum

                new(thresholdUpper  : int16,
                    hysteresisUpper : uint16,
                    thresholdLower  : int16,
                    hysteresisLower : uint16,
                    channel         : ChannelEnum,
                    thresholdMode   : ThresholdModeEnum) =
                    { ThresholdUpper  = thresholdUpper
                      HysteresisUpper = hysteresisUpper
                      ThresholdLower  = thresholdLower
                      HysteresisLower = hysteresisLower
                      Channel         = channel
                      ThresholdMode   = thresholdMode }
            end

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type internal TriggerConditions =
            struct
                val ChannelA            : TriggerStateEnum
                val ChannelB            : TriggerStateEnum
                val ChannelC            : TriggerStateEnum
                val ChannelD            : TriggerStateEnum
                val External            : TriggerStateEnum
                val Auxiliary           : TriggerStateEnum
                val PulseWidthQualifier : TriggerStateEnum

                new ( channelA            : TriggerStateEnum,
                      channelB            : TriggerStateEnum,
                      channelC            : TriggerStateEnum,
                      channelD            : TriggerStateEnum,
                      external            : TriggerStateEnum,
                      auxiliary           : TriggerStateEnum,
                      pulseWidthQualifier : TriggerStateEnum ) =
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
                val ChannelA  : TriggerStateEnum
                val ChannelB  : TriggerStateEnum
                val ChannelC  : TriggerStateEnum
                val ChannelD  : TriggerStateEnum
                val External  : TriggerStateEnum
                val Auxiliary : TriggerStateEnum

                new ( channelA  : TriggerStateEnum,
                      channelB  : TriggerStateEnum,
                      channelC  : TriggerStateEnum,
                      channelD  : TriggerStateEnum,
                      external  : TriggerStateEnum,
                      auxiliary : TriggerStateEnum) =
                      { ChannelA = channelA
                        ChannelB = channelB
                        ChannelC = channelC
                        ChannelD = channelD
                        External = external
                        Auxiliary = auxiliary }
            end

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type internal TriggerInfo =
            struct
                val Status           : StatusCode
                val SegmentIndex     : uint32
                val Reserved0        : uint32
                val TriggerTime      : int64
                val TimeUnits        : int16
                val Reserved1        : int16
                val TimeStampCounter : uint64

                new ( status           : StatusCode,
                      segmentIndex     : uint32,
                      reserved0        : uint32,
                      triggerTime      : int64,
                      timeUnits        : int16,
                      reserved1        : int16,
                      timeStampCounter : uint64 ) =
                      { Status           = status
                        SegmentIndex     = segmentIndex
                        Reserved0        = reserved0
                        TriggerTime      = triggerTime
                        TimeUnits        = timeUnits
                        Reserved1        = reserved1
                        TimeStampCounter = timeStampCounter }
            end

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type DigitalChannelDirections =
            struct
                val Channel   : DigitalChannelEnum
                val Direction : DigitalDirectionEnum

                new ( channel   : DigitalChannelEnum,
                      direction : DigitalDirectionEnum ) =
                      { Channel   = channel
                        Direction = direction }
            end

    [<AutoOpen>]
    /// Native model types relating to signal sampling.
    module Acquisition =
        /// Vertical resolution, set for all input channels on the device. The PicoScope 3000
        /// series uses a variable resolution architecture which allows it to change the
        /// resolution between 8 and 16 bit in exchange for having fewer channels and/or a
        /// lower maximum sampling rate.
        type ResolutionEnum =
            | _8bit  = 0
            | _12bit = 1
            | _14bit = 2
            | _15bit = 3
            | _16bit = 4

        /// Time unit for sample intervals.
        type TimeUnitEnum =
            | Femtoseconds = 0
            | Picoseconds  = 1
            | Nanoseconds  = 2
            | Microseconds = 3
            | Milliseconds = 4
            | Seconds      = 5

        /// Equivalent time sampling mode setting. In ETS mode, the PicoScope samples a repetitive
        /// signal by interleaving multiple captures to achieve smaller sample intervals.
        type EtsModeEnum =
            | Off  = 0
            | Fast = 1
            | Slow = 2

        /// Input channel downsampling mode. Note that multiple downsampling modes can be set for
        /// each channel and they can be set independently of other input channels. This is achieved
        /// by combining the enumeration values using a bitwise OR operation. However, note that
        /// the downsampling mode "None" cannot be combined with other modes.
        type DownsamplingModeEnum =
            | None      = 0
            | Averaged  = 1
            | Decimated = 2
            | Aggregate = 4

        [<AutoOpen>]
        module Callbacks =
            /// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a block acquisition.
            /// Format: handle, status, state -> unit
            type internal PicoScopeBlockReady =
                delegate of int16 * int16 * nativeint -> unit

            /// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a streaming acquisition.
            /// Format; handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
            type internal PicoScopeStreamingReady =
                delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

            /// Callback delegate type used by the PicoScope driver to indicate that it has finished writing data to a buffer when reading data already
            /// stored in the device memory.
            /// Format: handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
            type internal PicoScopeDataReady =
                delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit

    [<AutoOpen>]
    module SignalGenerator =
        type SignalGeneratorWaveTypeEnum =
            | Sine      = 0
            | Square    = 1
            | Triangle  = 2
            | RampUp    = 3
            | RampDown  = 4
            | Sinc      = 5
            | Gaussian  = 6
            | HalfSine  = 7
            | DcVoltage = 8

        type SignalGeneratorSweepTypeEnum =
            | Up     = 0
            | Down   = 1
            | UpDown = 2
            | DownUp = 3

        type SignalGeneratorExtrasEnum =
            | None                  = 0
            | WhiteNoise            = 1
            | PseudoRandomBitStream = 2

        type SignalGeneratorTriggerTypeEnum =
            | Rising   = 0
            | Falling  = 1
            | GateHigh = 2
            | GateLow  = 3

        type SignalGeneratorTriggerSourceEnum =
            | None      = 0
            | Scope     = 1
            | Auxiliary = 2
            | External  = 3
            | Software  = 4

        type SignalGeneratorIndexModeEnum =
            | Single = 0
            | Dual   = 1
            | Quad   = 2