namespace Endorphin.Instrument.PicoScope5000

open System.Runtime.InteropServices

// This module uses the StructLayout attribute to define a struct used for interoperation with
// a native C library. The sequential StructLayout may result in non-verifiable IL code when
// FieldOffset attributes are also used (but not in this case), so suppress the warning.
#nowarn "9"

/// Contains model types required for making calls to the PicoScope 5000 series native C API.
module internal NativeModel =
    
    /// Information types which can be requested from the PicoScope 5000 series C API about
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
        | FirmwareVersion1        = 9
        | FirmwareVersion2        = 10

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

        /// Information types which can be requested from the PicoScope 5000 series driver
        /// about an input channel.
        type ChannelInfoEnum =
            | VoltageOffsetRanges = 0

    [<AutoOpen>]
    /// Native model types related to triggering.
    module Triggering =
        
        /// Trigger channel threshold direction.
        type ThresholdDirectionEnum = 
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


        // The types below are included for completeness but not currently in use and only required
        // for more advanced types of triggering.

        type PulseWidthTypeEnum =
            | None = 0
            | LessThan = 1
            | GreaterThan = 2
            | InRange = 3
            | OutOfRange = 4

        type TriggerStateEnum =
            | DontCare = 0
            | True = 1
            | False = 2

        type ThresholdModeEnum =
            | Level = 0
            | Window = 1

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type internal TriggerChannelProperties =
            struct 
                val ThresholdMajor : int16 
                val ThresholdMinor : int16
                val Hysteresis     : uint16
                val Channel        : ChannelEnum
                val ThresholdMode  : ThresholdModeEnum

                new(thresholdMajor : int16,
                    thresholdMinor : int16,
                    hysteresis     : uint16,
                    channel        : ChannelEnum,
                    thresholdMode  : ThresholdModeEnum) = 
                    { ThresholdMajor = thresholdMajor
                      ThresholdMinor = thresholdMinor
                      Hysteresis = hysteresis
                      Channel = channel
                      ThresholdMode = thresholdMode }
            end

        [<StructLayout(LayoutKind.Sequential, Pack = 1)>]
        type internal TriggerConditions = 
            struct
                val ChannelA            : TriggerStateEnum
                val ChannelB            : TriggerStateEnum
                val ChannelC            : TriggerStateEnum
                val ChannelD            : TriggerStateEnum;
                val External            : TriggerStateEnum
                val Auxiliary           : TriggerStateEnum
                val PulseWidthQualifier : TriggerStateEnum

                new(channelA            : TriggerStateEnum,
                    channelB            : TriggerStateEnum,
                    channelC            : TriggerStateEnum,
                    channelD            : TriggerStateEnum,
                    external            : TriggerStateEnum,
                    auxiliary           : TriggerStateEnum,
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
                val ChannelA  : TriggerStateEnum
                val ChannelB  : TriggerStateEnum
                val ChannelC  : TriggerStateEnum
                val ChannelD  : TriggerStateEnum
                val External  : TriggerStateEnum
                val Auxiliary : TriggerStateEnum

              new(channelA  : TriggerStateEnum,
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

    [<AutoOpen>]
    /// Native model types relating to signal sampling.
    module Acquisition =

        /// Vertical resolution, set for all input channels on the device. The PicoScope 5000
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