// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

open Endorphin.Core
open NativeModel
open StatusCodes

/// Contains functions related to parsing between native model and high level model domain
/// types.
module internal Parsing =
    /// Parses a 16-bit integer into a toggle state indicating whether a setting is enabled
    /// or disabled.
    let parseToggleState value = if value <> 0s then Enabled else Disabled

    [<AutoOpen>]
    /// Parsing functions related to status codes.
    module StatusCodes =
        /// Matches StatusCode.Ok as Ok and all other status codes as errors. Note that this
        /// is not always the desired behaviour as some status codes indicate different device
        /// states rather than failure.
        let (|Ok|HasError|) = function
            | StatusCode.Ok -> Ok
            | error         -> HasError (statusMessage error)

        /// Parses the provided status as an availability status, matching StatusCode.Ok as
        /// Available and StatusCode.Busy as Busy. Doesn't match any other status codes.
        let (|AvailabilityStatus|_|) = function
            | StatusCode.Ok   -> Some (AvailabilityStatus Available)
            | StatusCode.Busy -> Some (AvailabilityStatus Busy)
            | _               -> None

        /// Parses the provided status as a found/not found status, match StatusCode.Ok as
        /// (Found true) and StatusCode.NotFound as (Found false).
        let (|Found|_|) = function
            | StatusCode.Ok       -> Some (Found true)
            | StatusCode.NotFound -> Some (Found false)
            | _                   -> None

        /// Parses the provided status as a power source, matching
        /// StatusCode.PowerSupplyConnected and StatusCode.PowerSupplyNotConnected accordingly.
        /// Doesn't match any other status codes.
        let (|PowerSourceStatus|_|) = function
            | StatusCode.PowerSupplyConnected    -> Some MainsPower
            | StatusCode.PowerSupplyNotConnected -> Some UsbPower
            | _                                  -> None

        /// Returns the status code corresponding to a provided power source, which can be used
        /// to switch the power source of the PicoScope hardware.
        let powerSourceStatusCode = function
            | MainsPower -> StatusCode.PowerSupplyConnected
            | UsbPower   -> StatusCode.PowerSupplyNotConnected

    [<AutoOpen>]
    /// Parsing functions related to the device indicators and information.
    module Device =
        // TODO: model device info types and add device parsing functions

        /// Converts an LedFlash setting to a 16-bit integer which can be used to set the LED
        /// flash state on the device.
        let ledFlashCounts = function
            | LedOff              -> 0s
            | LedRepeat n         -> n
            | LedIndefiniteRepeat -> -1s

    /// Parses the provided integer interval with and enumeration indicating the time unit into
    /// an interval with unit of measure.
    let parseIntervalWithInterval (interval, unit) =
        match unit with
        | TimeUnitEnum.Femtoseconds -> Interval_fs (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Picoseconds  -> Interval_ps (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Nanoseconds  -> Interval_ns (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Microseconds -> Interval_us (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Milliseconds -> Interval_ms (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Seconds      -> Interval_s  (LanguagePrimitives.Int32WithMeasure interval)
        | enum                      -> unexpectedReply <| sprintf "Unexpected time unit enum value: %A" enum

    /// Converts the provided interval with unit of measure to an integer interval with an
    /// enumeration indicating the time unit.
    let intervalAndTimeUnitEnum = function
        | Interval_fs interval -> (int interval, TimeUnitEnum.Femtoseconds)
        | Interval_ps interval -> (int interval, TimeUnitEnum.Picoseconds)
        | Interval_ns interval -> (int interval, TimeUnitEnum.Nanoseconds)
        | Interval_us interval -> (int interval, TimeUnitEnum.Microseconds)
        | Interval_ms interval -> (int interval, TimeUnitEnum.Milliseconds)
        | Interval_s  interval -> (int interval, TimeUnitEnum.Seconds)

    /// Converts the provided voltage with unit of measure to a 32-bit precision floating point
    /// voltage in volts.
    let voltageFloat_V (Voltage_V v) = float32 v

    /// Converts an AutoTriggerDelay option to a corresponding 16-bit integer indicating either that
    /// there is no auto-trigger or the auto-trigger delay value in milliseconds.
    let autoTriggerDelayInt_ms = function
        | Some (AutoTriggerDelay_ms delay) -> int16 delay
        | None                             -> 0s

    [<AutoOpen>]
    /// Parsing functions related to channel settings.
    module ChannelSettings =
        /// Parse an enumeration as a digital port.  This does not currently parse all possible values of
        /// the enumeration, because the PicoScope 3406D MSO (which is what we have) doesn't support them.
        let parseDigitalPort = function
            | DigitalPortEnum._0 -> DigitalPort0
            | DigitalPortEnum._1 -> DigitalPort1
            | enum               -> unexpectedReply <| sprintf "Unexpected digital port value: %A." enum

        /// Convert the provided digital port into an enumeration.
        let digitalPortEnum = function
            | DigitalPort0 -> DigitalPortEnum._0
            | DigitalPort1 -> DigitalPortEnum._1

        /// Parses an enumeration as an input channel. Note that this does not parse all possible
        /// values of ChannelEnum as not all of these indicate an input channel.
        let parseInputChannel = function
            | ChannelEnum.A -> ChannelA
            | ChannelEnum.B -> ChannelB
            | ChannelEnum.C -> ChannelC
            | ChannelEnum.D -> ChannelD
            | enum          -> unexpectedReply <| sprintf "Unexpected input channel enum value: %A." enum

        /// Converts the provided input channel into an enumeration.
        let inputChannelEnum = function
            | ChannelA -> ChannelEnum.A
            | ChannelB -> ChannelEnum.B
            | ChannelC -> ChannelEnum.C
            | ChannelD -> ChannelEnum.D

        /// Parses a enumeration into a coupling.
        let parseCoupling = function
            | CouplingEnum.AC -> AC
            | CouplingEnum.DC -> DC
            | enum            -> unexpectedReply <| sprintf "Unexpected coupling enum value: %A." enum

        /// Converts the provided coupling into an enumeration.
        let couplingEnum = function
            | AC -> CouplingEnum.AC
            | DC -> CouplingEnum.DC

        /// Parses the provided enumeration into a voltage range.
        let parseRange = function
            | RangeEnum._10mV  -> Range_10mV
            | RangeEnum._20mV  -> Range_20mV
            | RangeEnum._50mV  -> Range_50mV
            | RangeEnum._100mV -> Range_100mV
            | RangeEnum._200mV -> Range_200mV
            | RangeEnum._500mV -> Range_500mV
            | RangeEnum._1V    -> Range_1V
            | RangeEnum._2V    -> Range_2V
            | RangeEnum._5V    -> Range_5V
            | RangeEnum._10V   -> Range_10V
            | RangeEnum._20V   -> Range_20V
            | RangeEnum._50V   -> Range_50V
            | enum             -> unexpectedReply <| sprintf "Unexpected range enum value: %A." enum

        /// Converts the provided voltage range into an enumeration.
        let rangeEnum = function
            | Range_10mV  -> RangeEnum._10mV
            | Range_20mV  -> RangeEnum._20mV
            | Range_50mV  -> RangeEnum._50mV
            | Range_100mV -> RangeEnum._100mV
            | Range_200mV -> RangeEnum._200mV
            | Range_500mV -> RangeEnum._500mV
            | Range_1V    -> RangeEnum._1V
            | Range_2V    -> RangeEnum._2V
            | Range_5V    -> RangeEnum._5V
            | Range_10V   -> RangeEnum._10V
            | Range_20V   -> RangeEnum._20V
            | Range_50V   -> RangeEnum._50V

        /// Parses the provided enumeration into a bandwidth limit.
        let parseBandwidthLimit = function
            | BandwidthLimitEnum.Full   -> FullBandwidth
            | BandwidthLimitEnum._20MHz -> Bandwidth_20MHz
            | enum                      -> unexpectedReply <| sprintf "Unexpected bandwidth limit enum value: %A." enum

        /// Converts the provided bandwidth limit into an enumeration.
        let bandwidthLimitEnum = function
            | FullBandwidth   -> BandwidthLimitEnum.Full
            | Bandwidth_20MHz -> BandwidthLimitEnum._20MHz

    [<AutoOpen>]
    /// Parsing functions related to triggering.
    module Triggering =
        /// Converts the provided level threshold direction into an enumeration.
        let internal levelThresholdEnum = function
            | Above Upper     -> ThresholdDirectionEnum.AboveUpper
            | Above Lower     -> ThresholdDirectionEnum.AboveLower
            | Below Upper     -> ThresholdDirectionEnum.BelowUpper
            | Below Lower     -> ThresholdDirectionEnum.BelowLower
            | Rising Upper    -> ThresholdDirectionEnum.RisingUpper
            | Rising Lower    -> ThresholdDirectionEnum.RisingLower
            | Falling Upper   -> ThresholdDirectionEnum.FallingUpper
            | Falling Lower   -> ThresholdDirectionEnum.FallingLower
            | RisingOrFalling -> ThresholdDirectionEnum.RisingOrFalling

        /// Converts the provided window threshold direction into an enumeration.
        let internal windowThresholdEnum = function
            | Inside      -> ThresholdDirectionEnum.Inside
            | Outside     -> ThresholdDirectionEnum.Outside
            | Enter       -> ThresholdDirectionEnum.Enter
            | Exit        -> ThresholdDirectionEnum.Exit
            | EnterOrExit -> ThresholdDirectionEnum.EnterOrExit

        /// Parses the provided enumeration into a trigger channel (which can be an input
        /// channel, or the external or auxiliary channel).
        let parseTriggerChannel = function
            | ChannelEnum.A   -> InputChannelTrigger ChannelA
            | ChannelEnum.B   -> InputChannelTrigger ChannelB
            | ChannelEnum.C   -> InputChannelTrigger ChannelC
            | ChannelEnum.D   -> InputChannelTrigger ChannelD
            | ChannelEnum.Ext -> ExternalTrigger
            | ChannelEnum.Aux -> AuxiliaryTrigger
            | enum            -> unexpectedReply <| sprintf "Unexpected trigger channel enum value: %A." enum

        /// Converts the provided trigger channel into a enumeration.
        let triggerChannelEnum = function
            | InputChannelTrigger channel -> inputChannelEnum channel
            | ExternalTrigger             -> ChannelEnum.Ext
            | AuxiliaryTrigger            -> ChannelEnum.Aux

    [<AutoOpen>]
    module Acquisition =
        /// Parses the provided enumeration into an ETS mode.
        let parseEtsMode = function
            | EtsModeEnum.Off  -> Acquisition.NoEts
            | EtsModeEnum.Fast -> Acquisition.FastEts
            | EtsModeEnum.Slow -> Acquisition.SlowEts
            | enum             -> unexpectedReply <| sprintf "Unexpected ETS mode enum value: %A." enum

        /// Converts the provided ETS mode to an enumeration.
        let etsModeEnum = function
            | Acquisition.NoEts   -> EtsModeEnum.Off
            | Acquisition.FastEts -> EtsModeEnum.Fast
            | Acquisition.SlowEts -> EtsModeEnum.Slow

        /// Parses the provided enumeration as a downsampling mode.
        let parseDownsamplingMode = function
            | DownsamplingModeEnum.None      -> NoDownsampling
            | DownsamplingModeEnum.Averaged  -> Averaged
            | DownsamplingModeEnum.Decimated -> Decimated
            | DownsamplingModeEnum.Aggregate -> Aggregate
            | enum                           -> unexpectedReply <| sprintf "Unexpected downsampling mode enum value: %A." enum

        /// Converts the provided enumeration to a downsampling mode.
        let downsamplingModeEnum = function
            | NoDownsampling -> DownsamplingModeEnum.None
            | Averaged       -> DownsamplingModeEnum.Averaged
            | Decimated      -> DownsamplingModeEnum.Decimated
            | Aggregate      -> DownsamplingModeEnum.Aggregate

        /// Converts a set of downsampling modes into an enumeration.
        let downsamplingModeEnumForSet =
            Set.map downsamplingModeEnum
            >> Set.reduce (|||)

        /// Converts the provided StreamStop settings into the parameters required by the hardware,
        /// containing the flag indicating whether auto-stop is enabled and the maximum number of
        /// pre- and post-trigger samples.
        let streamStopParameters streamStop : (int16 * SampleIndex * SampleIndex) =
            match streamStop with
            | ManualStop                                       -> (0s, 0u, 1u)
            | AutoStop (preTriggerSamples, postTriggerSamples) -> (1s, preTriggerSamples, postTriggerSamples)

        /// Parses the provided trigger flag and position into a TriggerPosition.
        let parseTriggerPosition triggered position =
            if triggered then Triggered position
            else NotTriggered