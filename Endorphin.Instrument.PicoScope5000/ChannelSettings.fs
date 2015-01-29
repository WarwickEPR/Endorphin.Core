namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core.Units

/// Enumeration representing a PicoScope 5000 series device channel.
type Channel =
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | Ext = 4
    | Aux = 5
    | None = 6

/// Enumeration representing the input coupling of a PicoScope 5000 series channel.
type Coupling = 
    | AC = 0
    | DC = 1

/// Enumeration representing the input voltage range of a PicoScope 5000 series channel.
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

/// Enumeration representing the possible time units for samlpe intervals of a PicoScope 5000 series device.
type TimeUnit =
    | Femtoseconds = 0
    | Picoseconds = 1
    | Nanoseconds = 2
    | Microseconds = 3
    | Milliseconds = 4
    | Seconds = 5

/// Enumeration indicating whether equivalent time sampling is enabled on a PicoScope 5000 series device.
type EtsMode =
    | Off = 0
    | Fast = 1
    | Slow = 2

/// Enumeration representing the vertical resolution of a PicoScope 5000 series device.
type Resolution =
    | _8bit = 0
    | _12bit = 1
    | _14bit = 2
    | _15bit = 3
    | _16bit = 4

/// Enumeration representing possible types channel information which can be requested from a PicoScope 5000 series device.
type ChannelInfo =
    | VoltageOffsetRanges = 0

/// Enumeration representing the input bandwidth of a PicoScope 5000 series device.
type BandwidthLimit =
    | Full = 0
    | _20MHz = 1

/// Defines all input settings for a given PicoScope 5000 series channel.
type InputSettings = 
    { Coupling : Coupling
      Range : Range
      AnalogueOffset : float<V>
      BandwidthLimit : BandwidthLimit }

/// Defines the possible states of a PicoScope 5000 series channel. It can either be enabled or
/// disabled. If it is enabled, it need to have specified input settings.
type ChannelSettings =
    | Enabled of inputSettings : InputSettings
    | Disabled 

[<AutoOpen>]
/// Provides useful extnesion members for types representing channel settings.
module ChannelSettingsExtensions =
    type Range with

        /// Converts the range to a value in volts.
        member range.ToVolts() =
            match range with
            | Range._10mV -> 0.010<V>
            | Range._20mV -> 0.020<V>
            | Range._50mV -> 0.050<V>
            | Range._100mV -> 0.100<V>
            | Range._200mV -> 0.200<V>
            | Range._500mV -> 0.500<V>
            | Range._1V -> 1.0<V>
            | Range._2V -> 2.0<V>
            | Range._5V -> 5.0<V>
            | Range._10V -> 10.0<V>
            | Range._20V -> 20.0<V>
            | Range._50V -> 50.0<V>
            | _ -> failwith "Unexpected range."

        /// Returns the smallest available input range which will handle a specified zero-to-peak input voltage.
        static member SmallestInputRangeForVoltage (zeroToPeakVoltage : float<V>) =
            match zeroToPeakVoltage with
            | voltage when voltage <= 0.010<V> -> Range._10mV
            | voltage when voltage <= 0.020<V> -> Range._20mV
            | voltage when voltage <= 0.050<V> -> Range._50mV
            | voltage when voltage <= 0.100<V> -> Range._100mV
            | voltage when voltage <= 0.200<V> -> Range._200mV
            | voltage when voltage <= 0.500<V> -> Range._500mV
            | voltage when voltage <= 1.0<V> -> Range._1V
            | voltage when voltage <= 2.0<V> -> Range._2V
            | voltage when voltage <= 5.0<V> -> Range._5V
            | voltage when voltage <= 10.0<V> -> Range._10V
            | voltage when voltage <= 20.0<V> -> Range._20V
            | _ -> failwith "Requested voltage exceed maximum device input range."

    type Resolution with
        /// Returns the fastest timebase supported by the PicoScope 5000 hardware for this vertical resolution.
        member resolution.FastestTimebase =
            match resolution with
            | Resolution._8bit -> 0u
            | Resolution._12bit -> 1u
            | Resolution._14bit
            | Resolution._15bit -> 3u
            | Resolution._16bit -> 4u
            | _ -> failwith "Unexpected resolution."

        /// Returns the maximum number of input channels supported by the PicoScope 5000 hardware for this vertical
        /// resolution.
        member resolution.MaximumNumberOfChannels =
            match resolution with
            | Resolution._8bit | Resolution._12bit | Resolution._14bit -> 4
            | Resolution._15bit -> 2
            | Resolution._16bit -> 1
            | _ -> failwith "Unexpected resolution."

        /// Returns the fastest streaming sample interval supported by the PicoScope 5000 hardware for this vertical
        /// resolution.
        member resolution.FastestStreamingInterval channelCount =
            match (resolution, channelCount) with
            | (Resolution._12bit, 4)
            | (Resolution._12bit, 3)
            | (Resolution._14bit, 4)
            | (Resolution._14bit, 3) -> 256.0<ns>
            | (Resolution._8bit, 4) 
            | (Resolution._8bit, 3) 
            | (Resolution._12bit, 2) 
            | (Resolution._14bit, 2) 
            | (Resolution._15bit, 2) -> 128.0<ns>
            | (Resolution._8bit, 3)
            | (Resolution._12bit, 1)
            | (Resolution._14bit, 1)
            | (Resolution._15bit, 1)
            | (Resolution._16bit, 2) -> 64.0<ns>
            | (Resolution._8bit, 1) -> 32.0<ns>
            | (resolution, channelCount) when channelCount > resolution.MaximumNumberOfChannels ->
                failwith "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
            | parameters -> failwith "Unexpected number of channels or resolution: %A." parameters

        /// Returns the number of bits for this resolution as an integer value.
        member resolution.Value =
            match resolution with
            | Resolution._8bit -> 8<bits>
            | Resolution._12bit -> 12<bits>
            | Resolution._14bit -> 14<bits>
            | Resolution._15bit -> 15<bits>
            | Resolution._16bit -> 16<bits>
            | _ -> failwith "Unexpected resolution."

        /// Returns a bit mask which only selects the significant number of top bits for the given resolution.
        member resolution.BitMask =
            // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
            // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
            // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
            // correspond to the device resolution are 1s and the lowest order bits are 0s.
            ~~~ ((1s <<< (int (16<bits> - resolution.Value))) - 1s)