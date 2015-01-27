namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

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
