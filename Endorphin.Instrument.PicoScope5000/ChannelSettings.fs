namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Channel =
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | Ext = 4
    | Aux = 5
    | None = 6

type Coupling = 
    | AC = 0
    | DC = 1

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

type TimeUnit =
    | Femtoseconds = 0
    | Picoseconds = 1
    | Nanoseconds = 2
    | Microseconds = 3
    | Milliseconds = 4
    | Seconds = 5

type EtsMode =
    | Off = 0
    | Fast = 1
    | Slow = 2

type Resolution =
    | _8bit = 0
    | _12bit = 1
    | _14bit = 2
    | _15bit = 3
    | _16bit = 4

type ChannelInfo =
    | VoltageOffsetRanges = 0

type BandwidthLimit =
    | Full = 0
    | _20MHz = 1

type InputSettings = 
    { coupling : Coupling
      range : Range
      analogueOffset : float<V>
      bandwidthLimit : BandwidthLimit }

type ChannelSettings =
    | Enabled of InputSettings
    | Disabled 
