namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NativeModel

[<RequireQualifiedAccess>]
/// Functions for creating and manipulating time intervals.
module Interval =

    /// Creates an integer time interval in femtoseconds.
    let fromFemtoseconds = Interval_fs 

    /// Creates an integer time interval in picoseconds.
    let fromPicoseconds  = Interval_ps  

    /// Creates an integer time interval in nanoseconds.
    let fromNanoseconds  = Interval_ns  

    /// Creates an integer time interval in microseconds.
    let fromMicroseconds = Interval_us

    /// Creates an integer time interval in milliseconds.
    let fromMilliseconds = Interval_ms 

    /// Creates an integer time interval in seconds.
    let fromSeconds      = Interval_s      
    
    /// Converts the given time interval to a floating point value in seconds.
    let asSeconds =
        function
        | Interval_fs  interval -> (float interval) * 1e-15<s>
        | Interval_ps  interval -> (float interval) * 1e-12<s>
        | Interval_ns  interval -> (float interval) * 1e-9<s>
        | Interval_us  interval -> (float interval) * 1e-6<s>
        | Interval_ms  interval -> (float interval) * 1e-3<s>
        | Interval_s   interval -> (float interval) * 1.0<s>

[<RequireQualifiedAccess>]
/// Functions related to sample voltages.
module Voltage =
    
    /// Converts the given ADC counts value sampled at the specified voltage range and resolution
    /// to a voltage.
    let fromAdcCounts resolution range analogueOffset (adcCounts : AdcCount) =
        Range.voltage range * ((float32 adcCounts) / (float32 <| Resolution.maximumAdcCounts resolution)) - analogueOffset