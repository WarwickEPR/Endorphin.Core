namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NativeModel

[<RequireQualifiedAccess>]
/// Functions for creating and manipulating time intervals.
module Interval =

    /// Creates an integer time interval in femtoseconds.
    let fromFemtoseconds = IntervalInFemtoseconds 

    /// Creates an integer time interval in picoseconds.
    let fromPicoseconds  = IntervalInPicoseconds  

    /// Creates an integer time interval in nanoseconds.
    let fromNanoseconds  = IntervalInNanoseconds  

    /// Creates an integer time interval in microseconds.
    let fromMicroseconds = IntervalInMicroseconds

    /// Creates an integer time interval in milliseconds.
    let fromMilliseconds = IntervalInMilliseconds 

    /// Creates an integer time interval in seconds.
    let fromSeconds      = IntervalInSeconds      
    
    /// Converts the given time interval to a floating point value in seconds.
    let asSeconds =
        function
        | IntervalInFemtoseconds  interval -> (float interval) * 1e-15<s>
        | IntervalInPicoseconds   interval -> (float interval) * 1e-12<s>
        | IntervalInNanoseconds   interval -> (float interval) * 1e-9<s>
        | IntervalInMicroseconds  interval -> (float interval) * 1e-6<s>
        | IntervalInMilliseconds  interval -> (float interval) * 1e-3<s>
        | IntervalInSeconds       interval -> (float interval) * 1.0<s>

[<RequireQualifiedAccess>]
/// Functions for creating and manipulating voltages.
module Voltage =
    
    /// Creates a floating point voltage in volts.
    let fromVolts = VoltageInVolts

    /// Creates a voltage corresponding to zero volts.
    let zero = VoltageInVolts 0.0f<V>