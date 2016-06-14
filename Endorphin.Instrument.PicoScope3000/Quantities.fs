// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

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
    let asSeconds = function
        | Interval_fs interval -> (float interval) * 1e-15<s>
        | Interval_ps interval -> (float interval) * 1e-12<s>
        | Interval_ns interval -> (float interval) * 1e-9<s>
        | Interval_us interval -> (float interval) * 1e-6<s>
        | Interval_ms interval -> (float interval) * 1e-3<s>
        | Interval_s  interval -> (float interval) * 1.0<s>

    /// Convert an interval to a long int number of nanoseconds without measure
    let asIntegerNanoseconds (interval:Interval) =
        interval.InFemtoseconds / int64 1e6 * 1L</fs>

    /// Convert an interval to a long int number of femtoseconds without measure
    let asIntegerFemtoseconds (interval:Interval) =
        interval.InFemtoseconds * 1L</fs>
