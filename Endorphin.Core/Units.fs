namespace Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Units =

    // Note: only use these for instruments which have parameters which are in discrete steps of the unit.
    // Otherwise, use the SI units in Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    
    /// Nanoseconds.
    [<Measure>] type ns

    /// Microseconds.
    [<Measure>] type us

    /// Milliseconds.
    [<Measure>] type ms

    /// Binary bits.
    [<Measure>] type bits
    
    /// Decimal places.
    [<Measure>] type dp

    /// Significant figures.
    [<Measure>] type sf

    /// dBm i.e. decibels with respect to 1 mW of power
    [<Measure>] type dBm

    /// dB decibels
    [<Measure>] type dB

    /// Hz (cycles per second)
    [<Measure>] type Hz = 1/s

    /// Percent
    [<Measure>] type pct
    let fractionToPercentage fraction   = fraction * 100.0<pct>
    let percentageToFraction percentage = percentage / 100.0<pct>

    /// Degrees
    [<Measure>] type deg

    /// Radians
    [<Measure>] type rad

    let radiansToDegrees angle = angle * (180.0<deg/rad> / System.Math.PI)
    let degreesToRadians angle = angle * (System.Math.PI / 180.0<deg/rad>)
