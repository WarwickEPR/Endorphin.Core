namespace Endorphin.Core

module Units =
    // Note: only use these for instruments which have parameters which are in discrete setps of the unit.
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