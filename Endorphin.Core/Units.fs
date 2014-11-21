namespace Endorphin.Core

module Units =
    // Note: only use these for instruments which have parameters which are in discrete setps of the unit.
    // Otherwise, use the SI units in Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
    [<Measure>] type ns // nanoseconds
    [<Measure>] type us // microseconds
    [<Measure>] type ms // milliseconds