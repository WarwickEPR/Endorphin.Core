namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NativeModel

[<RequireQualifiedAccess>]
module Interval =
    let fromFemtoseconds = IntervalInFemtoseconds 
    let fromPicoseconds  = IntervalInPicoseconds  
    let fromNanoseconds  = IntervalInNanoseconds  
    let fromMicroseconds = IntervalInMicroseconds 
    let fromMilliseconds = IntervalInMilliseconds 
    let fromSeconds      = IntervalInSeconds      

[<RequireQualifiedAccess>]
module Voltage =
    let fromVolts = VoltageInVolts
    let zero = VoltageInVolts 0.0f<V>