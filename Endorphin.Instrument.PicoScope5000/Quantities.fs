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
    
    let asSeconds =
        function
        | IntervalInFemtoseconds   interval -> (float interval) * 1e-15<s>
        |  IntervalInPicoseconds   interval -> (float interval) * 1e-12<s>
        |  IntervalInNanoseconds   interval -> (float interval) * 1e-9<s>
        |  IntervalInMicroseconds  interval -> (float interval) * 1e-6<s>
        |  IntervalInMilliseconds  interval -> (float interval) * 1e-3<s>
        |  IntervalInSeconds       interval -> (float interval) * 1.0<s>

[<RequireQualifiedAccess>]
module Voltage =
    let fromVolts = VoltageInVolts
    let zero = VoltageInVolts 0.0f<V>