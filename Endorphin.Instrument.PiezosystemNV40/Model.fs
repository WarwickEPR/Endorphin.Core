namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module Model = 

    type Channel = 
        | Channel1
        | Channel2
        | Channel3
    
    type Mode = 
        | Normal                  
        | Interval                  
        | IntervalAcceleration 

    type Voltage = Voltage of Voltage : float<V>

    type Loop = 
        | Closedloop
        | Openloop

    type Posistion = 
        | X of x : float<um>
        | Y of y : float<um>
        | Z of z : float<um>

