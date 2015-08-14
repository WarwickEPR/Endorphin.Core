namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module Model = 
    

    type internal serialConnection = {
        BaudRate: int
        DataBits: int
        StopBits: Piezojena.Protocols.SerialStopBitsKind
        Parity: Piezojena.Protocols.SerialParity
        FlowControl: Piezojena.Protocols.SerialFlowControls }

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

    type ActuatorPosition = 
        | PositionX 
        | PositionY 
        | PositionZ 

