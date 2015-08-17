namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module Model = 
   
    type Piezojena = Piezojena of Piezojena : string
    let identification (Piezojena ID) = ID 

    type PiezojenaInformation = {
        SerialString : string  
        Version :        }

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
        | IntervalWithAcceleration              
        | Interval                  
 
    type Voltage = Voltage of Voltage : float<V>

    type Loop = 
        | ClosedLoop
        | OpenLoop

    type ActuatorPosition = 
        | PositionX 
        | PositionY 
        | PositionZ 
        | PositionNone

    type Encoder = {
        Mode: Mode
        Time: int<ms>
        StepLimit: int
        Exponent: byte option
        ClosedStep: float
        OpenStep: float} 