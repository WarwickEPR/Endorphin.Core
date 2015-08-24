namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module Model = 
    
    /// Type switch. 
    type Switch = On|Off

    /// Type containing status, Ok or Error. 
    type Status = 
        | Ok
        | Error of Error : string 

    /// Type Piezojena of Piezojena : string where the string is the Piezojena's ID string.
    type Piezojena = internal Piezojena of stage : Piezojena.Protocols.Nv40Multi.Nv40Multi

    /// Extracts the string ID from the type Piezojena.
    let  id (Piezojena multi) = multi 

    /// Type used to store software version, version in the form Major.Minor.Build.
    /// Also stroes the time at which the software version was retrieved.
    type Version = {
        Major: int
        Minor: int
        Build: int
        Date: System.DateTime}

    /// Three possible stop bits for a serial connection.
    type StopBits = 
        | One
        | OnePointFive
        | Two
    
    /// Parity of the serial connection.
    type Parity = 
        | ParityNone
        | ParityOdd
        | ParityEven
        | ParityMark
        | ParitySpace
    
    /// Flow control for a serial connection. 
    type SerialFlowControl = 
        | FlowControlNone
        | FlowControlDtrDsr
        | FlowControlRtsCts
        | FlowControlXOnXOff

    /// Type containing information nessesary to setup a serial connection with the Piezojena.
    type Serial = {
        BaudRate: int
        DataBits: int
        StopBits: StopBits
        Parity: Parity
        FlowControl: SerialFlowControl}
    
    /// Three Piezojena channels, each channel controls movment in one direction. 
    type Channel = 
        | Channel0
        | Channel1
        | Channel2
    
    /// Three Piezojena modes.
    type Mode = 
        | Normal    
        | IntervalWithAcceleration              
        | Interval                  
    
    type Voltage = Voltage of Voltage : float<V>

    /// Piezojena channels can be controlled in both ope and closed loop modes. 
    type Loop = 
        | ClosedLoop
        | OpenLoop
    
    /// Type used to check posistion of Piezojena on all three axes. 
    type ActuatorPosition = 
        | PositionX 
        | PositionY 
        | PositionZ 
        | PositionNone

    /// Type containing all parameters required to check and use the encoder. 
    type Encoder = {
        Mode: Mode
        Time: int
        StepLimit: int
        Exponent: byte option
        ClosedStep: float32
        OpenStep: float32} 