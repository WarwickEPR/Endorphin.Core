namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module Model = 
    
    /// Type containing status, Ok or Error. 
    type Status = 
        | Ok
        | Error of Error : string 

    /// Type Piezojena of Piezojena : string where the string is the Piezojena's ID string.
    type Piezojena = Piezojena of Piezojena : string
    /// Extracts the string ID from the type Piezojena.
    let identification (Piezojena ID) = ID 

    /// Type used to store software version, version in the form Major.Minor.Build.
    /// Also stroes the time at which the software version was retrieved.
    type Version = {
        Major: int
        Minor: int
        Build: int
        Date: System.DateTime}

    /// Type containing Piezojena information.
    type PiezojenaInformation = {
        SerialString: string  
        Version: Version}
    
    /// Type containing information nessesary to setup a serial connection with the Piezojena.
    type internal serialConnection = {
        BaudRate: int
        DataBits: int
        StopBits: Piezojena.Protocols.SerialStopBitsKind
        Parity: Piezojena.Protocols.SerialParity
        FlowControl: Piezojena.Protocols.SerialFlowControls }
    
    /// Three Piezojena channels, each channel controls movment in one direction. 
    type Channel = 
        | Channel1
        | Channel2
        | Channel3
    
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
        Time: int<ms>
        StepLimit: int
        Exponent: byte option
        ClosedStep: float
        OpenStep: float} 