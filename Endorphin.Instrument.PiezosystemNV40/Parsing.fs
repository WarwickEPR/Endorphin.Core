namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Parsing = 
    
    let channelByte = function
        | Channel1 -> 00000001uy 
        | Channel2 -> 00000010uy 
        | Channel3 -> 00000011uy

    let parseChannel = function
        | 00000001uy -> Channel1
        | 00000010uy -> Channel2
        | 00000011uy -> Channel3
        | uy         -> failwithf "Not a valid channel: %A" uy

    let actuatorPositionEnum = function
        | PositionX -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X
        | PositionY -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y
        | PositionZ -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z

    let parseActuatorPosition= function
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X -> PositionX
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y -> PositionY
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z -> PositionZ
        | _ -> failwithf "Not a valid coordinate axis."

    let modeEncoderMode = function
        | Normal               ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal               
        | Interval             ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval     
        | IntervalAcceleration ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration

    let parseMode = function 
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal                   -> Normal              
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval                 -> Interval            
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration -> IntervalAcceleration
        |  str -> failwithf "Not a valid mode: %A" str



