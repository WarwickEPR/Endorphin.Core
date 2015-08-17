﻿namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Parsing = 
    
    /// Converts type byte option into type byte.
    /// Used on exponent field in record type Encoder. 
    let internal byteOptionConvert = function 
        | Some byte -> byte
        | None -> 0uy

    /// Converts type channel into bytes. 
    let channelByte = function
        | Channel1 -> 1uy 
        | Channel2 -> 2uy 
        | Channel3 -> 3uy
     
    /// Converts channel bytes into type channel.  
    let parseChannel = function
        | 1uy -> Channel1
        | 2uy -> Channel2
        | 3uy -> Channel3
        | xuy         -> failwithf "Not a valid channel: %A" xuy

    /// Converts type ActuatorPosition into type expected by the NativeApi functions (ActuatorCoordinate).
    let actuatorPositiontoCoordinate = function
        | PositionX -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X
        | PositionY -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y
        | PositionZ -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z
        | PositionNone -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.None

    /// Converts type expected by the NativeApi functions (ActuatorCoordinate) into type AcuatorPosition.
    /// Includes fields Phi and Theta, can be returned from NativeApi functions. 
    let parseActuatorPosition= function
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X     -> PositionX
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y     -> PositionY
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z     -> PositionZ
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Phi   -> failwithf "Phi positioning not avalible."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Theta -> failwithf "Theta positioning not avalible."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.None  -> PositionNone
        | _ -> failwithf "Not a valid coordinate axis."

    /// Converts type mode into type expected by NativeApi functions (EncoderMode).
    let modetoEncoderMode = function
        | Normal               ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal               
        | IntervalWithAcceleration ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration
        | Interval             ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval     

    /// Converts type expected by NativeApi functions (EncoderMode) to type Mode. 
    let parseMode = function 
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal                   -> Normal     
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration -> IntervalWithAcceleration         
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval                 -> Interval            
        |  str -> failwithf "Not a valid mode: %A" str

    /// Converts type Loop to type boolean.
    let loopBoolean = function
        | OpenLoop  -> false
        | ClosedLoop -> true 

    /// Converts typr boolean to type Loop.
    let parseLoop = function
        | false -> OpenLoop
        | true  -> ClosedLoop
        | _ -> failwithf "Not a valid boolean."

  
