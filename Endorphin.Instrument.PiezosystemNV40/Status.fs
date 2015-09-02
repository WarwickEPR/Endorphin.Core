namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<AutoOpen>]
module internal Status =    
    
    type ErrorCode =
        | NoError
        | ActuatorUnplugged
        | WrongActuatorType
        | ActuatorShortCircuitFault
        | Overload
        | Underload
        | TemperatureOutofRange

    let private errorMap = function
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.None                      -> NoError
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.ActuatorNotPluggedIn      -> ActuatorUnplugged
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.WrongActuatorType         -> WrongActuatorType
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.ActuatorShortCircuitFault -> ActuatorShortCircuitFault 
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.Overload                  -> Overload
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.Underload                 -> Underload
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.TemperatureOutOfRange     -> TemperatureOutofRange
        | _                         -> failwithf "Invalid error."

    let statusString = function
        | NoError                   -> "OK. No errors."                         
        | ActuatorUnplugged         -> "The actuator is not plugged in."
        | WrongActuatorType         -> "The actuator is of the incorrect type."
        | ActuatorShortCircuitFault -> "Actuator short circuit."
        | Overload                  -> "Value out of range, overshoot."
        | Underload                 -> "Value out of range, undershoot."
        | TemperatureOutofRange     -> "Temperature out range."
     
    let parseStatus = function 
        | "OK. No errors."                         -> NoError                  
        | "The actuator is not plugged in."        -> ActuatorUnplugged        
        | "The actuator is of the incorrect type." -> WrongActuatorType        
        | "Actuator short circuit."                -> ActuatorShortCircuitFault
        | "Value out of range, overshoot."         -> Overload                 
        | "Value out of range, undershoot."        -> Underload                
        | "Temperature out range."                 -> TemperatureOutofRange                    
        | _ -> failwithf "Not a valid error."

