namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

module Error =    
    
    type Error =
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

    let statusStrings = function
        | NoError                   -> "No errors."
        | ActuatorUnplugged         -> "The actuator is not plugged in."
        | WrongActuatorType         -> "The actuator is of the incorrect type."
        | ActuatorShortCircuitFault -> "Actuator short circuit."
        | Overload                  -> "Value out of range, overshoot."
        | Underload                 -> "Value out of range, undershoot."
        | TemperatureOutofRange     -> "Temperature out range."
        | _                         -> failwithf "Invalid error."

                                                                                                                                         
    let (|Ok|Error|) = function
        | Error.NoError -> Ok  
        | errorCode     -> Error (statusStrings errorCode)