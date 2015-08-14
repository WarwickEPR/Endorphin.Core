namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

module Errors = 
    
    let errorStrings = function 
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.None                      -> "No errors."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.ActuatorNotPluggedIn      -> "The actuator is not plugged in."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.WrongActuatorType         -> "The actuator is of the wrong type."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.ActuatorShortCircuitFault -> "Actuator short circuit fail."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.Overload                  -> "Out of range, overshot."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.Underload                 -> "Out of range, undershot."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiErrors.TemperatureOutOfRange     -> "The temperature is out of range."
        
