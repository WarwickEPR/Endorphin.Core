namespace Endorphin.Instrument.LakeShoreTempController

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core

exception Command of string
exception Parameter of string
exception UnexpectedReply of string

[<AutoOpen>]
/// Model for LakeShore model 325 temperature controller.
module Model =

    [<AutoOpen>]
    module Instrument =
        
        /// LakeShore model 325 temperature controlller.    
        type TempController = internal TempController of tempController : Visa.Instrument

        /// LakeShore model 325 temperature controller identity details.
        type Identity =
            { Manufacturer : string
              ModelNumber  : string
              SerialNumber : string
              Version      : string }

    /// Temperature in Kelvin.
    type Temperature = Temperature_K of temperature : float<K>
    
    /// Heater output in percent.
    type HeaterOutput = HeaterOutput of output : float<pct>

    /// Heater output range of the temperature controller.
    type HeaterRange = HeaterOff | HeaterLow | HeaterHigh

    /// Control mode of the temperature controller.
    type ControlMode =
        | ManualPid
        | ZoneMode
        | OpenLoop
        | AutoTunePid
        | AutoTunePi
        | AutoTuneP 

    /// The sensor loop of the temperature controller.
    type Loop = Loop1 | Loop2

    /// Proportional, integral and differential coefficients for the temperature controller output
    /// in closed loop mode.
    type PidSettings =
        { Proportional : float
          Integral     : float
          Differential : float }
    
    /// Event status byte indicating the instrument status.
    type StandardEventStatus = StandardEventStatus of status : byte