namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
/// Twickenham superconducting magnet controller model types.
module Model =
   
    [<AutoOpen>]
    module Instrument =
        /// Parameters specifying the magnet controller hardware configuratoin.
        type HardwareParameters =
            { MaximumCurrent      : decimal<A>
              CalibratedRampRates : decimal<A/s> list }

        /// Software-enforced output limits for the magnet controller.
        type Limits =
            { RampRateLimit    : decimal<A/s>
              TripVoltageLimit : decimal<V>
              CurrentLimit     : decimal<A> }

        /// Calibration parameters for the magnetic field.
        type FieldCalibration =
            { StaticField       : decimal<T> 
              LinearCoefficient : decimal<T/A> }

        /// Calibration parameters for the output monitoring shunt.
        type ShuntCalibration =
            { VoltageOffset     : decimal<V>
              LinearCoefficient : decimal<V/A>
              RmsVoltageNoise   : decimal<V> }
   
        /// Magnet controller settings including hardware configuration, output limits and calibration parameters.
        type Settings =
            { HardwareParameters : HardwareParameters
              Limits             : Limits
              FieldCalibration   : FieldCalibration
              ShuntCalibration   : ShuntCalibration
              LastUpdated        : DateTime }

        /// Twickenham superconducting magnet controller.
        type MagnetController = internal MagnetController of magnetController : Visa.Instrument * settings : Settings

    /// Output current direction.
    type CurrentDirection = Forward | Reverse

    /// Magnet controller output ramp target.
    type RampTarget = Zero | Lower | Upper

    /// Magnet controller output parameters.
    type OutputParameters = 
        { OutputCurrent : decimal<A>
          OutputVoltage : decimal<V>
          RampTarget    : RampTarget }

    /// Magnet controller current parameters.
    type CurrentParameters = 
        { RampTarget    : RampTarget
          ReachedTarget : bool
          IsPaused      : bool }
    
    /// Magnet controller operating parameters.
    type OperatingParameters = 
        { RampRate         : decimal<A/s>
          CurrentDirection : CurrentDirection }

    /// Magnet controller set point parameters.
    type SetPointParameters = 
        { LowerSetPoint : decimal<A>
          UpperSetPoint : decimal<A>
          TripVoltage   : decimal<V> }