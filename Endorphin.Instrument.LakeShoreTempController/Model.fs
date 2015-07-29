namespace Endorphin.Instrument.LakeShoreTempController

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
/// Model for LakeShore model 325 temperature controller.
module Model =
    
    /// Temperature in Kelvin.
    type Temperatue = TemperatureInK of temperature : float<K>
    
    /// Heater output in percent.
    type HeaterOutput = HeaterOutput of output : float<pct>

    /// Heater output range of the temperature controller.
    type HeaterRange = HeaterOff | HeaterLow | HeaterHigh

    /// Control mode of the temperature controller. Open loop represents manual heater output
    /// control whereas closed loop represents PID feedback.
    type ControlMode = OpenLoop | ClosedLoop 

    /// The sensor loop of the temperature controller.
    type Loop = Loop1 | Loop2

    /// Proportional, integral and differential coefficients for the temperature controller output
    /// in closed loop mode.
    type PIDSettings =
        { Proportional : float
          Intergral    : float
          Differential : float }
