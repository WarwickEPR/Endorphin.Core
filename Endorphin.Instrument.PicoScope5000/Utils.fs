namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Units
open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Enumeration representing the available device information which can be requested from the PicoScope driver.
type PicoInfo = 
    | DriverVersion = 0
    | UsbVersion = 1
    | HardwareVersion = 2
    | ModelNumber = 3
    | SerialNumber = 4
    | CalibrationDate = 5
    | KernelVersion = 6
    | DigitalHardwareVersion = 7
    | AnalogueHardwareVersion = 8
    | FirmwareVersion1 = 9
    | FirmwareVersion2 = 10
  
/// Represents the possible states which can be set to the front panel LED of the PicoScope 5000 device.
type LedFlash =
    | LedOff
    | LedRepeat of counts : int16
    | LedIndefiniteRepeat