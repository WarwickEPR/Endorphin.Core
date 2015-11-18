namespace Endorphin.Instrument.FastScanningController
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Setup = 
        
        [<AutoOpen>]
        module Timing =
            type DwellTime = decimal<ms>

            type SettlingTime = decimal<us>

        [<AutoOpen>]
        module Motion = 
            type PositionCalibration = 
                { X     : decimal<m/V>
                  Y     : decimal<m/V>
                  Z     : decimal<m/V> }

    type ScanningController = internal { PortName : string; Calibration : PositionCalibration }


