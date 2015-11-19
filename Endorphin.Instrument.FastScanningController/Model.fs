namespace Endorphin.Instrument.FastScanningController
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core


[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Motion = 
        type PositionCalibration = 
              { X     : decimal<m/V>
                Y     : decimal<m/V>
                Z     : decimal<m/V> }

type ScanningController = internal ScanningController of scanningController : Visa.Instrument * settings : PositionCalibration