namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open System.IO.Ports
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Instrument = 
    [<RequireQualifiedAccess>]
    module ScanningController = 
        let openInstrument visaAddress timeout calibration = async {
            let instrument = Visa.openInstrument visaAddress timeout None
            let controller = ScanningController(instrument, calibration)

            return controller }

        let internal pointToVoltage point (calibration : PositionCalibration)) = 
            (tfst point * 1e-6m<m/um> / calibration.X, tsnd point * 1e-6m<m/um> / calibration.Y, ttrd point * 1e-6m<m/um> / calibration.Z)