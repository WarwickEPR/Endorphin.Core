namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open System.IO.Ports
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point

module Instrument = 
    [<RequireQualifiedAccess>]
    module ScanningController = 
        let openInstrument visaAddress timeout calibration = async {
            let instrument = Visa.openInstrument visaAddress timeout None
            let controller = ScanningController(instrument, calibration)

            return controller }

        let closeInstrument (ScanningController (instrument, _)) =
            Visa.closeInstrument instrument

        let internal pointToVoltage (point : Point) (calibration : PositionCalibration) = 
            (tfst point * 1e-6m<m/um> / calibration.X, tsnd point * 1e-6m<m/um> / calibration.Y, ttrd point * 1e-6m<m/um> / calibration.Z)

        
        let internal voltageToPoint (point : VoltagePoint) (calibration : PositionCalibration) = 
            (tfst point * 1e6m<um/m> * calibration.X, tsnd point * 1e6m<um/m> * calibration.Y, ttrd point * 1e6m<um/m> * calibration.Z)