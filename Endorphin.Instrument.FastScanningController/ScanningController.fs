namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open System.IO.Ports
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point

module Instrument = 
    [<RequireQualifiedAccess>]
    module ScanningController = 
        let openInstrument visaAddress timeout calibration = async {
            let (configuration : Visa.SerialConfiguration) = 
                                { BaudRate  = 460800
                                  StopBits  = Visa.StopBitMode.One
                                  Parity    = Visa.ParityMode.NoParity
                                  DataBits  = 8s }
            let instrument = Visa.openSerialInstrument visaAddress timeout None configuration
            let controller = ScanningController(instrument, calibration)
            return controller }

        let closeInstrument (ScanningController (instrument, _)) =
            Visa.closeInstrument instrument

    [<AutoOpen>]
    module internal Convert = 
        let pointToVoltage (ScanningController (_, calibration)) (point : Point) = 
            (tfst point * 1e-6m<m/um> / calibration.X, tsnd point * 1e-6m<m/um> / calibration.Y, ttrd point * 1e-6m<m/um> / calibration.Z)
     
        let voltageToPoint (ScanningController (_, calibration)) (point : VoltagePoint) = 
            (tfst point * 1e6m<um/m> * calibration.X, tsnd point * 1e6m<um/m> * calibration.Y, ttrd point * 1e6m<um/m> * calibration.Z)

    module Position = 
        let getPosition controller = async {
            let! voltages = IO.getCurrentVoltages controller
            return voltageToPoint controller voltages }          

        let setPosition controller (point: Point) = async {
            let voltages = pointToVoltage controller point
            do! IO.setCurrentVoltages controller voltages }