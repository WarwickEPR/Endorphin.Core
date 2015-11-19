namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open System.IO.Ports
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point
open Endorphin.Utilities.Position.Path

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

    module Position = 
        let getPosition controller = async {
            let! voltages = IO.getCurrentVoltages controller
            return voltageToPoint controller voltages }          

        let setPosition controller (point: Point) = async {
            let voltages = pointToVoltage controller point
            do! IO.setCurrentVoltages controller voltages }

        let writePathToController controller (path : Path) = async {
            do! IO.writePath controller path
            }

        let runPath controller = 
            IO.runPath controller

        let stopPath controller = 
            IO.stopPath controller

    module Timing = 
        let setDwellTime controller (dwellTime : int<ms>) = async {
            do! IO.setDwell controller dwellTime
        }
            