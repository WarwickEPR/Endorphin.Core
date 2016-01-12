namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open System.IO.Ports
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Utilities.Position.Point
open Endorphin.Utilities.Position.Path

module Instrument = 
    [<RequireQualifiedAccess>]
    module ScanningController = 
        /// Open connection to the fast scanning controller using the provided voltage -> displacement calibration
        let openInstrument visaAddress timeout calibration = async {
            let (configuration : Visa.SerialConfiguration) = 
                                { BaudRate  = 460800
                                  StopBits  = Visa.StopBitMode.One
                                  Parity    = Visa.ParityMode.NoParity
                                  DataBits  = 8s }
            let instrument = Visa.openSerialInstrument visaAddress timeout None configuration
            let controller = ScanningController(instrument, calibration)
            return controller }

        /// Close connection to the fast scanning controller
        let closeInstrument (ScanningController (instrument, _)) =
            Visa.closeInstrument instrument

    module Position = 
        /// Get current stage position
        let getPosition controller = async {
            let! voltages = IO.getCurrentVoltages controller
            return voltageToPoint controller voltages }          

        /// Set current stage position
        let setPosition controller (point: Point) = async {
            let voltages = pointToVoltage controller point
            do! IO.setCurrentVoltages controller voltages }

        module Path = 
            /// Write a path to the controller for subsequent playback
            let writePathToController controller (path : Path) = async {
                do! IO.writePath controller path }

            /// Run the path stored on the controller
            let runPath controller = 
                IO.runPath controller

            /// Stop playback of the path (if running)
            let stopPath controller = 
                IO.stopPath controller

            /// Get number of points of the path stored on the controller
            let getNumberOfPoints controller = 
                IO.getNumberOfPoints controller

    module Timing = 
        /// Set the time the stage is stable after each acquisition trigger
        let setDwellTime controller (dwellTime : int<ms>) = async {
            do! IO.setDwell controller dwellTime }

        /// Set the settling time after each movement before the acquisition trigger is fired
        let setTriggerDelay controller (delayTime: float<ms>) = async {
            do! IO.setTriggerDelay controller delayTime }
        
            