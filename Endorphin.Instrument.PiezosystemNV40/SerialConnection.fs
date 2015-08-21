namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open ExtCore.Control.Choice
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

module SerialConnection = 
    
    let connect (serial:Serial) =
        /// Sets up serial port configuration using serial port settings contained in record type Serial. 
        let serialConfiguration = new Piezojena.Protocols.SerialConfiguration()
        serialConfiguration.BaudRate    <- serial.BaudRate
        serialConfiguration.DataBits    <- serial.DataBits
        serialConfiguration.StopBits    <- Parsing.stopBitsMap (serial.StopBits)
        serialConfiguration.Parity      <- Parsing.parityMap (serial.Parity)
        serialConfiguration.FlowControl <- Parsing.flowControlMap (serial.FlowControl)
        /// Connects to port using serialConfiguration. 
        let serialConnection = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices ()
        serialConnection.CreateSerialPortConnection ("COM3", serialConfiguration)

    /// Standard serial connection configuration for the Piezojena. 
    let standardSerial = {
        BaudRate = 19200
        DataBits = 8
        StopBits = One
        Parity = ParityNone
        FlowControl = FlowControlXOnXOff}
         
