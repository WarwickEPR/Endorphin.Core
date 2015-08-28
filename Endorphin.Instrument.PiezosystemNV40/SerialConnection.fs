namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open ExtCore.Control.Choice
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

[<AutoOpen>]
module SerialConnection = 
    
    /// The device identification string. 
    let private identification (Piezojena ID) = ID     

    /// Standard serial connection configuration for the Piezojena. 
    let standardSerial = {
        BaudRate = 19200
        DataBits = 8
        StopBits = One
        Parity = ParityNone
        FlowControl = FlowControlXOnXOff
        Port = "COM3"}
    
    /// Makes a serial connection using the standardSerial values.
    let connect serialPort = 
        let serialConfiguration = new Piezojena.Protocols.SerialConfiguration()
        serialConfiguration.BaudRate    <- standardSerial.BaudRate
        serialConfiguration.DataBits    <- standardSerial.DataBits
        serialConfiguration.StopBits    <- Parsing.stopBitsMap standardSerial.StopBits
        serialConfiguration.Parity      <- Parsing.parityMap standardSerial.Parity
        serialConfiguration.FlowControl <- Parsing.flowControlMap standardSerial.FlowControl
        let serialConnect = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
        let config = serialConnect.CreateSerialPortConnection (serialPort, serialConfiguration)
        let stage = serialConnect.ConnectNv40MultiToSerialPort (serialPort) 
        Piezojena stage 

    /// Makes serial connection using user input values. 
    let connectandConfigure (serial:Serial)  portName=
        /// Sets up serial port configuration using serial port settings contained in record type Serial. 
        let serialConfiguration = new Piezojena.Protocols.SerialConfiguration()
        serialConfiguration.BaudRate    <- serial.BaudRate
        serialConfiguration.DataBits    <- serial.DataBits
        serialConfiguration.StopBits    <- Parsing.stopBitsMap serial.StopBits
        serialConfiguration.Parity      <- Parsing.parityMap serial.Parity
        serialConfiguration.FlowControl <- Parsing.flowControlMap serial.FlowControl
        /// Connects to port using serialConfiguration. 
        let serialConnection = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices ()
        serialConnection.CreateSerialPortConnection (portName, serialConfiguration)

    /// Opens the piezojena.  
    let private openInstrument serialPort =
        let multiServices = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
        let stage = multiServices.ConnectNv40MultiToSerialPort (serialPort) 
        Piezojena stage
    
    /// Extracts the stage from the piezojena type, used to access piezojena functions. 
    let getPiezojena serialPort = identification (openInstrument serialPort)