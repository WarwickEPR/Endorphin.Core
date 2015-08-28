﻿namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open ExtCore.Control.Choice
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40

[<AutoOpen>]
module internal SerialConnection = 
    
    /// The device identification string. 
    let private identification (Piezojena ID) = ID     

    /// Standard serial connection configuration for the Piezojena. 
    let standardSerial portName = {
        BaudRate = 19200
        DataBits = 8
        StopBits = One
        Parity = ParityNone
        FlowControl = FlowControlXOnXOff
        Port = portName}
    
    /// Makes a serial connection using the standardSerial values.
    let Standard portName = 
        let serialConnect = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
        let stage = serialConnect.ConnectNv40MultiToSerialPort (portName) 
        Piezojena stage 

    /// Makes serial connection using user input values. 
    let configureandConnect (serial:Serial)  portName =
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

    (*let close portname = 
        let serialConnection = new Piezojena.Protocols.SerialPortConnection ()
        serialConnection.*)

    /// Opens the piezojena.  
    let private openInstrument serialPort =
        let multiServices = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
        let stage = multiServices.ConnectNv40MultiToSerialPort (serialPort) 
        Piezojena stage
    
    /// Extracts the stage from the piezojena type, used to access piezojena functions. 
    let getPiezojena serialPort = identification (openInstrument serialPort)