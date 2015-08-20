#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/ExtCore.0.8.45/lib/net45/ExtCore.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../Endorphin.Instrument.PiezosystemNV40\Piezojena.Protocols.dll"
#r "../Endorphin.Instrument.PiezosystemNV40\Piezojena.Protocols.Nv40Multi.dll"
#r "bin/Debug/Endorphin.Instrument.PiezosystemNV40.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Windows.Forms
open ExtCore.Control
open FSharp.Control.Reactive
open Endorphin.Core
open Endorphin.Instrument.PiezosystemNV40

module Connection =  


         
    let private multiServices = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
    let private stage = multiServices.ConnectNv40MultiToSerialPort("COM3")      
       
    let mutable error : string = Unchecked.defaultof<_>
    let mutable array = [|Unchecked.defaultof<_>; Unchecked.defaultof<_>; Unchecked.defaultof<_>|]
    stage.GetMeasuredValueChunk (&array)
       
   
   
   
   
   
   
     
    
    //let serialConfiguration = new Piezojena.Protocols.SerialConfiguration()
    //sprintf "myface %s" <| serialConfiguration.BaudRate.ToString()
    //sprintf "databits: %s" <| serialConfiguration.DataBits.ToString()
    //
    //let serialPort = new System.IO.Ports.SerialPort("COM3")
    //let serialConnection = new Piezojena.Protocols.SerialPortConnection(serialPort, serialConfiguration)
    //
    //let multiService = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
    //let myStage = multiService.ConnectNv40MultiToSerialPort("COM3")
    //myStage.SetDesiredOutput(byte 0, 10.0f)
    //Piezojena.Protocols.Nv40Multi.Nv40Multi.
    //j.

    //let serialConfiguration = new Piezojena.Protocols.SerialConfiguration()
    //serialConfiguration.BaudRate     <- 19200 
    //serialConfiguration.DataBits     <- 8
    //serialConfiguration.StopBits    <- Piezojena.Protocols.SerialStopBitsKind.One
    //serialConfiguration.Parity      <- Piezojena.Protocols.SerialParity.None
    //serialConfiguration.FlowControl <- Piezojena.Protocols.SerialFlowControls.XOnXOff
    //let write = new Piezojena.Protocols.Nv40Multi.Nv40MultiServices()
    //let mystage = write.ConnectNv40MultiToSerialPort("COM3")
    //mystage.SetDesiredOutput (byte 1, 10.0f)
    //mystage.SetDesiredOutput (byte 1, 1.0f)
    //mystage.SetDesiredOutput (byte 2, 5.0f)
    //mystage.SetDesiredOutput (byte 3, 56.0f)
    
    //let multi = Piezojena.Protocols.Nv40Multi.N
    //let myStage = multiService.ConnectNv40MultiToSerialPort("COM3")
    //myStage.SetDesiredOutput(byte 0, 10.0f)
    //
    //sprintf "yourface %s" <| serialConnection.Configuration.BaudRate.ToString()

    

  
 

