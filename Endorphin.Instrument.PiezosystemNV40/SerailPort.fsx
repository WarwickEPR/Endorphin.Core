namespace Endorphin.Instrument.PiezosystemNV40

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
    type internal serialConnection = {
        BaudRate: int
        DataBits: int
        StopBits: StopBits
        Parity: Parity
        FlowControl: SerialFlowControl}

    let internal serialConnection = {
        BaudRate = 19200;
        DataBits = 8;
        StopBits = One;
        Parity = ParityNone;    
        FlowControl = FlowControlXOnXOff;}

    let stopBitsNative    = Parsing.stopBitsMap serialConnection.StopBits  
    let partiyNative      = Parsing.parityMap serialConnection.Parity
    let flowControlNative = Parsing.flowControlMap serialConnection.FlowControl 


        
