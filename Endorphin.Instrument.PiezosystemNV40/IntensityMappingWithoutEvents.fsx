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

module IntensityMappingWithoutTriggering = 

    let rec scanMap piezojena count length arrayofPoints resolution = asyncChoice{
            if count < length || count = length then
                let desiredOutput = (Array.get arrayofPoints 0)
                let! setPosition = PiezojenaNV40.Motion.setPosition piezojena desiredOutput resolution 
                Async.Sleep 100 |> Async.RunSynchronously 
                return! scanMap piezojena (count + 1) length arrayofPoints resolution  
            else 
                return ()}

// Test x axis. 
let xAxis = {
    Axis = Channel0
    Length = 100.0}

// Test y aixs.
let yAxis = {
    Axis = Channel1
    Length = 100.0}

// Test interval and resolution. 
let interval = 2.0
let resolution = 0.05    

let experiment = asyncChoice{
   /// Initalises piezojena, connects to serial port and returns piezojena of type Piezojena.
   let! piezojena = PiezojenaNV40.Initialise.initialise "COM3" 
   /// Sets softstart initalisation. 
   do! PiezojenaNV40.SetModes.setSoftStart piezojena On
   /// Measures starting coordinates and generates an array of grid points. 
   let! arrayofPoints = PiezojenaNV40.Motion.generateGrid piezojena xAxis yAxis interval  
   let length = Array.length arrayofPoints
   do! IntensityMappingWithoutTriggering.scanMap piezojena 0 length arrayofPoints resolution 
   // do! IntensityMappingWithoutTriggering.scanMap piezojena 0 length arrayofPoints resolution 
   } 

         
