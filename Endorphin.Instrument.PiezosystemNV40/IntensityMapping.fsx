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

let serialPort = "COM3"

/// Creates serial port connection. 
let piezojena = connect serialPort 

module IntensityMapping = 
    
    let dummyEvent = new Event<bool>()
    let publishedDummyEvent = dummyEvent.Publish
    publishedDummyEvent.Add (fun boolean -> if boolean = true then printfn "Trigger."
                                            else printfn "Don't trigger.")

    let sleep = async{ 
        Async.Sleep 2000 |> Async.RunSynchronously
        return true} 

    let start = asyncChoice {
        let! coordinate = PiezojenaNV40.Query.queryAllPositions piezojena 
        return coordinate}

    let PositionSetSuccess = PiezojenaNV40.Motion.PositionSet.Publish 

    let scanMap arrayofPoints resolution =
        let length = Array.length arrayofPoints - 1          
        let rec scan count = 
            if count < length || count = length then
                let desiredOutput = Array.get arrayofPoints 0
                PiezojenaNV40.Motion.setAllOutputs piezojena desiredOutput resolution 
                sleep |> Async.RunSynchronously |> dummyEvent.Trigger 
                scan (count + 1)
            else 
                ()  
        ()
                                          
let xAxis = {
    Axis = Channel0
    Length = 100.0f}

let yAxis = {
    Axis = Channel1
    Length = 100.0f}

let interval = 2.0f 
let resolution = 0.05f   

PiezojenaNV40.Initialise.initialise piezojena |> Async.RunSynchronously
let startCoordinates = IntensityMapping.start |> Async.RunSynchronously 
let checkStartCoordinates = function
    | Success coordinate -> coordinate
    | Failure message -> (0.0f, 0.0f, 0.0f) 
let start = checkStartCoordinates startCoordinates       
let arrayofPoints = IntensityMap.Generate.generateGridPoints xAxis yAxis interval start 
IntensityMapping.scanMap arrayofPoints resolution 
 
        
           
   
 
    