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

module IntensityMapping = 
    
    let dummyEvent = new Event<bool>()
    let publishedDummyEvent = dummyEvent.Publish
    publishedDummyEvent.Add (fun boolean -> if boolean = true then printfn "Trigger."
                                            else printfn "Don't trigger.")

    let sleep = async{ 
        Async.Sleep 2000 |> Async.RunSynchronously
        return true} 

    /// Measures current coordinates.
    let start piezojena = asyncChoice {
        let! coordinate = PiezojenaNV40.Query.queryAllPositions piezojena 
        return coordinate}

    let PositionSetSuccess = PiezojenaNV40.Motion.PositionSet.Publish 

    /// Scans the piezojena over all points in arrayofPoints. 
    let scanMap piezojena arrayofPoints resolution =
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


// Creates serial port connection using serial port stored in the standardSerial.  
let piezojena = connect SerialConnection.standardSerial.Port  

// Test x axis. 
let xAxis = {
    Axis = Channel0
    Length = 100.0f}

// Test y aixs.
let yAxis = {
    Axis = Channel1
    Length = 100.0f}

// Test interval and resolution. 
let interval = 2.0f 
let resolution = 0.05f   

// Initialises the piezojena by setting all channels to remote mode and closed loop mode. 
PiezojenaNV40.Initialise.initialise piezojena |> Async.RunSynchronously
// Gets the current coordinates of the piezojena. 
let startCoordinates = IntensityMapping.start piezojena |> Async.RunSynchronously
// If the piezojenas coordinates have been measured successfully then they are returned, else sets coordinates to (0,0,0) 
let checkStartCoordinates = function
    | Success coordinate -> coordinate
    | Failure message -> (0.0f, 0.0f, 0.0f) 
// Binds starting coordinates to name start.
let start = checkStartCoordinates startCoordinates       
// Generates an array of grid points. 
let arrayofPoints = IntensityMap.Generate.generateGridPoints xAxis yAxis interval start 
// Preforms intensity map using arrayofPoints.
IntensityMapping.scanMap piezojena arrayofPoints resolution 
        
           
   
 
    