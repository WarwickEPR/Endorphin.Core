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

// Test x axis. 
let xAxis = {
    Axis = Channel0
    Length = 100.0}

// Test y aixs.
let yAxis = {
    Axis = Channel1
    Length = 100.0}

// Test interval and resolution. 
let interval = 2.0f 
let resolution = 0.05f   

module IntensityMapping = 

    let PositionSetSuccess = PiezojenaNV40.Motion.PositionSet.Publish 

    //let rec private scanMap piezojena resolution start inital = asyncChoice{
            // if count =  some event trigger then
                // get some 2d coordinate 
                //let! setPosition = PiezojenaNV40.Motion.setGridPosition piezojena xAxis.Axis yAxis.Axis initial start resolution 
                //Async.Sleep 100 |> Async.RunSynchronously 
                //PiezojenaNV40.Motion.PositionSet.Trigger setPosition
                /// let new = wait for event trigger. 
               // return! scanMap piezojena resolution start new  
          //  else 
//                return ()}


let experiment = asyncChoice{
    /// Initalises piezojena, connects to serial port and returns piezojena of type Piezojena.
    let! piezojena = PiezojenaNV40.Initialise.initialise "COM3" 
    /// Sets softstart initalisation. 
    do! PiezojenaNV40.SetModes.setSoftStart piezojena On
    /// Measures starting coordinates.
    let! start = PiezojenaNV40.Motion.queryPosition piezojena 
    // wait for some event to fire and give a 2d coordinate, call inital
    // do! scanMap piezojena resolution start inital 
    return ()
    }
