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
    
    let start = asyncChoice {
        let! coordinate = PiezojenaNV40.Query.queryAllPositions piezojena 
        return coordinate}

    let PositionSetSuccess = PiezojenaNV40.Motion.PositionSet.Publish 

    let scanMap desiredOutput arrayofPoints interval =
        let length = Array.length arrayofPoints - 1          
        let rec scan count = 
            if count < length || count = length then
                let desiredOutput = Array.get arrayofPoints 0
                PiezojenaNV40.Motion.setAllOutputs piezojena desiredOutput interval 
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


         
 
    