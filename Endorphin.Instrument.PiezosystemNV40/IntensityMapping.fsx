﻿#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
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

    /// Gets the current position of all the channels. 
    let getCoordinates = asyncChoice{ 
        let! coordinate = PiezojenaNV40.Query.queryAllPositions piezojena
        return coordinate}

    /// Generates grid with generate grid points. 
    let getGrid firstAxis secondAxis interval = asyncChoice{
        let! start = getCoordinates 
        let gridList = IntensityMap.Generate.generateGridPoints firstAxis secondAxis interval start
        return gridList}
    
    /// Sets the piezojena's position. 
    let setPosition piezojena desiredPosition = asyncChoice{
        do! PiezojenaNV40.SetParameters.setAllOutputs piezojena desiredPosition
        do Async.Sleep 100 |> Async.RunSynchronously
        let! currentPosition = PiezojenaNV40.Query.queryAllPositions piezojena
        return currentPosition }    

    /// Scans over the grid points to create an intensity map.
    let createMap piezojena points (start: float32*float32*float32)  = 
        let initial = List.item 0 points 
        let numberofPoints = List.length points 
        
        let rec scan count (desiredPosition: float32*float32*float32) = asyncChoice{ 
            if count > numberofPoints then
                return ()
            else 
               let! currentPosition = setPosition piezojena desiredPosition         
               if currentPosition = desiredPosition then 
                   do! scan (count + 1) (List.item count points)
               else 
                    do! scan count desiredPosition
               }
        scan 0 initial |> Async.RunSynchronously
                                          
let xAxis = {
    Axis = Channel0
    Length = 100.0f}

let yAxis = {
    Axis = Channel1
    Length = 100.0f}

let interval = 2.0f    

let start = IntensityMapping.getCoordinates |> Async.RunSynchronously |> Choice.bindOrFail  
let points = IntensityMapping.getGrid xAxis yAxis interval |> Async.RunSynchronously |> Choice.bindOrFail
IntensityMapping.createMap piezojena points start 

         
 
    