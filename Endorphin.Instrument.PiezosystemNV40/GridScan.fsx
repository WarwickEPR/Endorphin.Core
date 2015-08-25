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

module GridScan = 

    let piezojena = connect serialPort 
    
    let scanGrid piezojena (firstAxis: Axis) (secondAxis: Axis) (interval:float32) = asyncChoice{ 
        let! startingCoordinates = PiezojenaNV40.Query.queryAllPositions piezojena 
        let firstChannel = firstAxis.Channel
        let secondChannel = secondAxis.Channel
        let firstLength = firstAxis.Length - interval
        let secondLength = secondAxis.Length - interval 
        let rec scan (firstPosition, secondPosition) = 
            if firstPosition < firstLength then
                let newPosition = (firstPosition + interval, secondPosition)  
                let coordinate = PiezojenaNV40.Coordinate.arrangeCoordinate firstChannel secondChannel newPosition startingCoordinates 
                do! PiezojenaNV40.SetParameters.setAllOutputs coordinate
                scan newPosition 
            else 
                if secondPosition < secondLength then
                    let newFirstPosition = 
                        match startingCoordinates with
                        | (x, y, z) -> x
                    let newSecondPosition = secondPosition + interval 
                    let newPosition = (newFirstPosition, newSecondPosition)
                    scan newPosition 
                else 
                    do! PiezojenaNV40.Query.queryAllPositions 
    ()
                                 
                

         
 
    