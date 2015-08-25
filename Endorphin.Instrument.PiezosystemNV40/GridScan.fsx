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
    
    /// Creates serial port connection. 
    let piezojena = connect serialPort 
    
    /// Gets the current position of all the channels. 
    let getCoordinates = asyncChoice{ 
        let! coordinate = PiezojenaNV40.Query.queryAllPositions piezojena
        return coordinate}
    
    /// Gets the value of the specified channel. 
    let getValue (channel:Channel) (x:float32, y:float32, z:float32)= 
        match channel with         
            | Channel0 -> x
            | Channel1 -> y
            | Channel2 -> z

    /// Scan a grid of area determined by firstAxis.length and secondAxis.length. 
    /// Scan begins from the current postion of the channels.
    /// Only two channels used in scan, the third channel is held at a constant value. 
    let scanGrid piezojena (firstAxis: Axis) (secondAxis: Axis) (interval:float32) (start: float32*float32*float32) = 
        /// Gets the axis corresponding channels.
        let firstChannel = firstAxis.Channel
        let secondChannel = secondAxis.Channel
        /// The offsets are the starting postions of the two channels that will be used in the scan. 
        let firstOffset  = getValue firstChannel  start 
        let secondOffset = getValue secondChannel start
        /// Calculates the length over which to scan for both channels. 
        /// Uses the length specified in the Axis types, adds the offset so scan will strat in the current position
        /// and subtracts the interval in order to prevent an actuator. 
        let firstLength = firstAxis.Length - interval + firstOffset
        let secondLength = secondAxis.Length - interval + firstOffset
        /// Fucntion for scanning. 
        let rec scan (firstPosition, secondPosition) = asyncChoice{
            /// If first position less than first length then increment by one step. 
            if firstPosition < firstLength then
                let newPosition = (firstPosition + interval, secondPosition)  
                /// Arranges two element tuple coordinate into a three element tuple to be passed into the Pieojenas dll functions. 
                let coordinate = PiezojenaNV40.Coordinate.arrangeCoordinate firstChannel secondChannel newPosition start 
                /// Sets the Piezojena coordinates. 
                do! PiezojenaNV40.SetParameters.setAllOutputs piezojena coordinate  
                do! scan newPosition 
            else 
                /// If firstPosition is larger than first length but second position is not larger than second length the
                /// reset firstPosition and increment second position. 
                if secondPosition < secondLength then
                    let newFirstPosition = 
                        match start with
                        | (x, y, z) -> x
                    let newSecondPosition = secondPosition + interval 
                    let newPosition = (newFirstPosition, newSecondPosition)
                    do! scan newPosition 
                else 
                    return ()  
            }
        scan (firstOffset, secondOffset) |> Async.RunSynchronously 
        
        
        
                                 
                

         
 
    