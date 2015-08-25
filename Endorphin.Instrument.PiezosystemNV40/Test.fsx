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

/// Gets the value of the specified channel. 
let getValue (channel:Channel) (x:float32, y:float32, z:float32) = 
    match channel with         
        | Channel0 -> x
        | Channel1 -> y
        | Channel2 -> z

let generateSequence (firstAxis:Axis) (secondAxis:Axis) (interval:float32) (start:float32*float32*float32) = 
    let firstChannel  = (firstAxis.Axis) 
    let secondChannel = (secondAxis.Axis)
    // The offsets are the starting postions of the two channels that will be used in the scan. 
    let firstOffset  = getValue firstChannel start 
    let secondOffset = getValue secondChannel start
    // Calculates the length over which to scan for both channels. 
    // Uses the length specified in the Axis types, adds the offset so scan will strat in the current position
    // and subtracts the interval in order to prevent an actuator. 
    let firstLength  = firstAxis.Length
    let secondLength = secondAxis.Length
    let newfirstLength  = firstLength - interval + firstOffset
    let newsecondLength = secondLength - interval + firstOffset
    let points = []
    let rec generate (firstPosition, secondPosition) list = 
        if firstPosition < newfirstLength then
            let coordinate = PiezojenaNV40.Coordinate.arrangeCoordinate firstChannel secondChannel (firstPosition, secondPosition) start
            let newList = coordinate :: list 
            let newPosition = (firstPosition + interval, secondPosition)
            generate newPosition newList  
        else 
            // If firstPosition is larger than first length but second position is not larger than second length the
            // reset firstPosition and increment second position. 
            if secondPosition < secondLength then
                let newFirstPosition = 
                    match start with
                    | (x, y, z) -> x
                let newSecondPosition = secondPosition + interval 
                let newPosition = (newFirstPosition, newSecondPosition)
                generate newPosition list 
            else 
                list 
    generate (firstOffset, secondOffset) points        

let xAxis = {
    Axis = Channel0;
    Length = 100.0f;}

let yAxis = {
    Axis = Channel1;
    Length = 100.0f;}

generateSequence xAxis yAxis 2.0f (0.0f, 0.0f, 5.0f)
