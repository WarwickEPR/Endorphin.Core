namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Text
open Endorphin.Core
open log4net 
open ExtCore.Control
open System.Runtime.InteropServices
open Endorphin.Instrument.PiezosystemNV40


module IntensityMap = 
     
    [<AutoOpen>]
    module Manipulations = 
       
       let internal addTuples (x:float,y:float,z:float) (a:float,b:float,c:float) = (x + a, y + b, z + c)
       let internal multiplyTuple (x:float,y:float, z:float) (a:float,b:float,c:float) = (x*a , y*b, z*c)

       /// Orders tuple.
       let internal orderTuple (first:Channel) (second:Channel) (x, y) = 
           let firstEnum = 
               match first with 
               | Channel0 -> 0
               | Channel1 -> 1
               | Channel2 -> 2

           let secondEnum = 
               match second with
               | Channel0 -> 0
               | Channel1 -> 1
               | Channel2 -> 2

           if firstEnum < secondEnum then
               (x, y)
           else
               (y, x)
        
       /// Converts tuple with elements of type float into a tuple with elements of type float32.
       let typetoFloat32 (coordinate: float*float*float) = 
           let a =
                match coordinate with
                | (x, y, z) -> float32(x)
           let b =
                match coordinate with
                | (x, y, z) -> float32(y)
           let c =  
                match coordinate with 
                | (x, y, z) -> float32(z) 
           (a, b, c)   

       /// Converts tuple with elements of type float32 into a tuple with elements of type float.
       let typetoFloat (coordinate: float32*float32*float32) = 
           let a =
                match coordinate with
                | (x, y, z) -> float(x)
           let b =
                match coordinate with
                | (x, y, z) -> float(y)
           let c =  
                match coordinate with 
                | (x, y, z) -> float(z) 
           (a, b, c)   

    module Coordinate = 

        /// Returns a tuple cotaining 1's and 0's, 1's indicate that the channel is not in use. 
        let private findEmptyChannels (firstChannel:Channel) (secondChannel:Channel) = 
            let channels = [|Channel0; Channel1; Channel2|] 
            //if firstChannel = secondChannel then 
            //    let emptyChannels = Array.filter (fun elem -> elem <> firstChannel) channels
            //    let first  = Array.get emptyChannels 0
            //    let second = Array.get emptyChannels 1    
            //    let firstTuple = 
            //        match first with
            //        | Channel0 -> (1.0,0.0,0.0)
            //        | Channel1 -> (0.0,1.0,0.0)
            //        | Channel2 -> (0.0,0.0,1.0)
            //    let secondTuple = 
            //        match second with 
            //        | Channel0 -> (1.0,0.0,0.0)
            //        | Channel1 -> (0.0,1.0,0.0)
            //        | Channel2 -> (0.0,0.0,1.0)
            //    let addTuples (x:float,y:float,z:float) (a:float,b:float,c:float) = (x + a, y + b, z + c)
            //    addTuples firstTuple secondTuple 
            //else 
            let emptyChannels = Array.filter (fun elem -> elem <> firstChannel && elem <> secondChannel) channels
            let first = Array.get emptyChannels 0 
            let firstTuple = 
                match first with 
                | Channel0 -> (1.0,0.0,0.0)
                | Channel1 -> (0.0,1.0,0.0)
                | Channel2 -> (0.0,0.0,1.0)   
            firstTuple
        

        /// Stores starting coordinates of the channels not in use, these will remain fixed. 
        let private fixedCoordinates (firstChannel: Channel) (secondChannel: Channel) startingPosition =
            let empty = findEmptyChannels firstChannel secondChannel
            let fixedCoordinates = multiplyTuple startingPosition empty 
            fixedCoordinates 

        /// Expands a two element tuple containing desired position (on a 2D grid) into a 3 element tuple, contains zero's
        /// for channels not in use. 
        let private expandCoordinates (firstChannel:Channel) (secondChannel: Channel) desiredPosition = 
            let empty = findEmptyChannels firstChannel secondChannel
            let orderedPosition = orderTuple firstChannel secondChannel desiredPosition
            let fullCoordinate (x, y) (a, b, c) =
                if a = 0.0 then 
                    if b = 0.0 then
                        (x , y , 0.0)
                    else 
                        (x, 0.0, y)
                else
                    (0.0, x, y)
            fullCoordinate orderedPosition empty 

        /// Adds fixed coordinate tuple to expanded desired coordinate tuple to get full coordinates. 
        let arrangeCoordinate (first:Channel) (second:Channel) desired start =
             let fix = fixedCoordinates first second start 
             let expanded = expandCoordinates first second desired 
             let coordinate = addTuples expanded fix
                   
             coordinate 


     module Generate =          
         
         /// Gets the value of the specified channel. 
         let private getValue (channel:Channel) (x:float, y:float, z:float) = 
             match channel with         
                 | Channel0 -> x
                 | Channel1 -> y
                 | Channel2 -> z
     
         let private generateGridPointsList (firstAxis:Axis) (secondAxis:Axis) interval start = 
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
             // Maximums are the maxiumu values the first and second channels will be set to. 
             let firstMaximum  = firstLength  + firstOffset
             let secondMaximum = secondLength + firstOffset
             
             // An empty list to write into.
             let points = []
     
             /// This functions generates points from 0 to firstMaximum in steps of interval, the value of the second channel is
             /// held constant.
             let rec generateUp (first, second) coordinateList = 
                 // If second is above secondMaximum then scan is not preformed, returns the unchanged list. 
                 if second > secondMaximum then
                     coordinateList
                 else         
                     let rec generate (firstPosition, secondPosition) list =         
                         if firstPosition < firstMaximum || firstPosition = firstMaximum then
                             let coordinate = Coordinate.arrangeCoordinate firstChannel secondChannel (firstPosition, secondPosition) start
                             let newList = coordinate :: list 
                             let newPosition = (firstPosition + interval, secondPosition)
                             generate newPosition newList  
                         else 
                             list
                     generate (first + interval, second) coordinateList              
             
             /// This functions generates points from firstMaximum to 0 in steps of interval, the value of the second channel is
             /// held constant.        
             let generateDown (first, second) coordinateList =   
                // If second is above secondMaximum then scan is not preformed, returns the unchanged list. 
                if second > secondMaximum then
                    coordinateList
                else      
                    let rec generate (firstPosition, secondPosition) list = 
                        if firstPosition > 0.0 || firstPosition = 0.0 then 
                            let coordinate = Coordinate.arrangeCoordinate firstChannel secondChannel (firstPosition, secondPosition) start
                            let newlist = coordinate :: list 
                            let newPosition = (firstPosition - interval, secondPosition) 
                            generate newPosition newlist 
                        else 
                            list
                    generate (first - interval, second) coordinateList
             
             /// This function preforms a full grid generation, preforms generateUp, then it preforms generateDown with
             /// secondPosition set one interval higher. Once both function have executed two rows will have been generated,
             /// secondPosition is set one interval higher and the generateAll function repeats.  
             let rec generateAll (firstPosition, secondPosition) list = 
                 if secondPosition < secondMaximum then
                     let uplist = generateUp (firstPosition, secondPosition) list
                     let newSecond = secondPosition + interval
                     let downlist = uplist |> generateDown (firstMaximum, newSecond) 
                     generateAll (firstPosition, newSecond + interval) downlist 
                 // If secondPosition = secondMaximum then only one more row is required to complete grid, so only generteUp
                 // is used. Wll always require generate up as the function generateAll always finished on generateDown.  
                 elif secondPosition = secondMaximum then
                     let uplist = generateUp (firstPosition, secondPosition) list
                     uplist
                 // If neither conditions met it means that secondPosition > secondMaximum, the grid generation is complete
                 // the list containing all points is returned. 
                 else 
                     list 
         
             generateAll (firstOffset, secondOffset) points
        
         /// Generates a list of grid points and converts to an array. 
         let generateGridPoints (firstAxis:Axis) (secondAxis:Axis) (interval:float) (start:float32*float32*float32) = 
             let newstart = typetoFloat start
             let list = generateGridPointsList firstAxis secondAxis interval newstart
             list |> List.toArray 

         