namespace Endorphin.Utilities.Position

open Point
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Path =
    type Plane = 
        | XY
        | XZ
        | YZ

    type Path = 
        private { ArrayIndices : (int * int) array
                  Origin       : Point
                  Scale        : decimal<um> * decimal<um>
                  Plane        : Plane }

    let createSnake origin (gridSize : int<um>) stepSize plane = 
        let isEven x = (x % 2 = 0)

        let numberOfSteps = int (round((float gridSize) / (float (stepSize/1.0m<um>))))
        let path = seq {
            for x in 0 .. numberOfSteps do   
                if isEven x 
                then for y in 0 .. numberOfSteps -> (x,y)
                else for y in numberOfSteps .. -1 .. 0 -> (x,y) }

        { ArrayIndices = path |> Seq.toArray
          Origin = origin
          Plane = plane
          Scale = (stepSize, stepSize) }
        
    let createOneDirection origin gridSize stepSize plane = 
        let numberOfSteps = int (round((float gridSize) / (float (stepSize/1.0m<um>))))
        let path = seq {
            for x in 0 .. numberOfSteps do
                for y in 0 .. numberOfSteps -> (x, y) }

        { ArrayIndices = path |> Seq.toArray
          Origin = origin
          Scale = (stepSize, stepSize)
          Plane = plane }

    let coordinateForPoint (point : int * int) path =
        let (x0, y0, z0) = path.Origin

        match path.Plane with
        | XY ->
            let (x, y) = point
            let (dx, dy) = path.Scale
            (x0 + (decimal x) * dx, y0 + (decimal y * dy), z0)
        | XZ ->
            let (x, z) = point
            let (dx, dz) = path.Scale
            (x0 + (decimal x) * dx, y0, z0 + (decimal z * dz))
        | YZ ->
            let (y, z) = point
            let (dy, dz) = path.Scale
            (x0, y0 + (decimal y * dy), z0 + (decimal z * dz))

    let points path = 
        path.ArrayIndices |> Array.ofSeq

    let pointAtIndex i path =
        path.ArrayIndices.[i]

    let coordinatesAtIndex i path =
        path |> coordinateForPoint path.ArrayIndices.[i]