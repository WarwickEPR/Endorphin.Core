namespace Endorphin.Utilities.Position

module Path =
    type Path = 
        private { ArrayIndices : (int * int) array
                  Origin       : float * float
                  Scale        : float * float }

    let createSnake origin gridSize stepSize = 
        let isEven x = (x % 2 = 0)

        let numberOfSteps = int (round((float gridSize) / stepSize))
        let path = seq {
            for x in 0 .. numberOfSteps do   
                if isEven x 
                then for y in 0 .. numberOfSteps -> (x,y)
                else for y in numberOfSteps .. -1 .. 0 -> (x,y) }

        { ArrayIndices = path |> Seq.toArray
          Origin = origin
          Scale = (stepSize, stepSize) }
        
    let createOneDirection origin gridSize stepSize = 
        let numberOfSteps = int (round((float gridSize) / stepSize))
        let path = seq {
            for x in 0 .. numberOfSteps do
                for y in 0 .. numberOfSteps -> (x, y) }

        { ArrayIndices = path |> Seq.toArray
          Origin = origin
          Scale = (stepSize, stepSize) }

    let coordinateForPoint point path =
        let (x0, y0) = path.Origin
        let (x, y) = point
        let (dx, dy) = path.Scale
        (x0 + (float x) * dx, y0 + (float y) * dy)

    let points path = 
        path.ArrayIndices |> Array.ofSeq

    let pointAtIndex i path =
        path.ArrayIndices.[i]

    let coordinatesAtIndex i path =
        path |> coordinateForPoint path.ArrayIndices.[i]