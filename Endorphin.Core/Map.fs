namespace Endorphin.Core

[<AutoOpen>]
module Map =
    let updateKey (key : 'Key) (updateFunc : 'T option -> 'T) (map : Map<'Key, 'T>) =
        map 
        |> Map.add key (updateFunc (Map.tryFind key map))
        
        