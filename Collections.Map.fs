[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Endorphin.Core.Map

let findArray keys map = 
    keys
    |> Array.map (fun key -> Map.find key map)