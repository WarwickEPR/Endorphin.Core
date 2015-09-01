namespace Endorphin.Core

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =
    let ofSet set = Set.toList set

    let duplicates list =
        let rec findDuplicates acc = function
            | []                            -> ofSet acc
            | x::xs when List.contains x xs -> findDuplicates (Set.add x acc) xs
            | x::xs                         -> findDuplicates acc xs
        findDuplicates Set.empty list

    /// Prettily print out a list.
    let prettyPrint list =
        let rec loop acc = function
            | [] -> acc
            | head :: [] -> sprintf "%s & %A" acc head
            | head :: (tail :: []) -> sprintf "%s, %A & %A" acc head tail
            | head :: tail -> loop (sprintf "%s, %A" acc head) tail
        let (acc, list') =
            match list with
            | [] -> ("", [])
            | head :: tail -> ((sprintf "%A" head), tail)
        loop acc list'

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Map =
    let findArray keys map = keys |> Array.map (fun key -> Map.find key map)

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    /// Prepend a single value onto the front of a sequence.
    let prependSingleton value sequence = seq { yield value; yield! sequence }
