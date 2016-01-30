// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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

    /// Cons an element onto a list.
    let cons list value = value :: list

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Map =
    let findArray keys map = keys |> Array.map (fun key -> Map.find key map)

    /// Get a set of the keys in a map.
    let keys map = Map.fold (fun set key _ -> Set.add key set) Set.empty map

    /// Get a set of the values in a map.
    let values map = Map.fold (fun set _ value -> Set.add value set) Set.empty map

    /// Create a union of two maps.
    let union map1 map2 = Map.fold (fun map key value -> Map.add key value map) map2 map1

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Set =
    /// Extract the minimum value from a set.
    let extractMin set =
        let min = Set.minElement set
        (min, Set.remove min set)

    /// Extract the maximum value from a set.
    let extractMax set =
        let max = Set.maxElement set
        (max, Set.remove max set)

    /// Apply a reduction to a set.
    let reduce reduction set =
        let (state, set') = extractMin set
        Set.fold reduction state set'


[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    /// Prepend a single value onto the front of a sequence.
    let prependSingleton (value : 'T) sequence = seq { yield value;     yield! sequence }

    /// Append a single value onto the front of a sequence.
    let appendSingleton  (value : 'T) sequence = seq { yield! sequence; yield value }
