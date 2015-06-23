[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Endorphin.Core.List
    let duplicates list =
        let rec findDuplicates acc =
            function
            | []                            -> List.ofSet acc
            | x::xs when List.contains x xs -> findDuplicates (Set.add x acc) xs
            | x::xs                         -> findDuplicates acc xs

        findDuplicates Set.empty list

    let rec prettyPrint =
        function
        | []               -> ""
        | head::[]         -> sprintf "%A" head
        | head::(tail::[]) -> sprintf "%A & %A" head tail
        | head::tail       -> sprintf "%A, %s" head (prettyPrint tail)