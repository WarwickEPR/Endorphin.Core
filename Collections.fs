namespace Microsoft.FSharp.Collections

[<AutoOpen>]
module CollectionExtensions =
    
    [<RequireQualifiedAccess>]
    module List =
        let duplicates list =
          let rec findDuplicates acc =
            function
            | []                            -> List.ofSet acc
            | x::xs when List.contains x xs -> findDuplicates (Set.add x acc) xs
            | x::xs                         -> findDuplicates acc xs

          findDuplicates Set.empty list

        let rec prettyPrintList =
            function
            | []               -> ""
            | head::[]         -> sprintf "%A" head
            | head::(tail::[]) -> sprintf "%A & %A" head tail
            | head::tail       -> sprintf "%A, %s" head (prettyPrintList tail)