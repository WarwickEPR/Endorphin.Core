namespace Endorphin.Core

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =

    /// Trims any characters found in the `chars` array from the end of the string.
    let trimEnd chars (str : string) = str.TrimEnd chars

    /// Trims any characters found in the `chars` array from the start of the string.
    let trimStart chars (str : string) = str.TrimStart chars

    /// Split the string into an array of strings, breaking at any character in the `seps` array.
    let split seps (str : string) = str.Split seps

    /// Convert a string to upper case.
    let toUpper (str : string) = str.ToUpper ()

    /// Convert a string to lower case.
    let toLower (str : string) = str.ToLower ()

    /// Parses each comma-separated substring in a string using the provided parsing function and 
    /// returns the elements.
    let parseCsvSeq  parseFunc (str : string) = str.Split [|','|] |> Seq.map parseFunc
    
    /// Returns a comma-separated string, consisiting of the elements of the provided sequence
    /// mapped onto the provided string conversion function.
    let csvSeqString stringFunc = Seq.map stringFunc   >> String.concat ","

    /// Returns a hexidecimal string for a provided byte array.
    let hexOfBytes (bytes : byte array) =
        let hex = new StringBuilder((Array.length bytes) * 2)
        for byte in bytes do
            hex.AppendFormat("{0:x2}", byte) |> ignore
        
        hex.ToString()
