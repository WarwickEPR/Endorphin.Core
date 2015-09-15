namespace Endorphin.Core

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    
    /// Partial active pattern which returns the list of matches (excluding the complete pattern)
    /// for a regular expression.
    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match str
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    /// Partial active pattern which matches strings which can be parsed as 32-bit integers and
    /// returns the corresponding integer.
    let (|ParseInteger|_|) str =
        try
            Some (Int32.Parse str)
        with _ -> None

    /// Partial active pattern which matches strings which can be parsed as double-precision
    /// floating point numbers and returns the corresponding number.
    let (|ParseFloat|_|) str =
        try
            Some(Double.Parse str)
        with _ -> None

    /// Partial active pattern which mathes strings which are either "1" or "0", returning either
    /// true or false respectively.
    let (|ParseIntegerBool|_|) =
        function
        | "0" -> Some false
        | "1" -> Some true
        | _ -> None

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
