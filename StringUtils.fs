namespace Endorphin.Core

open NationalInstruments.VisaNS
open System
open System.Threading
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open System.Reactive.Linq
open Endorphin.Core.ObservableExtensions

module StringUtils =
    /// <summary>
    /// Partial active pattern which returns the list of matches (excluding the complete pattern)
    /// for a regular expression.
    /// </summary>
    /// <param name="regex">The regular expression string.</param>
    /// <param name="str">The string which needs to be matched.</param>
    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some (List.tail [ for x in m.Groups -> x.Value ])
        else None

    /// <summary>
    /// Partial active pattern which matches strings which can be parsed as 32-bit integers and
    /// returns the corresponding integer.
    /// </summary>
    /// <param name="str">The string to be parsed.</param>
    let (|ParseInteger|_|) str =
        try
            Some(Int32.Parse(str))
        with _ -> None

    /// <summary>
    /// Partial active pattern which matches strings which can be parsed as double-precision
    /// floating point numbers and returns the corresponding number.
    /// </summary>
    /// <param name="str">The string to be parsed.</param>
    let (|ParseFloat|_|) str =
        try
            Some(Double.Parse(str))
        with _ -> None

    /// <summary>
    /// Partial active pattern which mathes strings which are either "1" or "0", returning either
    /// true or false respectively.
    /// </summary>
    let (|ParseIntegerBool|_|) =
        function
        | "0" -> Some(false)
        | "1" -> Some(true)
        | _ -> None
