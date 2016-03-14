// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.HMC804x

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Functions to translate between internal and machine representations.
[<AutoOpen>]
module internal Parsing =
    /// Convert a machine representation of an on/off state into an internal representation.
    let parseOnOffState str =
        match String.toUpper str with
        | "0" | "OFF" -> Off
        | "1" | "ON"  -> On
        | str         -> raise << UnexpectedReplyException <| sprintf "Unexpected on-off string: %s." str

    /// Convert an internal representation of an on/off state into a machine representation.
    let onOffStateString = function
        | Off -> "OFF"
        | On  -> "ON"

    let parseOutput str =
        match String.toUpper str with
        | "OUT1" | "OUTPUT1" -> OUT1
        | "OUT2" | "OUTPUT2" -> OUT1
        | "OUT3" | "OUTPUT3" -> OUT1
        | str -> raise << UnexpectedReplyException <| sprintf "Unexpected output channel string: %s." str

    let outputString = function
        | OUT1 -> "OUT1"
        | OUT2 -> "OUT2"
        | OUT3 -> "OUT3"

    let parseInterpolation = function
        | "0" -> Step
        | "1" -> Interpolate
        | str -> raise << UnexpectedReplyException <| sprintf "Unexpected interpolation: %s." str

    let interpolationString = function
        | Step        -> 0
        | Interpolate -> 1

    let parseCurrent (str:string) =
        Current_A (float str * 1.0<A>)

    let currentString (Current_A current) = sprintf "%e" (float current)

    let parseVoltage (str:string) =
        Voltage_V (float str * 1.0<V>)

    let voltageString (Voltage_V voltage) = sprintf "%e" (float voltage)

    let parseTime str =
        Time_s (float str * 1.0<s>)

    let timeString (Time_s time) = sprintf "%e" (float time)

    let parsePower str =
        Power_W (float str * 1.0<W>)

    let powerString (Power_W power) = sprintf "%e" (float power)

    let commaSep : seq<string> -> string = String.concat ","

    let arbString : ArbSequence -> string =
        Seq.map (fun (v,i,t,interpolation) ->
                        sprintf "%s,%s,%s,%d"
                           (voltageString v)
                           (currentString i)
                           (timeString t)
                           (interpolationString interpolation))
        >> commaSep

    let arbRepetitions = function
        | Repetitions n -> n
        | Forever -> 0us

    let arbTriggerModeString = function
        | Single  -> "SING"
        | Run     -> "RUN"

    let parseTriggerMode str =
        match String.toUpper str with
        | "SING"
        | "SINGLE"  -> Single
        | "RUN"     -> Run
        | str       -> raise << UnexpectedReplyException <| sprintf "Unexpected trigger mode string: %s." str