﻿module Utils

open System
open NationalInstruments.VisaNS
open System.Text.RegularExpressions

let readFromSession (session : MessageBasedSession) =
    Async.FromBeginEnd(
        (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
        session.EndReadString)

let writeToSesiion (session : MessageBasedSession) message = 
    Async.FromBeginEnd(
        (fun (callback, state) -> session.BeginWrite(message, callback, state)),
        session.EndWrite)

let querySession (session : MessageBasedSession) message =
    async {
        do! writeToSesiion session message 
        return! readFromSession session }

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

let (|ParseInteger|_|) str =
    try
        Some(Int32.Parse(str))
    with _ -> None

let (|ParseFloat|_|) str =
    try
        Some(Double.Parse(str))
    with _ -> None

let (|ParseIntegerBool|_|) =
    function
    | "0" -> Some(false)
    | "1" -> Some(true)
    | _ -> None