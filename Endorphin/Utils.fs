module Utils

open System
open NationalInstruments.VisaNS
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// <summary>
/// Returns an asynchronous computation which reads the next string from an NI VISA
/// device session.
/// </summary>
/// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
/// which represents the NI VISA message-based instrument control session.</param>
let readFromSession (session : MessageBasedSession) =
    // Creates an asynchronous computation from the begin/end asynchronous pattern.
    // See the documentation for Async.FromBeginEnd for details.
    Async.FromBeginEnd(
        (fun (callback, state) -> session.BeginRead(session.DefaultBufferSize, callback, state)),
        session.EndReadString)

/// <summary>
/// Returns an asynchronous computation which writes a string message to an NI VISA
/// device session.
/// </summary>
/// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
/// which represents the NI VISA message-based instrument control session.</param>
/// <param name="message">The message to be written.</param>
let writeToSesiion (session : MessageBasedSession) message = 
    // Creates an asynchronous computation from the begin/end asynchronous pattern.
    // See the documentation for Async.FromBeginEnd for details.
    Async.FromBeginEnd(
        (fun (callback, state) -> session.BeginWrite(message, callback, state)),
        session.EndWrite)

/// <summary>
/// Returns an asynchronous computation which first writes a string message to an NI
/// VISA device session and then awaits a response to that message.
/// </summary>
/// <param name="session">The <see cref="NationalInstruments.VisaNS.MessageBasedSession" />
/// which represents the NI VISA message-based instrument control session.</param>
/// <param name="message">The message to be written.</param>
let querySession (session : MessageBasedSession) message =
    async {
        // This code protects the two asynchronous workflows form cancellation. If the request
        // is started but the data is not read out from the buffer, this could cause problems
        // with subsequent commands.
        Async.Start((writeToSesiion session message), CancellationToken.None)
        let readTask = Async.StartAsTask((readFromSession session), TaskCreationOptions.None, CancellationToken.None)
        return! Async.AwaitTask(readTask) }

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