namespace Endorphin.Instrument.LakeShoreTempController

open Endorphin.Core

[<RequireQualifiedAccess>]
/// Functions for extracting information about the device status from status bytes.
module internal Status =
    
    /// True if a command causes an execution error (e.g. parameter out of range).
    let executionError (EventStatus status) = Byte.nthBit 4 status

    /// True if an invalid command is sent (e.g. syntax error).
    let commandError   (EventStatus status) = Byte.nthBit 5 status