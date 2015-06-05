namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.NationalInstruments

// Command set of the Keysight RF instrument
// Implements functions to modify & query configuration
// Organised by subsystem mirroring the Keysight configuration

[<RequireQualifiedAccess>]
module RfSource =
    let openInstrument  = IO.Connect.openInstrument
    let closeInstrument = IO.Connect.closeInstrument
    let queryIdentity = IO.Identify.queryIdentity



