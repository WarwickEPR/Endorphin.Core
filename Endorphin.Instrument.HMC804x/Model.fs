// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.HMC804x

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

exception InstrumentErrorException of string seq
exception UnexpectedReplyException of string

[<AutoOpen>]
module Model =

    [<AutoOpen>]
    module Instrument =
        /// An opened and connected RF source, which can have commands written to it.
        type IVSource = internal HMC804x of Visa.Instrument

        /// A record of the identification information a device provides.
        type DeviceId = {
            Manufacturer : string
            ModelNumber : string
            SerialNumber : string
            Version : string }

        /// Model numbers that are recognised by the program.
        type ModelNumber = HMC8041 | HMC8042 | HMC8043
        type Output = OUT1 | OUT2 | OUT3

        /// A returned error, including its code and the associated message.
        type Error = { Code : int ; Message : string }

    type Voltage = Voltage_V of float<V>
    type Current = Current_A of float<A>
    type Power   = Power_W   of float<W>
    type Time    = Time_s    of float<s>

    /// A toggle state, where something can either be On or Off.
    type OnOffState = On | Off

    type Interpolation = Interpolate | Step

    type ArbPoint = Voltage * Current * Time * Interpolation
    type ArbSequence = ArbPoint seq
    type ArbRepetitions =
    | Repetitions of uint16 // Up to 255
    | Forever
    type ArbTriggerMode = Single | Run

    type ArbSettings = {
        Sequence    : ArbSequence
        Repetitions : ArbRepetitions
        Triggering  : OnOffState
        TriggerMode : ArbTriggerMode }

    type OutputSetting =
    | Fixed of (Voltage * Current)
    | Arb   of ArbSettings

    type PowerLimit = MaximumPower of Power
    
    type Settings = {
        Output     : OutputSetting
        PowerLimit : PowerLimit option }

    type PowerSupplySettings = Map<Output,Settings>