// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight

open Endorphin.Core

module Triggering =
    module internal Translate =
        /// Convert a machine representation of an external trigger source to an internal
        /// representation.
        let parseExternalTriggerSource str =
            match String.toUpper str with
            | "TRIG1" | "TRIGGER1" -> Trigger1
            | "TRIG2" | "TRIGGER2" -> Trigger2
            | "PULS" | "PULSE"     -> Pulse
            | _                    -> raise << UnexpectedReplyException
                                      <| sprintf "Unexpected external trigger source string: %s" str

        /// Convert an internal representation of an external trigger source to a machine
        /// representation.
        let externalTriggerSourceString = function
            | Trigger1 -> "TRIG1"
            | Trigger2 -> "TRIG2"
            | Pulse    -> "PULS"

        /// Convert a machine representation of an internal trigger source to an internal
        /// representation.
        let parseInternalTriggerSource str =
            match String.toUpper str with
            | "PVID" | "PVIDEO" -> PulseVideo
            | "PSYN" | "PSYNC"  -> PulseSync
            | _                 -> raise << UnexpectedReplyException
                                   <| sprintf "Unexpected trigger source string: %s" str

        /// Convert an internal representation of an internal trigger source to a machine
        /// representation.
        let internalTriggerSourceString = function
            | PulseVideo -> "PVID"
            | PulseSync  -> "PSYN"

        /// Convert a machine representation of a trigger source into an internal representation.
        let parseTriggerSourceType str =
            match String.toUpper str with
            | "IMM" | "IMMEDIATE" -> ImmediateType
            | "KEY"               -> TriggerKeyType
            | "BUS"               -> BusType
            | "EXT" | "EXTERNAL"  -> ExternalType
            | "INT" | "INTERNAL"  -> InternalType
            | "TIM" | "TIMER"     -> TimerType
            | _                   -> raise << UnexpectedReplyException
                                     <| sprintf "Unexpected trigger source type string: %s." str

        /// Convert an internal representation of a trigger source into a machine representation.
        let triggerSourceTypeString = function
            | ImmediateType  -> "IMM"
            | TriggerKeyType -> "KEY"
            | BusType        -> "BUS"
            | ExternalType   -> "EXT"
            | InternalType   -> "INT"
            | TimerType      -> "TIM"

        /// Get the key prefix needed for different types of trigger.
        let triggerTypePrefix = function
            | StepTrigger -> ""
            | ListTrigger -> ":LIST"

    module Control =
        open Translate

        /// Key for the type of the trigger source.
        /// Command reference p.60 for list triggers.
        let private sourceTypeKey trigger = sprintf "%s:TRIGGER:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of trigger source, given a trigger type.
        let setSourceType trigger = IO.setValueString triggerSourceTypeString (sourceTypeKey trigger)
        /// Query the type of trigger source, given a trigger type.
        let querySourceType trigger = IO.queryKeyString parseTriggerSourceType (sourceTypeKey trigger)

        /// Key for the type of external trigger source.
        /// Command reference p.58.
        let private externalSourceKey trigger = sprintf "%s:TRIGGER:EXTERNAL:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of external trigger source, given a trigger type.
        let setExternalSource trigger = IO.setValueString externalTriggerSourceString (externalSourceKey trigger)
        /// Query the value of the external trigger source, given a trigger type.
        let queryExternalSource trigger = IO.queryKeyString parseExternalTriggerSource (externalSourceKey trigger)

        /// Key for the external slope parity.
        /// Command reference p.59.
        let private externalSlopePolarityKey trigger = sprintf "%s:TRIGGER:SLOPE" (triggerTypePrefix trigger)
        /// Set the external slope polarity of the given trigger type to the given value.
        let setExternalSlopePolarity trigger = IO.setPolarity (externalSlopePolarityKey trigger)
        /// Query the external slope polarity of the given trigger type.
        let queryExternalSlopePolarity trigger = IO.queryPolarity (externalSlopePolarityKey trigger)

        /// Key for the internal trigger source.
        /// Command reference p.59.
        let private internalSourceKey trigger = sprintf "%s:TRIGGER:INTERNAL:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of internal trigger source, given a trigger type.
        let setInternalSource trigger = IO.setValueString internalTriggerSourceString (internalSourceKey trigger)
        /// Query the value of the internal trigger source, given a trigger type.
        let queryInternalSource trigger = IO.queryKeyString parseInternalTriggerSource (internalSourceKey trigger)

        /// Key for the period of the timer trigger.
        /// Command reference p.215.
        let private timerPeriodKey trigger = sprintf "%s:TRIGGER:TIMER" (triggerTypePrefix trigger)
        /// Set the period of the timer trigger for the given trigger.
        let setTimerPeriod trigger = IO.setDuration (timerPeriodKey trigger)
        /// Query the period of the timer trigger for the given trigger.
        let queryTimerPeriod trigger = IO.queryDuration (timerPeriodKey trigger)

        /// Set the trigger source of the machine, given a type of trigger and a value to set
        /// the source to.
        let setTriggerSource trigger rfSource triggerSource = async {
            match triggerSource with
            | Immediate  -> do! setSourceType trigger rfSource ImmediateType
            | TriggerKey -> do! setSourceType trigger rfSource TriggerKeyType
            | Bus        -> do! setSourceType trigger rfSource BusType
            | TriggerSource.External (source, polarity) ->
                do! setSourceType trigger rfSource ExternalType
                do! setExternalSource trigger rfSource source
                do! setExternalSlopePolarity trigger rfSource polarity
            | Internal source ->
                do! setSourceType trigger rfSource InternalType
                do! setInternalSource trigger rfSource source
            | Timer period ->
                do! setSourceType trigger rfSource TimerType
                do! setTimerPeriod trigger rfSource period }