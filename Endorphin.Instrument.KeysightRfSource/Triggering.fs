namespace Endorphin.Instrument.Keysight

open ExtCore.Control

module Triggering =
    module Translate =
        open Endorphin.Core.StringUtils

        let internal parseExternalTriggerSource str =
            match upperCase str with
            | "TRIG1" | "TRIGGER1" -> Trigger1
            | "TRIG2" | "TRIGGER2" -> Trigger2
            | "PULS" | "PULSE"     -> Pulse
            | _                    -> failwithf "Unexpected external trigger source string: %s" str

        let internal externalTriggerSourceString =
            function
            | Trigger1 -> "TRIG1"
            | Trigger2 -> "TRIG2"
            | Pulse    -> "PULS"

  
        let internal parseInternalTriggerSource str =
            match upperCase str with
            | "PVID" | "PVIDEO" -> PulseVideo
            | "PSYN" | "PSYNC"  -> PulseSync
            | _                 -> failwithf "Unexpected internal trigger source string: %s" str

        let internal internalTriggerSourceString =
            function
            | PulseVideo -> "PVID"
            | PulseSync  -> "PSYN"

        let internal parseTriggerSourceType str =
            match upperCase str with
            | "IMM" | "IMMEDIATE" -> ImmediateType
            | "KEY"               -> TriggerKeyType
            | "BUS"               -> BusType
            | "EXT" | "EXTERNAL"  -> ExternalType
            | "INT" | "INTERNAL"  -> InternalType
            | "TIM" | "TIMER"     -> TimerType
            | _                   -> failwithf "Unexpected trigger source type string: %s." str

        let internal triggerSourceTypeString =
            function
            | ImmediateType  -> "IMM"
            | TriggerKeyType -> "KEY"
            | BusType        -> "BUS"
            | ExternalType   -> "EXT"
            | InternalType   -> "INT"
            | TimerType      -> "TIM"

        let triggerTypePrefix = function
            | StepTrigger -> ""
            | ListTrigger -> ":LIST"

    module Action =
        let private sourceTypeKey trigger = sprintf "%s:TRIGGER:SOURCE" (triggerTypePrefix trigger)
        let setSourceType trigger = IO.setValue triggerSourceTypeString (sourceTypeKey trigger)
        let querySourceType trigger = IO.queryValue parseTriggerSourceType (sourceTypeKey trigger)

        let private externalSourceKey trigger = sprintf "%s:TRIGGER:EXTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setExternalSource trigger = IO.setValue externalTriggerSourceString (externalSourceKey trigger)
        let queryExternalSource trigger = IO.queryValue parseExternalTriggerSource (externalSourceKey trigger)

        let private externalSlopePolarityKey trigger = sprintf "%s:TRIGGER:SLOPE" (triggerTypePrefix trigger)
        let setExternalSlopePolarity trigger = IO.setPolarity (externalSlopePolarityKey trigger)
        let queryExternalSlopePolarity trigger = IO.queryPolarity (externalSlopePolarityKey trigger)

        let private internalSourceKey trigger = sprintf "%s:TRIGGER:INTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setInternalSource trigger = IO.setValue internalTriggerSourceString (internalSourceKey trigger)
        let queryInternalSource trigger = IO.queryValue parseInternalTriggerSource (internalSourceKey trigger)

        let private timerPeriodKey trigger = sprintf "%s:TRIGGER:TIMER" (triggerTypePrefix trigger)
        let setTimerPeriod trigger = IO.setDuration (timerPeriodKey trigger)
        let queryTimerPeriod trigger = IO.queryDuration (timerPeriodKey trigger)

        let setTriggerSource trigger rfSource triggerSource = asyncChoice {
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
