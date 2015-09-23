namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
/// Functions for specifying trigger settings.
module Trigger =

    /// Specifies the given input channel as a trigger channel.
    let inputChannel = InputChannelTrigger

    /// Specifies the external ("EXT") channel as a trigger channel.
    let external     = ExternalTrigger

    /// Specifies the auxiliary ("AUX") channel as a trigger channel.
    let auxiliary    = AuxiliaryTrigger
    
    /// Specifies an automatic trigger with the given delay in milliseconds.
    let auto delay =
        if delay <= 0s<ms> then
            failwith "Auto-trigger delay must be positive and non-zero."
        AutoTrigger (AutoTriggerDelay_ms delay)

    /// Specifies simple trigger settings with the provided trigger channel but not auto-trigger.
    let simple triggerChannel adcThreshold levelThreshold startSample =
        { TriggerChannel     = triggerChannel 
          AdcThreshold       = adcThreshold
          ThresholdDirection = levelThreshold
          StartSample        = startSample
          AutoTrigger        = None }
    
    /// Specifies simple trigger settings with the provided trigger channel and auto-trigger.
    let simpleAndAuto triggerChannel adcThreshold levelThreshold startSample autoTriggerDelay =
        { TriggerChannel     = triggerChannel 
          AdcThreshold       = adcThreshold
          ThresholdDirection = levelThreshold
          StartSample        = startSample
          AutoTrigger        = Some <| AutoTriggerDelay_ms autoTriggerDelay }