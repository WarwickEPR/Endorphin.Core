namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
module Trigger =
    let inputChannel = InputChannelTrigger
    let external     = ExternalTrigger
    let auxiliary    = AuxiliaryTrigger
    
    let auto delay =
        if delay <= 0s<ms> then
            failwith "Auto-trigger delay must be positive and non-zero."
        AutoTrigger (AutoTriggerDelayInMilliseconds delay)

    let simple triggerChannel adcThreshold levelThreshold startSample =
        { TriggerChannel     = triggerChannel 
          AdcThreshold       = adcThreshold
          ThresholdDirection = levelThreshold
          StartSample        = SampleIndex startSample
          AutoTrigger        = None }
    
    let simpleAndAuto triggerChannel adcThreshold levelThreshold startSample autoTriggerDelay =
        { TriggerChannel     = triggerChannel 
          AdcThreshold       = adcThreshold
          ThresholdDirection = levelThreshold
          StartSample        = SampleIndex startSample
          AutoTrigger        = Some <| AutoTriggerDelayInMilliseconds autoTriggerDelay }