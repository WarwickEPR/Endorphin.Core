// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic

[<RequireQualifiedAccess>]
/// Functions for specifying trigger settings
module Trigger =

    /// Specifies an automatic trigger with the given delay in milliseconds.
    let auto delay =
        if delay <= 0s<ms> then
            invalidArg "Auto-trigger delay" "Auto-trigger delay must be positive and non-zero."
        { Trigger = AutoTrigger; Delay = None; Auto = Some (AutoTriggerDelay_ms delay) }

    module General =

        let simple range direction level source =
            let adcThreshold = Adc.voltageToAdc range level
            { Trigger = SimpleTrigger { Source = source; Threshold = adcThreshold; Direction = direction }
              Delay = None; Auto = None }

        let withDelayInSamples delay trigger =
            { trigger with Delay = Some delay }

        let withAutoTrigger delay trigger =
            { trigger with Auto = Some <| AutoTriggerDelay_ms delay }

        open Complex

        let thresholdWithoutHysteresis range voltage =
            { Threshold = voltage; Hysteresis = None }

        let thresholdWithHysteresis range hysteresis voltage =
            { Threshold = voltage; Hysteresis = Some hysteresis }

        let thresholdWithPercentageHysteresis range (percentage:float<pct>) (volts:Voltage) =
            let fraction = float32 <| percentageToFraction percentage
            let hysteresis = fraction * volts
            { Threshold = volts; Hysteresis = Some <| hysteresis }

        let complex trigger = { Trigger = ComplexTrigger <| trigger; Delay = None; Auto = None }

        let analogue condition state channel =
            let source = Complex.Channel channel
            { TriggerState = Complex.Require (source,state)
              AnalogueConditions = [source,condition] |> Map.ofList
              DigitalConditions = Map.empty }

        let digital condition state channel =
            let source = Complex.DigitalTrigger
            { TriggerState = Complex.Require (source,state)
              AnalogueConditions = Map.empty
              DigitalConditions = [channel,condition] |> Map.ofList }

        let channelA = AnalogueTrigger ChannelA
        let channelB = AnalogueTrigger ChannelB
        let channelC = AnalogueTrigger ChannelC
        let channelD = AnalogueTrigger ChannelD

        // Triggers can be boolean combinations of trigger states
        let private merge ma mb = Map.fold (fun m k v -> Map.add k v m) ma mb
        let (.&) a b =
            { TriggerState = And (a.TriggerState,b.TriggerState)
              AnalogueConditions = merge a.AnalogueConditions b.AnalogueConditions
              DigitalConditions = merge a.DigitalConditions b.DigitalConditions }
        let (.|) a b =
            { TriggerState = Or (a.TriggerState,b.TriggerState)
              AnalogueConditions = merge a.AnalogueConditions b.AnalogueConditions
              DigitalConditions = merge a.DigitalConditions b.DigitalConditions }

        // Convert a threshold from a voltage to an Adc
        let internal thresholdToAdc range threshold =
            { AdcThreshold = Adc.voltageToAdc range threshold.Threshold
              Hysteresis = Option.map (Adc.voltageToAdc range) threshold.Hysteresis }

//        let L = Voltage.fromVolts >> thresholdWithPercentageHysteresis Range_1V 5.0<pct>
//        let th = L 1.0f<V>
//        let risingEdge = analogue (L 1.0f<V> |> Rising) true
//        let high = digital High true
//        let t = risingEdge channelA .& risingEdge channelB .| high D1 |> complex |> withAutoTrigger 10s<ms> |> withDelayInSamples 1000

    module Streaming =

        let private range (inputs:AcquisitionInputs) source =
            match source with
            | AnalogueTrigger channel ->
                let inputChannel  = Analogue channel
                try
                    let inputSettings = Map.find inputChannel inputs.InputSettings
                    match inputSettings with
                    | AnalogueSettings settings -> settings.Range
                    | DigitalSettings _ -> failwith "Analogue channel with digital settings!"
                with
                    | :? KeyNotFoundException -> failwithf "Failed to find input settings for channel %A" inputChannel
            | t -> failwithf "Range unknown for trigger source %A" t

        let internal thresholdToAdc (acquisition:AcquisitionParameters) channel = General.thresholdToAdc (range acquisition.Inputs channel)

        let withSimpleTrigger direction level source parameters =
            General.simple (range parameters source) direction level source

        module Trigger =

            let withDelayInSamples delay trigger parameters =
                { trigger with Delay = Some delay }

            let withAutoTrigger = General.withAutoTrigger

    module Block =
     ()