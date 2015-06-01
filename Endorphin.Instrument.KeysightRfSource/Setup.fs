namespace Endorphin.Instrument.Keysight

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core.CollectionUtils
open Endorphin.Instrument.Keysight.Translate
open ExtCore.Control

// Construction and setup of a Keysight RF configuration

module Setup =

    module Sweep =
        let fixedPowerInDbm power = FixedAmplitude <| PowerInDbm power
        let fixedFrequencyInHz frequency = FixedFrequency <| FrequencyInHz frequency
        let frequencySweepInHz a b = FrequencySweep <| range (FrequencyInHz a) (FrequencyInHz b)
        let powerSweepInDbm a b = AmplitudeSweep <| range (PowerInDbm a) (PowerInDbm b)

        // Taking defaults from the documented *RST values
        let defaultStepSweep = { Frequency = fixedFrequencyInHz 1e9<Hz>
                                 Amplitude = fixedPowerInDbm -110.0<dBm>
                                 Points = 101
                                 Spacing = LinearStepSpacing
                                 Options = { Direction = Up
                                             StepTrigger = Immediate
                                             ListTrigger = Immediate
                                             DwellTime = Some ( DurationInSec 2e-3<s> )
                                             Retrace = On
                                             AttentuationProtection = On
                                             Mode = Auto }}

        let withPoints points config = { config with Points = points }
        let withSpacing spacing config = { config with Spacing = spacing }
        let withDirection direction config = { config with Options = { config.Options with Direction = direction } }
        let withDwellTime time config = { config with Options = { config.Options with DwellTime = time } }
        let withStepTrigger trigger config = { config with Options = { config.Options with StepTrigger = trigger } }
        let withListTrigger trigger config = { config with Options = { config.Options with ListTrigger = trigger } }
        let withRetrace state config = { config with Options = { config.Options with Retrace = state } }
        let withAttenuationProtection state config = { config with Options = { config.Options with AttentuationProtection = state } }
        let withFixedPowerInDbm power config = { config with StepSweep.Amplitude = fixedPowerInDbm power }
        let withFixedFrequencyInHz frequency config = { config with StepSweep.Frequency = fixedFrequencyInHz frequency }

        let frequencyStepSweepInHz start finish = { defaultStepSweep with Frequency = frequencySweepInHz start finish }
        let powerStepSweepInDbm start finish = { defaultStepSweep with Amplitude = powerSweepInDbm start finish }



    module Modulation =
        // Prepare function settings
        let basicFunctionSettings = { Shape = Sine
                                      Frequency = FrequencyInHz 1.0e3<Hz>
                                      PhaseOffset = PhaseInRad 0.0<rad> }
        let withShape shape (settings : FunctionSettings) =
            { settings with Shape = shape }
        let withFrequencyInHz frequency (settings : FunctionSettings) =
            { settings with Frequency = FrequencyInHz frequency }
        let withPhaseOffsetInRadians phase (settings : FunctionSettings) =
            { settings with PhaseOffset = PhaseInRad phase }

        let internalSineSourceInHz frequency =
            InternalSource (Function1, basicFunctionSettings |> withFrequencyInHz frequency )

        let internalGeneralSourceInHz frequency shape phase =
            let settings = basicFunctionSettings
                           |> withShape shape
                           |> withFrequencyInHz frequency
                           |> withPhaseOffsetInRadians phase
            InternalSource (Function1, settings)


        let consistentModulationSettings settings =
            
            let duplicateChannels = settings |> List.map modulationChannel |> duplicates
            let duplicateSources  = settings |> List.map modulationSource |> duplicates
            // When PM is added, check that PM and FM paths are exclusive

            if not duplicateChannels.IsEmpty then
                let duplicateAM = List.filter (function (``AM Channel`` c) -> true | _ -> false) duplicateChannels
                let duplicateFM = List.filter (function (``FM Channel`` c) -> true | _ -> false) duplicateChannels

                if not (List.isEmpty duplicateAM) then
                    failwith << sprintf "Repeated AM channel: %s" << prettyPrintList
                             << List.map (fun (``AM Channel`` c) -> ``AM Path String`` c)
                             <| duplicateAM 

                if not (List.isEmpty duplicateFM) then
                    failwith << sprintf "Repeated FM channel: %s" << prettyPrintList
                             << List.map (fun (``FM Channel`` c) -> ``FM Path String`` c)
                             <| duplicateFM
             
            if not duplicateSources.IsEmpty then
                failwith << sprintf "Modulation sources used more than once: %s" << prettyPrintList
                         << List.map (sourceProvider >> sourceString) <| duplicateSources

            succeed settings
