namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Source

module Modulation =
    module internal Translate =
        open Endorphin.Core.StringUtils

        let ``AM Path String`` =
            function
            | AM1 -> "AM1"
            | AM2 -> "AM2"

        let ``FM Path String`` =
            function
            | FM1 -> "FM1"
            | FM2 -> "FM2"

        let modulationChannelString =
            function
            | ``AM Channel`` path -> ``AM Path String`` path
            | ``FM Channel`` path -> ``FM Path String`` path


        // Modulation Settings 
        type DepthType = LinearType | ExponentialType

        let parseDepthType str =
            match upperCase str with
            | "LIN" | "LINEAR" -> LinearType
            | "EXP" | "EXPONENTIAL" -> ExponentialType
            | str -> failwithf "Unexpected depth type: %s" str

        let depthTypeString =
            function
            | Linear _ -> "LIN"
            | Exponential _ -> "EXP"

    module Control =
        open Translate
        open Source.Translate
        open Source.Control
        
        let private stateKey = ":STATE"
        let private sourceKey = ":SOURCE"
        let private setSource = IO.setValue sourceString 
        let private querySource = IO.queryValue parseSource

        module Amplitude =
            let private prefix path key = sprintf ":%s%s" (``AM Path String`` path) key

            let setState path = IO.setOnOffState (prefix path stateKey)
            let queryState path = IO.queryOnOffState (prefix path stateKey)
        
            let setSource path = setSource (prefix path sourceKey)
            let querySource path = querySource (prefix path sourceKey)

            let private typeKey path = prefix path ":TYPE"
            let internal setType path = IO.setValue depthTypeString (typeKey path)
            let internal queryType path = IO.queryValue parseDepthType (typeKey path)

            let private depthLinearKey path = prefix path ":DEPTH"
            let private setDepthLinear path = IO.setPercentage (depthLinearKey path)
            let private queryDepthLinear path = IO.queryPercentage (depthLinearKey path)

            let private depthExponentialKey path = prefix path ":DEPTH:EXPONENTIAL"
            let private setDepthExponential path = IO.setDecibelRatio (depthExponentialKey path)
            let private queryDepthExponential path = IO.queryDecibelRatio (depthExponentialKey path)

            let setDepth path rfSource depth =
                match depth with
                | Linear v -> setDepthLinear path rfSource v
                | Exponential v -> setDepthExponential path rfSource v

            module External =
                let private prefix path = prefix path ":EXTERNAL"

                let setCoupling path = External.setCoupling (prefix path)
                let queryCoupling path = External.queryCoupling (prefix path)

                let setImpedance path = External.setImpedance (prefix path)
                let queryImpedance path = External.queryImpedance (prefix path)

            module Internal =
                let private prefix path = prefix path ":INTERNAL"

                let setFunctionShape path = Function.setShape (prefix path)
                let queryFunctionShape path = Function.queryShape (prefix path)

                let setFunctionFrequency path = Function.setFrequency (prefix path)
                let queryFunctionFrequency path = Function.queryFrequency (prefix path)

        module Frequency =
            let private prefix path key = sprintf ":%s:%s" (``FM Path String`` path) key

            let setState path = IO.setOnOffState (prefix path stateKey)
            let queryState path = IO.queryOnOffState (prefix path stateKey)

            let setSource path = setSource (prefix path sourceKey)
            let querySource path = querySource (prefix path sourceKey)

            let private deviationKey path = prefix path ":DEVIATION"
            let setDeviation path = IO.setFrequency (deviationKey path)
            let queryDeviation path = IO.queryFrequency (deviationKey path)

            module External =
                let private prefix path = prefix path ":EXTERNAL"

                let setCoupling path = External.setCoupling (prefix path)
                let queryCoupling path = External.queryCoupling (prefix path)

                let setImpedance path = External.setImpedance (prefix path)
                let queryImpedance path = External.queryImpedance (prefix path)

            module Internal =
                let private prefix path = prefix path ":INTERNAL"

                let setFunctionShape path = Function.setShape (prefix path)
                let queryFunctionShape path = Function.queryShape (prefix path)

                let setFunctionFrequency path = Function.setFrequency (prefix path)
                let queryFunctionFrequency path = Function.queryFrequency (prefix path)

    module Runtime =
        let private modulationStateKey = ":OUTPUT:MODULATION"
        let setModulationState = IO.setOnOffState modulationStateKey
        let queryModulationState = IO.queryOnOffState modulationStateKey

    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
        open Translate

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

    
    module Apply =
        open Control
        open Translate
        open Endorphin.Core.CollectionUtils

        let consistentModulationSettings settings =
            let duplicateChannels = settings |> List.map modulationChannel |> duplicates
            let duplicateSources  = settings |> List.map modulationSource |> duplicates
            // When PM is added, check that PM and FM paths are exclusive

            if not duplicateChannels.IsEmpty then
                failwith << sprintf "Repeated modulation channels: %s"
                         << prettyPrintList
                         << List.map (fun channel -> modulationChannelString channel)
                         <| duplicateChannels
             
            if not duplicateSources.IsEmpty then
                failwith << sprintf "Modulation sources used more than once: %s" << prettyPrintList
                         << List.map (sourceProvider >> sourceString) <| duplicateSources

            succeed settings

        let private applyModulation rfSource modulation = asyncChoice {
            match modulation with
            | AmplitudeModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| ``AM Path String`` path
                do! Amplitude.setDepth path rfSource settings.Depth
                do! Source.Apply.setup prefix source rfSource
                do! Amplitude.setSource path rfSource (sourceProvider source)

            | FrequencyModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| ``FM Path String`` path
                do! Frequency.setDeviation path rfSource settings.Deviation
                do! Frequency.setSource path rfSource (sourceProvider source)
                do! Source.Apply.setup prefix source rfSource
                do! Frequency.setSource path rfSource (sourceProvider source)
        }


        let modulationSettings rfSource settings = choice {
            let! consistentSettings = consistentModulationSettings settings
            for modulation in consistentSettings do
                applyModulation rfSource modulation |> ignore }