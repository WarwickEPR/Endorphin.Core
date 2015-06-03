namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Source

module Modulation =
    module Translate =
        open Endorphin.Core.StringUtils

        let ``AM Path String`` =
            function
            | AM1 -> "AM1"
            | AM2 -> "AM2"

        let ``FM Path String`` =
            function
            | FM1 -> "FM1"
            | FM2 -> "FM2"

        // Modulation Settings 
        type internal DepthType = LinearType | ExponentialType

        let internal parseDepthType str =
            match upperCase str with
            | "LIN" | "LINEAR" -> LinearType
            | "EXP" | "EXPONENTIAL" -> ExponentialType
            | str -> failwithf "Unexpected depth type: %s" str

        let internal depthTypeString =
            function
            | Linear _ -> "LIN"
            | Exponential _ -> "EXP"

        let sourceString =
            function
            | ExternalPort EXT1 -> "EXT1"
            | ExternalPort EXT2 -> "EXT2"
            | InternalGenerator Function1 -> "FUNCTION1"

        let parseSource str =
            match upperCase str with
            | "EXT1" -> ExternalPort EXT1
            | "EXT2" -> ExternalPort EXT2
            | "FUNCTION1" -> InternalGenerator Function1
            | str -> failwithf "Unexpected source: %s" str

    module Act =
        open Translate
        
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
            let setDepthLinear path = IO.setPercentage (depthLinearKey path)
            let queryDepthLinear path = IO.queryPercentage (depthLinearKey path)

            let private depthExponentialKey path = prefix path ":DEPTH:EXPONENTIAL"
            let setDepthExponential path = IO.setDecibelRatio (depthExponentialKey path)
            let queryDepthExponential path = IO.queryDecibelRatio (depthExponentialKey path)

            module External =
                let private prefix path = prefix path ":EXTERNAL"

                let setCoupling path = Source.External.setCoupling (prefix path)
                let queryCoupling path = Source.External.queryCoupling (prefix path)

                let setImpedance path = Source.External.setImpedance (prefix path)
                let queryImpedance path = Source.External.queryImpedance (prefix path)

            module Internal =
                let private prefix path = prefix path ":INTERNAL"

                let setFunctionShape path = Source.Function.setShape (prefix path)
                let queryFunctionShape path = Source.Function.queryShape (prefix path)

                let setFunctionFrequency path = Source.Function.setFrequency (prefix path)
                let queryFunctionFrequency path = Source.Function.queryFrequency (prefix path)

        module Frequency =
            let private prefix path key = sprintf ":%s:%s" (``FM Path String`` path) key

            let setState path = IO.setOnOffState (prefix path stateKey)
            let queryState path = IO.queryOnOffState (prefix path stateKey)

            let setSource path = setSource (prefix path sourceKey)
            let querySource path = querySource (prefix path sourceKey)

            let private deviationKey path = prefix path ":DEVIATION"
            let setDepthLinear path = IO.setFrequency (deviationKey path)
            let queryDepthLinear path = IO.queryFrequency (deviationKey path)

            module External =
                let private prefix path = prefix path ":EXTERNAL"

                let setCoupling path = Source.External.setCoupling (prefix path)
                let queryCoupling path = Source.External.queryCoupling (prefix path)

                let setImpedance path = Source.External.setImpedance (prefix path)
                let queryImpedance path = Source.External.queryImpedance (prefix path)

            module Internal =
                let private prefix path = prefix path ":INTERNAL"

                let setFunctionShape path = Source.Function.setShape (prefix path)
                let queryFunctionShape path = Source.Function.queryShape (prefix path)

                let setFunctionFrequency path = Source.Function.setFrequency (prefix path)
                let queryFunctionFrequency path = Source.Function.queryFrequency (prefix path)

    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
        open Endorphin.Core.CollectionUtils
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
