namespace Endorphin.Instrument.Keysight

open Endorphin.Core
open Source

module Modulation =
    module internal Translate =
        /// Convert the internal representation of the AM path string into the machine
        /// representation.
        let amPathString = function
            | AM1 -> "AM1"
            | AM2 -> "AM2"

        /// Convert the internal representation of the FM path string into the machine
        /// representation.
        let fmPathString = function
            | FM1 -> "FM1"
            | FM2 -> "FM2"

        /// Convert the internal representation of the modulation channel into the machine
        /// representation.
        let modulationChannelString = function
            | AmChannel path -> amPathString path
            | FmChannel path -> fmPathString path

        /// The type of modulation depth - linear or exponential.
        type DepthType = LinearType | ExponentialType

        /// Convert the machine representation of the depth type into the internal
        /// representation.
        let parseDepthType str =
            match String.toUpper str with
            | "LIN" | "LINEAR" -> LinearType
            | "EXP" | "EXPONENTIAL" -> ExponentialType
            | str -> raise << UnexpectedReplyException <| sprintf "Unexpected depth type: %s" str

        /// Convert the internal representation of the depth type into the machine
        /// representation.
        let depthTypeString = function
            | Linear _ -> "LIN"
            | Exponential _ -> "EXP"

        /// Get the extra infix part of the key needed for the modulation source.
        let modulationSourceInfix = function
            | ExternalSource _ -> ""
            | InternalSource _ -> ":INTERNAL"

    module Control =
        open Translate
        open Source.Translate
        
        /// Key needed to set the state of the modulation.
        let private stateKey = ":STATE"
        /// Key needed to set the source of the modulation.
        let private sourceKey = ":SOURCE"
        /// Set the soruce of the modulation.
        let private setSource = IO.setValueString sourceString 
        /// Query the source of the modulation and parse the result.
        let private querySource = IO.queryKeyString parseSource

        module Amplitude =
            /// Generate the prefix needed for a certain key based on the subsystem path.
            let private prefix path key = sprintf ":%s%s" (amPathString path) key

            /// Set the state of the amplitude modulation of the given path.
            let setState path = IO.setOnOffState (prefix path stateKey)
            /// Query the state of the amplitude modulation of the given path.
            let queryState path = IO.queryOnOffState (prefix path stateKey)
        
            /// Set the source of the amplitude modulation of the given path.
            let setSource path = setSource (prefix path sourceKey)
            /// Query the source of the amplitude modulation of the given path.
            let querySource path = querySource (prefix path sourceKey)

            /// Key needed to set the type of the amplitude modulation.
            let private typeKey path = prefix path ":TYPE"
            /// Set the type of the ampltiude modulation of the given path.
            let internal setType path = IO.setValueString depthTypeString (typeKey path)
            /// Query the type of the amplitude modulation of the given path.
            let internal queryType path = IO.queryKeyString parseDepthType (typeKey path)

            /// Key needed for operations on linear depth.
            let private depthLinearKey path = prefix path ":DEPTH"
            /// Set the depth of the given path to be a percentage.
            let private setDepthLinear path = IO.setPercentage (depthLinearKey path)
            /// Query the depth of the given path, and parse the result as a percentage.
            let private queryDepthLinear path = IO.queryPercentage (depthLinearKey path)

            /// Key needed for operations on exponential depth.
            let private depthExponentialKey path = prefix path ":DEPTH:EXPONENTIAL"
            /// Set the depth of the given path to as a decibel ratio.
            let private setDepthExponential path = IO.setDecibelRatio (depthExponentialKey path)
            /// Query the depth of the given path, and parse the result as a decibel ratio.
            let private queryDepthExponential path = IO.queryDecibelRatio (depthExponentialKey path)

            /// Set the depth of the given path to the given value.
            let setDepth path rfSource = function
                | Linear v -> setDepthLinear path rfSource v
                | Exponential v -> setDepthExponential path rfSource v

        module Frequency =
            /// Create the prefix necessary for to apply the key to the correct path.
            let private prefix path key = sprintf ":%s%s" (fmPathString path) key

            /// Set the state of the frequency modulation of the given path.
            let setState path = IO.setOnOffState (prefix path stateKey)
            /// Query the state of the frequency modulation of the given path.
            let queryState path = IO.queryOnOffState (prefix path stateKey)

            /// Set the source of the frequency modulation of the given path.
            let setSource path = setSource (prefix path sourceKey)
            /// Query the source of the frequency modulation of the given path.
            let querySource path = querySource (prefix path sourceKey)

            /// Key needed for operations on frequency deviation.
            let private deviationKey path = prefix path ":DEVIATION"
            /// Set the deviation of the frequency modulation of the given path.
            let setDeviation path = IO.setFrequency (deviationKey path)
            /// Query the deviation of the frequency modulation of the given path.
            let queryDeviation path = IO.queryFrequency (deviationKey path)

    module Runtime =
        /// Key needed to set the output modulation state of the machine.
        let private modulationStateKey = ":OUTPUT:MODULATION"
        /// Set the output modulation state of the machine.
        let setModulationState = IO.setOnOffState modulationStateKey
        /// Query the output modulation state of the machine.
        let queryModulationState = IO.queryOnOffState modulationStateKey

    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
        open Translate

        /// A set of default function settings, which can be changed through the constructor
        /// functions.
        let defaultFunctionSettings = {
            Shape = Sine
            Frequency = Frequency_Hz 1.0e3<Hz>
            PhaseOffset = Phase_rad 0.0<rad> }
        /// Change the shape of the given settings to match the new value.
        let withShape shape (settings : FunctionSettings) = { settings with Shape = shape }
        /// Change the frequency (in Hz) of the given settings to match the new value.
        let withFrequencyInHz frequency (settings : FunctionSettings) =
            { settings with Frequency = Frequency_Hz frequency }
        /// Change the phase offset (in radians) of the given settings to match the
        /// new value.
        let withPhaseOffsetInRad phase (settings : FunctionSettings) =
            { settings with PhaseOffset = Phase_rad phase }

        /// Create a new sine-shaped function generator source with the given frequency in Hz.
        let internalSineSourceInHz frequency =
            InternalSource (Function1, defaultFunctionSettings |> withFrequencyInHz frequency)

        /// Create a new function generator source with the given frequence (in Hz), shape, and
        /// phase (in radians).
        let internalGeneralSourceInHz frequency shape phase =
            let settings = defaultFunctionSettings
                           |> withShape shape
                           |> withFrequencyInHz frequency
                           |> withPhaseOffsetInRad phase
            InternalSource (Function1, settings)
    
    module Apply =
        open Control
        open Translate
        open Endorphin.Core

        /// Verify that the given modulation settings are valid, producing a choice failure if
        /// they are not.
        let private verifyModulationSettings settings = choice {
            let duplicateChannels = settings |> List.map modulationChannel |> List.duplicates
            let duplicateSources  = settings |> List.map modulationSource |> List.duplicates
            // TODO: when PM is added, check that PM and FM paths are exclusive
            if not duplicateChannels.IsEmpty then
                do! Choice.fail << System.ArgumentException
                                << sprintf "Repeated modulation channels: %s"
                                << List.prettyPrint
                                << List.map (fun channel -> modulationChannelString channel)
                                <| duplicateChannels
            if not duplicateSources.IsEmpty then
                do! Choice.fail << System.ArgumentException
                                << sprintf "Modulation sources used more than once: %s"
                                << List.prettyPrint
                                << List.map (sourceProvider >> sourceString)
                                <| duplicateSources
            return! Choice.succeed () }

        /// Apply a given modulation to the machine.
        let private applyModulation rfSource modulation = async {
            printfn "applyModulation: %A" modulation
            match modulation with
            | AmplitudeModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| amPathString path
                let sourcePrefix = prefix + modulationSourceInfix source
                do! Amplitude.setDepth path rfSource settings.Depth
                do! Source.Apply.setup sourcePrefix source rfSource
                do! Amplitude.setSource path rfSource (sourceProvider source)
            | FrequencyModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| fmPathString path
                let sourcePrefix = prefix + modulationSourceInfix source
                do! Frequency.setDeviation path rfSource settings.Deviation
                do! Source.Apply.setup sourcePrefix source rfSource
                do! Frequency.setSource path rfSource (sourceProvider source) }

        /// Apply a list of modulation settings to the machine in order, after first
        /// verifying them.
        let modulationSettings rfSource settings = async {
            Choice.bindOrRaise <| verifyModulationSettings settings
            for modulation in settings do
                printfn "About to apply modulation %A" modulation
                do! applyModulation rfSource modulation }