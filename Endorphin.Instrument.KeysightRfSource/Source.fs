namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils

module Source =
    module internal Translate =
        /// Convert the internal representation of a trigger source to a machine representation.
        let sourceString = function
            | ExternalPort EXT1 -> "EXT1"
            | ExternalPort EXT2 -> "EXT2"
            | InternalGenerator Function1 -> "FUNCTION1"

        /// Convert the machine representation of a trigger source into an internal representation.
        let parseSource str =
            match String.toUpper str with
            | "EXT1" -> ExternalPort EXT1
            | "EXT2" -> ExternalPort EXT2
            | "FUNCTION1" -> InternalGenerator Function1
            | str -> failwithf "Unexpected source: %s" str

    module Control =
        open Translate

        // Keys are e.g. :AM1:INTERNAL:FUNCTION1:SHAPE
        //           or  :FM2:EXT1:COUPLING
        //           or  :FUNCTION1:SHAPE
        // Treated as :AM1:INTERNAL + :FUNCTION1 + :SHAPE
        //         or :FM2 + :EXT1 + :COUPLING
        //         or "" + "FUNCTION1 + :SHAPE
        // Sources can be used directly (low frequency oscillator) or by modulations

        /// Concatenate the key, command subsystem prefix and source into a machine-readable
        /// command string.
        let private sourceKey key prefix src = sprintf "%s:%s%s" prefix src key

        module Function =
            /// Create a key for the given function, with the correct subsystem and source.
            let private functionKey key prefix fg = sourceKey key prefix (sourceString fg)

            /// Key to create a function shape.
            let private shapeKey = ":SHAPE"
            /// Key to create a function in the ramp shape.
            let private rampKey =  ":SHAPE:RAMP"
            /// Query the shape of the machine, given the correct source and subsystem.
            let internal queryShape prefix fg rfSource str =
                let getRamp = asyncChoice { let rkey = functionKey rampKey prefix fg
                                            let! polarity = IO.queryPolarity rkey rfSource
                                            return Ramp polarity }
                let key = functionKey shapeKey prefix fg
                asyncChoice {
                    let! shape = IO.queryValue String.toUpper key rfSource
                    match shape with
                    | "SINE"             -> return Sine
                    | "TRI" | "TRIANGLE" -> return Triangle
                    | "SQU" | "SQUARE"   -> return Square
                    | "RAMP"             -> return! getRamp
                    | _                  -> return failwithf "Unexpected function shape type string: %s" str }

            /// Set the shape of the function generator.
            let internal setShape prefix fg rfSource (shape : FunctionShape) = 
                let key = functionKey shapeKey prefix fg
                let rkey = functionKey rampKey prefix fg 
                asyncChoice {
                    do! IO.setValue functionShapeString key rfSource shape
                    match shape with
                    | Ramp polarity -> do! IO.setPolarity rkey rfSource polarity
                    | _ -> () }

            /// Key for setting frequencies of the function generator.
            let private frequencyKey = ":FREQUENCY"
            /// Set the frequency of the function generator.
            let internal setFrequency prefix fg = IO.setFrequency (functionKey frequencyKey prefix fg)
            /// Query the frequency of the function generator.
            let internal queryFrequency prefix fg = IO.queryFrequency (functionKey frequencyKey prefix fg)

        module External =
            /// Key needed for operations on external sources.
            let private externalKey key prefix src = sourceKey key prefix (sourceString src)

            /// Key for the coupling system.
            let private couplingKey = ":COUPLING"
            /// Set the coupling of an external source.
            let internal setCoupling prefix src = IO.setValue couplingString (externalKey couplingKey prefix src)
            /// Query the coupling of an external source.
            let internal queryCoupling prefix src = IO.queryValue parseCoupling (externalKey couplingKey prefix src)

            /// Key for the impedance system.
            let private impedanceKey = ":IMPEDANCE"
            /// Set the impedance of an external source.
            let internal setImpedance prefix src = IO.setValue impedanceString (externalKey impedanceKey prefix src)
            /// Query the impedance of an external source.
            let internal queryImpedance prefix src = IO.queryValue parseImpedance (externalKey impedanceKey prefix src)

    module Apply =
        open Control

        /// Apply the given source configurations to the machine.
        let internal setup prefix source rfSource = asyncChoice {
            let sourceProvider = sourceProvider source
            match source with
            | ExternalSource (_, settings) ->
                do! External.setCoupling prefix sourceProvider rfSource settings.Coupling
                do! External.setImpedance prefix sourceProvider rfSource settings.Impedance
            | InternalSource (_, settings) ->
                do! Function.setShape prefix sourceProvider rfSource settings.Shape
                do! Function.setFrequency prefix sourceProvider rfSource settings.Frequency }