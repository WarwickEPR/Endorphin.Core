namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils

module Source =
    module internal Translate =
        let sourceString =
            function
            | ExternalPort EXT1 -> "EXT1"
            | ExternalPort EXT2 -> "EXT2"
            | InternalGenerator Function1 -> "FUNCTION1"

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

        let private sourceKey key prefix src = sprintf "%s:%s%s" prefix src key

        module Function =
            let private functionKey key prefix fg = sourceKey key prefix (sourceString fg)

            let private shapeKey = ":SHAPE"
            let private rampKey =  ":SHAPE:RAMP"
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

            let internal setShape prefix fg rfSource (shape : FunctionShape) = 
                let key = functionKey shapeKey prefix fg
                let rkey = functionKey rampKey prefix fg 
                asyncChoice {
                    do! IO.setValue functionShapeString key rfSource shape
                    match shape with
                    | Ramp polarity -> do! IO.setPolarity rkey rfSource polarity
                    | _ -> () }

            let private frequencyKey = ":FREQUENCY"
            let internal setFrequency prefix fg = IO.setFrequency (functionKey frequencyKey prefix fg)
            let internal queryFrequency prefix fg = IO.queryFrequency (functionKey frequencyKey prefix fg)

        module External =
            let private externalKey key prefix src = sourceKey key prefix (sourceString src)

            let private couplingKey = ":COUPLING"
            let internal setCoupling prefix src = IO.setValue couplingString (externalKey couplingKey prefix src)
            let internal queryCoupling prefix src = IO.queryValue parseCoupling (externalKey couplingKey prefix src)

            let private impedanceKey = ":IMPEDANCE"
            let internal setImpedance prefix src = IO.setValue impedanceString (externalKey impedanceKey prefix src)
            let internal queryImpedance prefix src = IO.queryValue parseImpedance (externalKey impedanceKey prefix src)

    module Apply =
        open Control

        let internal setup prefix source rfSource = asyncChoice {
            let sourceProvider = sourceProvider source
            match source with
            | ExternalSource (port,settings) ->
                do! External.setCoupling prefix sourceProvider rfSource settings.Coupling
                do! External.setImpedance prefix sourceProvider rfSource settings.Impedance
                
            | InternalSource (generator,settings) ->
                let functionGenerator = Translate.sourceString sourceProvider
                do! Function.setShape prefix sourceProvider rfSource settings.Shape
                do! Function.setFrequency prefix sourceProvider rfSource settings.Frequency
        }