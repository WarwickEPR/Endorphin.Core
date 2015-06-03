namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Translate

module Source =
    let private sourceKey key prefix src = sprintf "%s:%s:%s" prefix src key

    module Function =
        let private functionKey key prefix fg = sourceKey key prefix (sourceString fg)

        let private shapeKey = ":SHAPE"   
        let internal queryShape prefix fg rfSource str =
            let getRamp = asyncChoice { let rkey = functionKey ":SHAPE:RAMP" prefix fg
                                        let! polarity = IO.queryPolarity rkey rfSource
                                        return Ramp polarity }
            let key = functionKey shapeKey prefix fg
            asyncChoice {
                let! shape = IO.queryValue upperCase key rfSource
                match shape with
                | "SINE"             -> return Sine
                | "TRI" | "TRIANGLE" -> return Triangle
                | "SQU" | "SQUARE"   -> return Square
                | "RAMP"             -> return! getRamp
                | _                  -> return failwithf "Unexpected function shape type string: %s" str }

        let internal setShape prefix fg rfSource (shape : FunctionShape) = 
            let key = functionKey shapeKey prefix fg
            asyncChoice {
                do! IO.setValue functionShapeString key rfSource shape
                match shape with
                | Ramp polarity -> do! IO.setPolarity key rfSource polarity
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
