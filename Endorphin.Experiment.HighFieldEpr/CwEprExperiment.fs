namespace Endorphin.Experiment.HighFieldEpr

open System

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Instrument.TwickenhamSmc
open Endorphin.Instrument.PicoScope5000

module CwEprExperiment =
    
    [<AutoOpen>]
    module Model =

        type FieldSweepDirection = Increasing | Decreasing

        /// Defines the parameters for a CW EPR experiment.
        type ExperimentParameters =
            internal { CentreField         : decimal<T>
                       SweepWidth          : decimal<T>
                       FieldSweepDirection : FieldSweepDirection
                       RampRate            : decimal<T/s>
                       ConversionTime      : int<ms>
                       QuadratureDetection : bool
                       NumberOfScans       : int }

        type Sample =
            { MagneticFieldShuntAdc : int16
              ReSignalAdc           : int16
              ImSignalAdc           : int16 }

    module Parameters =
        
        let centreField parameters = parameters.CentreField
        
        let sweepWidth parameters = parameters.SweepWidth

        let fieldSweepDirection parameters = parameters.FieldSweepDirection
        
        let initialField parameters =
            if fieldSweepDirection parameters = Increasing
            then parameters.CentreField - parameters.SweepWidth / 2.0M
            else parameters.CentreField + parameters.SweepWidth / 2.0M 

        let finalField parameters =
            if fieldSweepDirection parameters = Increasing
            then parameters.CentreField + parameters.SweepWidth / 2.0M
            else parameters.CentreField - parameters.SweepWidth / 2.0M

        let rampRate parameters = parameters.RampRate

        let numberOfScans parameters = parameters.NumberOfScans

        let conversionTime parameters = parameters.ConversionTime

        let quadratureDetection parameters = parameters.QuadratureDetection

        let duration parameters =
            (decimal <| numberOfScans parameters) * (sweepWidth parameters / rampRate parameters)

        module Validate =

            let private liftValid (parameters : ExperimentParameters) = (parameters, ([] : string list)) 
            let private withError (error : string) (parameters : ExperimentParameters) = (parameters, [error])

            let private applyValidator (parameters : ExperimentParameters, errors : string list) validator =
                let (parameters' : ExperimentParameters, errors') = validator parameters
                (parameters', List.concat [errors ; errors'])

            let private applyValidators parameters = List.fold applyValidator (parameters, [])

            let private clipField magnetController parameters =
                let clip =
                    min (MagnetController.Output.MagneticField.upperLimit magnetController)
                    >> max (MagnetController.Output.MagneticField.lowerLimit magnetController)
                
                let initialField' = clip <| initialField parameters
                let finalField'   = clip <| finalField parameters

                { parameters with
                    CentreField = (initialField' + finalField') / 2.0M
                    SweepWidth  = abs (finalField' - initialField') }
                |> liftValid

            let private digitiseField magnetController parameters =
                let digitise = MagnetController.Output.MagneticField.digitise magnetController
                let initialField' = digitise <| initialField parameters
                let finalField'   = digitise <| finalField parameters

                { parameters with
                    CentreField = (initialField' + finalField') / 2.0M
                    SweepWidth  = abs (finalField' - initialField') }
                |> liftValid

            let private sweepWidthGreaterThanZero parameters =
                if sweepWidth parameters <= 0.0M<T>
                then parameters |> withError "Magnetic field sweep width must be greater than zero."
                else parameters |> liftValid

            let magneticField magnetController parameters =
                [ clipField magnetController
                  digitiseField magnetController
                  sweepWidthGreaterThanZero ]
                |> applyValidators parameters

            let private clipRampRate magnetController parameters =
                let rampRate' = 
                    rampRate parameters 
                    |> min ((MagnetController.Output.MagneticField.linearCoefficient magnetController) 
                                * (MagnetController.Ramp.Rate.maximum magnetController))

                { parameters with RampRate = rampRate' } |> liftValid

            let private nearestCalibratedRampRate magnetController parameters =
                let rampRate' =
                    (rampRate parameters) / (MagnetController.Output.MagneticField.linearCoefficient magnetController)
                    |> MagnetController.Ramp.Rate.nearest magnetController
                    |> (*) (MagnetController.Output.MagneticField.linearCoefficient magnetController)

                { parameters with RampRate = rampRate' } |> liftValid

            let rampRate magnetController parameters =
                [ clipRampRate magnetController
                  nearestCalibratedRampRate magnetController ]
                |> applyValidators parameters

            let private conversionTimeIsMultipleOfMains parameters =
                let conversionTime' = max 20<ms> (((conversionTime parameters) / 20) * 20)
                { parameters with ConversionTime = conversionTime' } |> liftValid

            let conversionTime = conversionTimeIsMultipleOfMains

            let private numberOfScansGreaterThanZero parameters =
                if numberOfScans parameters <= 0
                then { parameters with NumberOfScans = 1 } |> liftValid
                else parameters |> liftValid

            let numberOfScans = numberOfScansGreaterThanZero

            let experimentParameters magnetController parameters =
                [ magneticField magnetController
                  rampRate magnetController
                  conversionTime
                  numberOfScans ]
                |> applyValidators parameters

            let failIfInvalid magnetController parameters =
                let (experimentParameters', errors) = experimentParameters magnetController parameters
            
                if not <| List.isEmpty errors then
                    errors
                    |> List.map (sprintf "- %s")
                    |> String.concat "\n"
                    |> failwithf "Cannot create instrument parameters for invalid experiment parameters:\n%s"

    module internal InstrumentParameters =

        let rec initialFieldIndex magnetController parameters = 
            let (currentDirection, stepIndex) =
                Parameters.initialField parameters
                |> MagnetController.Output.MagneticField.toStepIndex magnetController

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| finalFieldIndex magnetController parameters, stepIndex)

        and finalFieldIndex magnetController parameters =
            let (currentDirection, stepIndex) =
                Parameters.finalField parameters
                |> MagnetController.Output.MagneticField.toStepIndex magnetController

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| initialFieldIndex magnetController parameters, stepIndex)
                 

        let rampRateIndex magnetController parameters =
            Parameters.rampRate parameters
            / (MagnetController.Output.MagneticField.linearCoefficient magnetController)
            |> MagnetController.Ramp.Rate.nearestIndex magnetController

        let sampleInterval = Interval.fromMicroseconds 20<us>

        let downsamplingRatio parameters = uint32 ((Parameters.conversionTime parameters) / 20<ms>) * 1000u

        let shuntVoltageRange magnetController parameters =
            let (initialCurrentDirection, initialStepIndex) = initialFieldIndex magnetController parameters
            let (finalCurrentDirection,   finalStepIndex  ) = finalFieldIndex magnetController parameters
            
            let initialShuntVoltage = MagnetController.Output.ShuntVoltage.fromStepIndex magnetController initialStepIndex
            let finalShuntVoltage   = MagnetController.Output.ShuntVoltage.fromStepIndex magnetController finalStepIndex
            let shuntOffset         = MagnetController.Output.ShuntVoltage.offset magnetController

            let minimumShuntVoltage =
                if initialCurrentDirection = finalCurrentDirection
                then min initialShuntVoltage finalShuntVoltage
                else min initialShuntVoltage finalShuntVoltage |> min shuntOffset
                
            let maximumShuntVoltage =
                if initialCurrentDirection = finalCurrentDirection
                then max initialShuntVoltage finalShuntVoltage
                else max initialShuntVoltage finalShuntVoltage |> max shuntOffset

            (minimumShuntVoltage, maximumShuntVoltage)

        let magneticFieldChannelOffset magnetController parameters =
            let (minimumVoltage, maximumVoltage) = shuntVoltageRange magnetController parameters
            1.0f<V> * (float32 <| Decimal.Round((minimumVoltage + maximumVoltage) / 2.0M<V>, 1))
            
        let magneticFieldChannelRange magnetController parameters = 
            let offset = magneticFieldChannelOffset magnetController parameters 
            
            let minimumForOffset = 
                offset 
                |> Range.minimumForOffsetVoltage
                |> function
                    | Some range' -> range'
                    | None        -> failwithf "Required shunt voltage offset (%f V) exceeds maximum." offset

            let (minimumVoltage, maximumVoltage) = shuntVoltageRange magnetController parameters
            let minimumForRange =
                abs (offset - 1.0f<V> * (float32 (minimumVoltage / 1.0M<V>)))
                |> max (abs (offset - 1.0f<V> * (float32 (maximumVoltage / 1.0M<V>))))
                |> Range.minimumForVoltage

            [ minimumForOffset ; minimumForRange ] |> List.maxBy Range.voltage


        let streamingParameters magnetController parameters =
            let magneticFieldChannelOffset' = magneticFieldChannelOffset magnetController parameters
            let magneticFieldChannelRange'  = magneticFieldChannelRange magnetController parameters

            Streaming.Parameters.create Resolution_14bit sampleInterval (64u * 1024u)
            |> Streaming.Parameters.withDownsamplingRatio (downsamplingRatio parameters)
            |> Streaming.Parameters.enableChannel ChannelA DC magneticFieldChannelRange' magneticFieldChannelOffset' Bandwidth_20MHz
    