// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Experiment.HighFieldEpr

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open MPFitLib

open FSharp.ViewModule.Validation
open FSharp.Control.Reactive

open Endorphin.Core
open Endorphin.Instrument.TwickenhamSmc
open Endorphin.Instrument.TwickenhamSmc.FieldSweep
open Endorphin.Instrument.PicoScope5000

/// Types and functions related to performing a CW EPR experiment.
module CwEprExperiment =
    
    [<AutoOpen>]
    module Model =
        
        /// Indicates whether the field sweep is performed in the direction of increasing or decreasing field.
        type FieldSweepDirection = Increasing | Decreasing

        /// Indicates whether one or two two channels are sampled during the acquisition.
        type Detection = Quadrature | SinglePhase

        /// Notes about the experimental and sample details.
        type Notes = { SampleNotes : string ; ExperimentNotes : string }

        /// Parameters for a CW EPR experiment.
        type CwEprExperimentParameters =
            { CentreField              : decimal<T>
              SweepWidth               : decimal<T>
              FieldSweepDirection      : FieldSweepDirection
              RampRate                 : decimal<T/s>
              ConversionTime           : int<ms>
              Detection                : Detection
              NumberOfScans            : int
              Notes                    : Notes
              Date                     : DateTime
              MagnetControllerSettings : TwickenhamSmc.Settings }
        
        /// Parameters for a part of a CW EPR scan.
        type ScanPartParameters =
            { InitialField     : decimal<T>
              FinalField       : decimal<T>
              RampRate         : decimal<T/s>
              CurrentDirection : CurrentDirection
              ConversionTime   : int<ms> }

        /// Contains the scan parts for a CW EPR experiment. Each scan can either contain one or two parts. If the
        /// magnetic field sweep range crosses the zero current magnetic field, then each scan has two parts.
        /// Otherwise, it is one part.
        type ExperimentScanParts =
            | OnePart of scan : ScanPartParameters
            | TwoPart of first : ScanPartParameters * second : ScanPartParameters

        /// A digitiser sample from a CW EPR experiment.
        type CwEprSample =
            { MagneticFieldShuntAdc : AdcCount
              ReSignalAdc           : AdcCount
              ImSignalAdc           : AdcCount }

        /// A point in a CW EPR signal.
        type CwEprSignalPoint =
            { MagneticField     : decimal<T>
              ReSignalIntensity : decimal
              ImSignalIntensity : decimal }

        /// Fitting parameters for the shunt voltage ramp as a function of time.
        type ShuntVoltageRamp =
            { InitialVoltage : float<V>
              FinalVoltage   : float<V>
              StartTime      : float<s>
              Duration       : float<s> }

        /// Raw samples for a scan part which is currently in progress.
        type ScanPartInProgress = int * ScanPartParameters * ResizeArray<CwEprSample>

        /// Raw data for the completed scans in a CW EPR experiment.
        type RawData = Map<int * ScanPartParameters, CwEprSample array>

        /// CW EPR signal accumulated so far during an experiment.
        type CwEprSignal = 
            { CompletedScans    : RawData
              AccumulatedSignal : CwEprSignalPoint array
              CurrentScan       : ScanPartInProgress option }

        /// An update to the CW EPR signal.
        type CwEprSignalUpdate =
            | SamplesAvailable  of samples : CwEprSample seq
            | ScanPartStarted   of scanNumber : int * scanPart : ScanPartParameters
            | ScanPartCompleted

        /// A message to the CW EPR signal processing agent.
        type CwEprSignalProcessorMessage =
            | SignalUpdate          of update : CwEprSignalUpdate
            | SignalSnapshotRequest of replyChannel : AsyncReplyChannel<CwEprSignalPoint array * (ScanPartInProgress option)>
            | RawDataRequest        of replyChannel : AsyncReplyChannel<RawData>

        /// CW EPR signal processing agent.
        type CwEprSignalProcessor = CwEprSignalProcessor of agent : Agent<CwEprSignalProcessorMessage> * parameters : CwEprExperimentParameters

        /// The status of a CW EPR experiment in progress.
        type CwEprExperimentStatus =
            | StartingExperiment
            | PerformingExperimentScan of scan : int
            | StoppedAfterScan of scan : int
            | FinishedExperiment
            | CancelledExperiment

        /// The result of a CW EPR experiment.
        type CwEprExperimentResult =
            | ExperimentCompleted
            | ExperimentError of exn
            | ExperimentCancelled

        /// A CW EPR experiment which has been created from the given parameters and ready to be run against the
        /// hardware.
        type CwEprExperiment = 
             { Parameters              : CwEprExperimentParameters
               MagnetController        : MagnetController
               PicoScope               : PicoScope5000
               StatusChanged           : NotificationEvent<CwEprExperimentStatus>
               SignalProcessor         : CwEprSignalProcessor
               StopAfterScanCapability : CancellationCapability }

    /// Functions related to CW EPR experiment parameters.
    module Parameters =
        
        /// Returns the centre field.
        let centreField parameters = parameters.CentreField
        
        /// Returns the sweep width.
        let sweepWidth parameters = parameters.SweepWidth

        /// Returns the field sweep direction.
        let fieldSweepDirection parameters = parameters.FieldSweepDirection
        
        /// Returns the initial field.
        let initialField parameters =
            if fieldSweepDirection parameters = Increasing
            then (centreField parameters) - (sweepWidth parameters) / 2.0M
            else (centreField parameters) + (sweepWidth parameters) / 2.0M 

        /// Returns the final field.
        let finalField parameters =
            if fieldSweepDirection parameters = Increasing
            then (centreField parameters) + (sweepWidth parameters) / 2.0M
            else (centreField parameters) - (sweepWidth parameters) / 2.0M

        /// Returns the ramp rate.
        let rampRate (parameters : CwEprExperimentParameters) = parameters.RampRate

        /// Returns the number of scans.
        let numberOfScans parameters = parameters.NumberOfScans

        /// Returns the conversion time.
        let conversionTime (parameters : CwEprExperimentParameters) = parameters.ConversionTime

        /// Returns the detection mode (single phase or quadrature).
        let detection parameters = parameters.Detection

        /// Returns the estimated scan duration for a single scan, not including magnet controller preparation.
        let scanDuration parameters = sweepWidth parameters / rampRate parameters

        /// Returns the estimated experiment duration, not including magnet controller preparation.
        let experimentDuration parameters = (decimal <| numberOfScans parameters) * (scanDuration parameters)
        
        /// Returns the experimental and sample notes.
        let notes parameters = parameters.Notes

        /// Returns the magnet controller settings.
        let magnetControllerSettings parameters =  parameters.MagnetControllerSettings

        /// Returns the number of digitiser samples per signal point.
        let samplesPerPoint parameters = conversionTime parameters / 20<ms>

        /// Returns the initial field current direction and magnet controller step index.
        let rec initialFieldIndex parameters = 
            let (currentDirection, stepIndex) =
                initialField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| finalFieldIndex parameters, stepIndex)

        /// Returns the final field current direction and magnet controller step index.
        and finalFieldIndex parameters =
            let (currentDirection, stepIndex) =
                finalField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| initialFieldIndex parameters, stepIndex)

        /// Clips the magnetic field within the available range according to the magnet controller settings.
        let private clipField parameters =
            let clip =
                min    (MagnetController.Settings.Limit.upperField <| magnetControllerSettings parameters)
                >> max (MagnetController.Settings.Limit.lowerField <| magnetControllerSettings parameters)
                
            let initialField' = clip <| initialField parameters
            let finalField'   = clip <| finalField parameters

            { parameters with
                CentreField = (initialField' + finalField') / 2.0M
                SweepWidth  = abs (finalField' - initialField') }

        /// Digitises the magnetic field to the available discrete magnet controller steps according to the magnet
        /// controller settings.
        let private digitiseField parameters =
            let digitise = MagnetController.Settings.Digitise.magneticField <| magnetControllerSettings parameters
            let initialField' = digitise <| initialField parameters
            let finalField'   = digitise <| finalField parameters
                            
            let initial = initialField parameters
            let final   = finalField parameters

            let initialField' = digitise initial
            let finalField'   = digitise final

            let centreField' = (initialField' + finalField') / 2.0M
            let sweepWidth'  = abs (finalField' - initialField')

            { parameters with
                CentreField = centreField'
                SweepWidth  = sweepWidth' }

        /// Clips the ramp rate within the ramp rate limit according to the magnet controller settings.
        let private clipRampRate parameters =
            let linearFieldCoeffiicent = abs (MagnetController.Settings.linearFieldCoefficient <| magnetControllerSettings parameters)
            let maximumRampRate = MagnetController.Settings.RampRate.maximum <| magnetControllerSettings parameters
            let rampRate' = min (rampRate parameters) (linearFieldCoeffiicent * maximumRampRate)

            { parameters with RampRate = rampRate' }

        /// Sets the ramp rate to the nearest available calibrated value according to the magnet controller settings.
        let private nearestCalibratedRampRate parameters =
            let linearFieldCoefficient = abs (MagnetController.Settings.linearFieldCoefficient <| magnetControllerSettings parameters)

            let rampRate' =
                (rampRate parameters) / linearFieldCoefficient
                |> (MagnetController.Settings.RampRate.nearest <| magnetControllerSettings parameters)
                |> (*) linearFieldCoefficient

            { parameters with RampRate = rampRate' }

        /// Sets the conversion time to be the nearest available multiple of the mains frequency.
        let private conversionTimeIsMultipleOfMains parameters =
            let conversionTime' = max 20<ms> (((conversionTime parameters) / 20) * 20)
            { parameters with ConversionTime = conversionTime' }

        /// Sets the number of scans to be at least one.
        let private numberOfScansGreaterThanZero parameters =
            if numberOfScans parameters <= 0
            then { parameters with NumberOfScans = 1 }
            else parameters

        /// Validates the sweep width to be non-zero.
        let private sweepWidthGreaterThanZero =
            (fun parameters ->
                if sweepWidth parameters <= 0.0M<T>
                then Some "Magnetic field sweep width must be greater than zero."
                else None)
            |> Validators.custom

        /// Validates the CW EPR experiment parameters.
        let validate = sweepWidthGreaterThanZero
        
        /// Returns updated CW EPR experiment parameters with the specified centre field. The field range is then
        /// clipped and digitised according to the magnet controller settings.
        let withCentreField centreField parameters =
            { parameters with CentreField = centreField }
            |> clipField
            |> digitiseField

        /// Returns updated CW EPR experiment parameters with the specified sweep width. The field range is then
        /// clipped and digitised according to the magnet controller settings.
        let withSweepWidth sweepWidth parameters =
            { parameters with SweepWidth = sweepWidth }
            |> clipField
            |> digitiseField
        
        /// Returns updated CW EPR experiment parameters with the specified field sweep direction.
        let withFieldSweepDirection direction parameters =
            { parameters with FieldSweepDirection = direction }

        /// Returns updated CW EPR experiment parameters with the specified ramp rate. The ramp rate is clipped
        /// to be within the limit and set to be the nearest available calibrated value according to the magnet
        /// controller settings.
        let withRampRate rampRate (parameters : CwEprExperimentParameters) =
            { parameters with RampRate = rampRate }
            |> clipRampRate
            |> nearestCalibratedRampRate

        /// Returns updated CW EPR experiment parameters with the specified conversion time, digitised to the nearest
        /// multiple of the mains frequency.
        let withConversionTime conversionTime (parameters : CwEprExperimentParameters) =
            { parameters with ConversionTime = conversionTime }
            |> conversionTimeIsMultipleOfMains
        
        /// Returns updated CW EPR experiment parameters with the specified detection mode (single phase or quadrature).
        let withDetection detection parameters =
            { parameters with Detection = detection }

        /// Returns updated CW EPR experiment parameters with the specified number of scans. The number of scans is set
        /// to be at least one.
        let withNumberOfScans numberOfScans parameters =
            { parameters with NumberOfScans = numberOfScans }
            |> numberOfScansGreaterThanZero

        /// Returns updated CW EPR experiment parameters with the specified experimental and sample notes.
        let withNotes notes parameters = { parameters with Notes = notes }

        /// Returns updated CW EPR experiment parameters with the specified date stamp.
        let withDate date parameters = { parameters with Date = date }
        
        /// Returns updated CW EPR experiment parameters with the given magnet controller settings. The magnetic field
        /// range and ramp rate are clipped and digitised according to the new magnet controller settings.
        let withMagnetControllerSettings settings parameters =
            { parameters with MagnetControllerSettings = settings }
            |> clipField
            |> digitiseField
            |> clipRampRate
            |> nearestCalibratedRampRate

        /// Creates CW EPR experiment parameters with the specified centre field, sweep width, ramp rate and magnet
        /// controller settings. By default, the sweep is in the direction of increasing field, the conversion time is
        /// 20 ms and quadrature detection is enabled. 
        let create centreField sweepWidth rampRate magnetControllerSettings =
            { CentreField              = centreField
              SweepWidth               = sweepWidth
              FieldSweepDirection      = Increasing
              RampRate                 = rampRate
              ConversionTime           = 20<ms>
              Detection                = Quadrature
              NumberOfScans            = 1
              Date                     = DateTime.Now
              Notes                    = { SampleNotes = "" ; ExperimentNotes = "" }
              MagnetControllerSettings = magnetControllerSettings }
            |> clipField
            |> digitiseField
            |> clipRampRate
            |> nearestCalibratedRampRate

    /// Functions related to CW EPR experiment notes.
    module Notes = 
        
        /// Returns the sample notes.
        let sampleNotes notes = notes.SampleNotes

        /// Returns the experiment notes.
        let experimentNotes notes = notes.ExperimentNotes
        
        /// Returns updated notes with the specified sample notes.
        let withSampleNotes sampleNotes notes = 
            { notes with SampleNotes = sampleNotes }

        /// Returns updated notes with the specified experiment notes.
        let withExperimentNotes experimentNotes notes =
            { notes with ExperimentNotes = experimentNotes }

    /// Functions related to CW EPR experiment scan parts.
    module private ScanPart =
        
        /// Returns the scan parts for the given CW EPR experiment parameters.
        let fromExperimentParameters parameters =
            let (initialCurrentDirection, initialStepIndex) = Parameters.initialFieldIndex parameters
            let (finalCurrentDirection,   finalStepIndex  ) = Parameters.finalFieldIndex   parameters

            if initialCurrentDirection = finalCurrentDirection then
                { InitialField     = Parameters.initialField parameters
                  FinalField       = Parameters.finalField parameters
                  RampRate         = Parameters.rampRate parameters
                  CurrentDirection = initialCurrentDirection
                  ConversionTime   = Parameters.conversionTime parameters }
                |> OnePart
            else
                let staticField = Parameters.magnetControllerSettings parameters |> MagnetController.Settings.staticField
                let first =
                    { InitialField     = Parameters.initialField parameters
                      FinalField       = staticField
                      RampRate         = Parameters.rampRate parameters
                      CurrentDirection = initialCurrentDirection
                      ConversionTime   = Parameters.conversionTime parameters }
                
                let second =
                    { InitialField     = staticField
                      FinalField       = Parameters.finalField parameters
                      RampRate         = Parameters.rampRate parameters
                      CurrentDirection = finalCurrentDirection
                      ConversionTime   = Parameters.conversionTime parameters }

                TwoPart (first, second)

        /// Returns the initial field of the given scan part.
        let initialField scanPart = scanPart.InitialField

        /// Returns the final field of the given scan part.
        let finalField scanPart = scanPart.FinalField

        /// Returns the ramp rate of the given scan part.
        let rampRate scanPart = scanPart.RampRate

        /// Returns the current direction of the given scan part.
        let currentDirection scanPart = scanPart.CurrentDirection

        /// Returns the conversion time of the given scan part.
        let conversionTime scanPart = scanPart.ConversionTime

        /// Returns the conversion time of the given scan part with a unit of measure.
        let conversionTimeInSeconds scanPart = 0.001M<s> * (decimal (int <| conversionTime scanPart))

        /// Returns the number of samples per CW EPR signal point for the given scan part.
        let samplesPerPoint scanPart = (conversionTime scanPart) / 20<ms>
        
        /// Returns the estimated magnetic field ramp duration for the given scan part.
        let rampDuration scanPart = (abs (finalField scanPart - initialField scanPart)) / (rampRate scanPart)

        /// Returns the initial field magnet controller step index for the given scan part.
        let initialFieldIndex magnetControllerSettings = 
            initialField >> MagnetController.Settings.Convert.magneticFieldToStepIndex magnetControllerSettings >> snd

        /// Returns the final field magnet controller step index for the given scan part.
        let finalFieldIndex magnetControllerSettings =
            finalField >> MagnetController.Settings.Convert.magneticFieldToStepIndex magnetControllerSettings >> snd

        /// Returns the ramp rate index for the given scan part.
        let rampRateIndex magnetControllerSettings scanPart =
            rampRate scanPart
            / (abs (MagnetController.Settings.linearFieldCoefficient magnetControllerSettings))
            |> (MagnetController.Settings.RampRate.nearestIndex magnetControllerSettings)

        /// Returns the magnet controller field sweep parameters for the given scan part.
        let fieldSweepParameters magnetControllerSettings scanPart =
            FieldSweep.Parameters.create
            <| currentDirection scanPart
            <| initialFieldIndex magnetControllerSettings scanPart
            <| finalFieldIndex   magnetControllerSettings scanPart
            <| rampRateIndex     magnetControllerSettings scanPart

        /// Returns the number of CW EPR signal points in the given magnet controller scan part.
        let numberOfPoints scanPart = 
            (abs <| initialField scanPart - finalField scanPart) / ((rampRate scanPart) * (conversionTimeInSeconds scanPart))
            |> floor |> int

        /// Returns the magnetic field for the n-th CW EPR signal point in the given scan part.
        let magneticFieldForPoint scanPart n =
            let stepPerSample = 
                if initialField scanPart < finalField scanPart
                then +0.020M<s> * (rampRate scanPart)
                else -0.020M<s> * (rampRate scanPart)

            seq { for sample in 0 .. samplesPerPoint scanPart - 1 -> 
                    initialField scanPart + (decimal (sample + n * samplesPerPoint scanPart)) * stepPerSample }
            |> Seq.average

        /// Returns sequence of magnetic field values for the CW EPR signal points in the given
        /// scan part in order of acquisition.
        let magneticFieldPoints scanPart =
            seq { for n in 0 .. numberOfPoints scanPart - 1 -> magneticFieldForPoint scanPart n }

    /// Functions related to shunt voltage ramp fitting.
    module private ShuntVoltageRamp =
        
        /// Returns the initial voltage of the shunt voltage ramp.
        let initialVoltage ramp = ramp.InitialVoltage

        /// Returns the final voltage of the shunt voltage ramp.
        let finalVoltage ramp = ramp.FinalVoltage
        
        /// Returns the duration of the shunt voltage ramp.
        let duration ramp = ramp.Duration

        /// Returns the start time of the shunt voltage ramp.
        let startTime ramp = ramp.StartTime
        
        /// Returns the finish time for the shunt voltage ramp
        let finishTime ramp = (startTime ramp) + (duration ramp)
        
        /// Computes the voltage at the specified time for the given shunt voltage ramp.
        let voltage ramp = function
            | x when x <= startTime ramp  -> initialVoltage ramp
            | x when x >= finishTime ramp -> finalVoltage ramp
            | x -> 
                initialVoltage ramp
                + ((x - startTime ramp) * (finalVoltage ramp - initialVoltage ramp) 
                    / duration ramp)

        /// Returns the expected shunt voltage ramp for the given scan part.
        let expectedForScanPart scanPart magnetControllerSettings =
            let magneticFieldToShuntVoltage = 
                MagnetController.Settings.Convert.magneticFieldToShuntVoltage magnetControllerSettings
                >> snd >> decimal >> float >> LanguagePrimitives.FloatWithMeasure<V>

            { InitialVoltage = ScanPart.initialField scanPart |> magneticFieldToShuntVoltage
              FinalVoltage   = ScanPart.finalField   scanPart |> magneticFieldToShuntVoltage
              StartTime      = 10.0<s>
              Duration       = 1.0<s> * float (decimal <| ScanPart.rampDuration scanPart) }

        /// Iteratively fits a linear ramp to the given array of shunt voltage samples starting
        /// with the specified guess and keeping the ramp duration fixed.
        let fitWithFixedDuration guess (data : (float<s> * float<V>) array) =
            let rampFunc parameters (dys : float array) (_ : System.Collections.Generic.IList<float> []) (data : obj) =
                match parameters with
                | [| initial ; final ; start |] -> 
                    let data' = data :?> (float * float) array
                    let ramp = 
                        { InitialVoltage = initial * 1.0<V>
                          FinalVoltage   = final * 1.0<V> 
                          StartTime      = start * 1.0<s>
                          Duration       = duration guess }
                    
                    data' |> Array.iteri (fun i (x, y) ->
                        let y' = voltage ramp (x * 1.0<s>)
                        dys.[i] <- float (1.0<V> * y - y'))

                    0
                | _ -> failwith "Parameter array has unexpected length."

            let parameters = [| float guess.InitialVoltage ; float guess.FinalVoltage ; float guess.StartTime |]
            let mutable result = mp_result(3)
            MPFit.Solve(mp_func(rampFunc), Array.length data, 3, parameters, null, null, data, &result) |> ignore

            { InitialVoltage = parameters.[0] * 1.0<V>
              FinalVoltage   = parameters.[1] * 1.0<V> 
              StartTime      = parameters.[2] * 1.0<s>
              Duration       = duration guess }
    
    /// Functions and parameters related to signal sampling.        
    module private Sampling =
        
        /// Digitiser sample interval.
        let private sampleInterval = Interval.fromMicroseconds 20<us>
        
        /// Digitiser vertical resolution.
        let private samplingResolution = Resolution_14bit
        
        /// Digitiser downsampling mode.
        let private downsamplingMode = Averaged
        
        /// Downsampling ratio.
        let private downsamplingRatio = 1000u // downsample data to 20 ms per sample to average mains hum
        
        /// Sample buffer.
        let private sampleBuffer = AveragedBuffer
        
        /// Magnetic field shunt voltage input channel.
        let private magneticFieldChannel = ChannelA
        
        /// EPR signal channels.
        let private signalChannels parameters =
            if Parameters.detection parameters = Quadrature
            then [ ChannelB ; ChannelC ]
            else [ ChannelB ]

        /// Returns the expected magnetic field shunt voltage range during the experiment.
        let private shuntVoltageRange parameters =
            let settings = Parameters.magnetControllerSettings parameters
            let (initialCurrentDirection, initialStepIndex) = Parameters.initialFieldIndex parameters
            let (finalCurrentDirection,   finalStepIndex  ) = Parameters.finalFieldIndex   parameters
            
            let initialShuntVoltage = MagnetController.Settings.Convert.stepIndexToShuntVoltage settings initialStepIndex
            let finalShuntVoltage   = MagnetController.Settings.Convert.stepIndexToShuntVoltage settings finalStepIndex
            let shuntOffset         = MagnetController.Settings.shuntVoltageOffset settings
            let shuntNoise          = MagnetController.Settings.shuntVoltageRmsNoise settings

            let minimumShuntVoltage =
                if initialCurrentDirection = finalCurrentDirection
                then min initialShuntVoltage finalShuntVoltage
                else min initialShuntVoltage finalShuntVoltage |> min shuntOffset
                
            let maximumShuntVoltage =
                if initialCurrentDirection = finalCurrentDirection
                then max initialShuntVoltage finalShuntVoltage
                else max initialShuntVoltage finalShuntVoltage |> max shuntOffset

            (minimumShuntVoltage - shuntNoise, maximumShuntVoltage + shuntNoise)
        
        /// Returns magnetic field channel input voltage offset.
        let private magneticFieldChannelOffset parameters =
            let (minimumVoltage, maximumVoltage) = shuntVoltageRange parameters
            - 1.0f<V> * (float32 <| Decimal.Round((minimumVoltage + maximumVoltage) / 2.0M<V>, 1))
           
        /// Returns the magnetic field channel input voltage range.
        let private magneticFieldChannelRange parameters = 
            let offset = magneticFieldChannelOffset parameters 
            
            let minimumForOffset = 
                offset 
                |> Range.minimumForOffsetVoltage
                |> function
                    | Some range' -> range'
                    | None        -> failwithf "Required shunt voltage offset (%f V) exceeds maximum." offset
            
            let (minimumVoltage, maximumVoltage) = shuntVoltageRange parameters

            let minimumForRange =
                abs (1.0f<V> * (float32 (decimal minimumVoltage)) + offset)
                |> max (abs (1.0f<V> * (float32 (decimal maximumVoltage)) + offset))
                |> Range.minimumForVoltage
                
            [ minimumForOffset ; minimumForRange ] |> List.maxBy Range.voltage

        /// Returns the conversion frunction from ADC counts to voltage for the magnetic field channel.
        let shuntAdcToVoltage parameters = 
            Voltage.fromAdcCounts
            <| samplingResolution
            <| magneticFieldChannelRange parameters
            <| magneticFieldChannelOffset parameters

        /// Signal channel range.
        let private signalChannelRange  = Range_10V
        
        /// Signal channel offset.
        let private signalChannelOffset = 0.0f<V>

        /// Streaming acquisition parameters.
        let streamingParameters parameters =
            let magneticFieldChannelOffset' = magneticFieldChannelOffset parameters
            let magneticFieldChannelRange'  = magneticFieldChannelRange  parameters
            let signalChannels' = signalChannels parameters

            Parameters.Acquisition.create sampleInterval samplingResolution (64 * 1024)
            |> Parameters.Acquisition.enableChannel magneticFieldChannel DC magneticFieldChannelRange' magneticFieldChannelOffset' Bandwidth_20MHz
            |> Parameters.Acquisition.enableChannels signalChannels' DC signalChannelRange signalChannelOffset Bandwidth_20MHz
            |> Parameters.Acquisition.sampleChannels (magneticFieldChannel :: signalChannels') downsamplingMode
            |> Parameters.Acquisition.withDownsamplingRatio downsamplingRatio
            |> Parameters.Streaming.create
            |> Parameters.Streaming.streamingCapture

        /// Returns an observable sequence of CW EPR sample blocks from the streaming acquisition.
        let samplesForAcquisition parameters acquisition =
            let channels = magneticFieldChannel :: signalChannels parameters
            let detection = Parameters.detection parameters
            let inputs = channels |> List.map (fun channel -> (channel, sampleBuffer)) |> Array.ofList

            Signal.adcCountsByBlock inputs acquisition
            |> Observable.map (fun block -> 
                let blockLength = block.[0] |> Array.length
                seq { for i in 0 .. blockLength - 1 ->
                        { MagneticFieldShuntAdc = block.[0].[i]
                          ReSignalAdc           = block.[1].[i]
                          ImSignalAdc           = match detection with Quadrature -> block.[2].[i] | SinglePhase -> 0s } })
      
    /// Functions related to signal processing and accumulation.
    module Signal =
        
        /// Returns the raw data for all completed scans in the signal.
        let private completedScans signal = signal.CompletedScans
        
        /// Returns the accumulated CW EPR signal points for the scans completed so far.
        let private accumulatedSignal signal = signal.AccumulatedSignal
        
        /// Returns the samples for the scan currently being accumulated in the signal.
        let private currentScan signal = signal.CurrentScan

        /// Returns an empty set of points with magnetic field values corresponding to the given scan
        /// part and zero signal intensity.
        let private emptyPoints scanPart =
            ScanPart.magneticFieldPoints scanPart
            |> Seq.map (fun field -> { MagneticField = field ; ReSignalIntensity = 0.0M ; ImSignalIntensity = 0.0M })

        /// Returns an empty CW EPR signal.
        let empty parameters =
            { CompletedScans = Map.empty
              AccumulatedSignal =
                match ScanPart.fromExperimentParameters parameters with
                | OnePart scanPart        -> emptyPoints scanPart |> Array.ofSeq
                | TwoPart (first, second) -> Seq.append (emptyPoints first) (emptyPoints second) |> Array.ofSeq
              CurrentScan = None }

        /// Returns the fitted shunt voltage ramp for the given scan part and samples.
        let private shuntVoltageRamp parameters (scanPart, samples : ResizeArray<_>) =
            let shuntToAdcVoltage = 
                Sampling.shuntAdcToVoltage parameters
                >> float >> LanguagePrimitives.FloatWithMeasure<V>  

            let estimatedRamp = ShuntVoltageRamp.expectedForScanPart scanPart (Parameters.magnetControllerSettings parameters)
            
            samples
            |> Seq.mapi (fun i sample -> (float i * 0.020<s>, shuntToAdcVoltage sample.MagneticFieldShuntAdc))
            |> Array.ofSeq
            |> ShuntVoltageRamp.fitWithFixedDuration estimatedRamp 

        /// Returns the CW EPR signal points for the given scan part and samples.
        let private scanSignal parameters (scanPart, samples : ResizeArray<_>) =
            let ramp = shuntVoltageRamp parameters (scanPart, samples)
            let length = samples.Count

            let skipCount = 
                int (round (ShuntVoltageRamp.startTime ramp / 0.020<s>)) - 1 |> min length |> max 0
            
            let takeCount = 
                (ScanPart.numberOfPoints scanPart * ScanPart.samplesPerPoint scanPart) 
                |> min (length - skipCount) |> max 0

            samples
            |> Seq.skip skipCount
            |> Seq.take takeCount
            |> Seq.chunkBySize (ScanPart.samplesPerPoint scanPart)
            |> Seq.mapi (fun i pointSamples ->
                { MagneticField     = ScanPart.magneticFieldForPoint scanPart i
                  ReSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ReSignalAdc)
                  ImSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ImSignalAdc) })

        /// Initialises the next scan part in a CW EPR signal.
        let private initialiseScanPart n scanPart signal =
            match currentScan signal with
            | Some _ -> failwith "Cannot initialise a new scan before the current one is completed."
            | None   -> { signal with CurrentScan = Some (n, scanPart, new ResizeArray<_>()) }

        /// Accumulates the given samples to the current scan in a CW EPR signal.
        let private accumulateSamples (samples : CwEprSample seq) signal =
            match currentScan signal with
            | Some (_, _, samples') -> samples'.AddRange samples ; signal
            | None                  -> failwith "Cannot accumulate data when no scan is in progress."

        /// Computes the start index signal due to the given scan part in the CW EPR signal point array.
        let private scanPartSignalStartIndex parameters scanPart =
            match parameters |> ScanPart.fromExperimentParameters with
            | OnePart first          
            | TwoPart (first, _)      when first  = scanPart -> 0
            | TwoPart (first, second) when second = scanPart -> ScanPart.numberOfPoints first
            | _                                              -> failwith "Scan part not part of given experiment parameters."

        /// Accumulates the given scan part and samples to the given CW EPR signal point array.
        let accumulateScanPart parameters (accumulatedSignal : CwEprSignalPoint array) (scanPart, samples) =
            let offset = scanPartSignalStartIndex parameters scanPart
            scanSignal parameters (scanPart, samples)
            |> Seq.iteri (fun i x ->
                accumulatedSignal.[offset + i] <- 
                    { accumulatedSignal.[offset + i] with
                        ReSignalIntensity = accumulatedSignal.[offset + i].ReSignalIntensity + x.ReSignalIntensity
                        ImSignalIntensity = accumulatedSignal.[offset + i].ImSignalIntensity + x.ImSignalIntensity })

        /// Completes the scan part currently in progress in the CW EPR signal, accumulating the current
        /// scan samples to the signal and adding the data to the completed scans.
        let private completeScanPart parameters signal =
            match currentScan signal with
            | Some (n, scanPart, samples) ->
                accumulateScanPart parameters (accumulatedSignal signal) (scanPart, samples)
                { signal with 
                    CurrentScan    = None 
                    CompletedScans = completedScans signal |> Map.add (n, scanPart) (Array.ofSeq samples) }
            | None -> failwith "Cannot complete scan when one is not in progress."

        /// Applies the given update to the CW EPR signal.
        let update parameters signal = function
            | SamplesAvailable samples      -> accumulateSamples samples signal
            | ScanPartStarted (n, scanPart) -> initialiseScanPart n scanPart signal
            | ScanPartCompleted             -> completeScanPart parameters signal

        /// Returns a snapshot of the CW EPR signal, containing a copy of the accumulated signal and a
        /// copy of the scan currently in progress.
        let snapshot parameters signal =
            (Array.copy signal.AccumulatedSignal,
                currentScan signal |> Option.map (fun (n, scanPart, samples) -> n, scanPart, ResizeArray<_>(samples)))            

        /// Returns a copy of the raw data accumulated so far in the CW EPR signal.
        let rawDataCopy signal = 
            match currentScan signal with
            | None -> completedScans signal |> Map.map (fun _ scan -> Array.copy scan)
            | Some (n, scanPart, samples) ->
                completedScans signal 
                |> Map.map (fun _ scan -> Array.copy scan)
                |> Map.add (n, scanPart) (Array.init samples.Count (fun i -> samples.[i]))

        /// Returns the real part of the CW EPR signal points.
        let reSignal (parameters : CwEprExperimentParameters) (points : CwEprSignalPoint array) =
            Some (points |> Seq.map (fun x -> x.MagneticField, x.ReSignalIntensity))

        /// Returns the imaginary part of the CW EPR signal points.
        let imSignal (parameters : CwEprExperimentParameters) (points : CwEprSignalPoint array) =
            if (Parameters.detection parameters) = Quadrature
            then Some (points |> Seq.map (fun x -> x.MagneticField, x.ImSignalIntensity))
            else None

        /// Returns the CSV rows for the CW EPR signal points.
        let signalRows points = seq {
            yield "Magnetic field (T), Real signal, Imaginary signal"
            
            yield! points
            |> Seq.ofArray
            |> Seq.map (fun signal ->
                [ string <| signal.MagneticField
                  string <| signal.ReSignalIntensity
                  string <| signal.ImSignalIntensity ]
                |> String.concat ", " ) }
        
        /// Returns the CSV rows for the CW EPR signal raw data.
        let rawDataRows (rawData : RawData) = seq {
            yield "Scan, Current direction, Time (s), Shunt ADC, Real ADC, Imaginary ADC"
            
            yield! rawData
            |> Map.toSeq
            |> Seq.collect (fun ((n, scanPart), samples) ->
                samples
                |> Seq.mapi (fun i sample -> 
                    [ string n
                      (match ScanPart.currentDirection scanPart with Forward -> "Forward" | Reverse -> "Reverse")
                      string <| (float i * 0.020)
                      string <| sample.MagneticFieldShuntAdc
                      string <| sample.ReSignalAdc
                      string <| sample.ImSignalAdc ]
                    |> String.concat ", ")) }

    /// Functions for posting samples to a signal processor agent and getting snapshots of the signal.
    module SignalProcessor = 
      
        /// Creates a signal processor agent initialised with an empty data set for the given CW EPR
        /// experiment parameters.
        let create parameters =
            let agent = Agent.Start (fun mailbox ->
                let rec loop signal = async {
                    let! message = mailbox.Receive()
                    match message with
                    | SignalUpdate signalUpdate -> 
                        return! loop (Signal.update parameters signal signalUpdate)
                
                    | SignalSnapshotRequest replyChannel ->
                        Signal.snapshot parameters signal |> replyChannel.Reply
                        return! loop signal
                
                    | RawDataRequest replyChannel ->
                        Signal.rawDataCopy signal |> replyChannel.Reply
                        return! loop signal }
            
                loop (Signal.empty parameters))

            CwEprSignalProcessor(agent, parameters)
        
        /// Posts a block of samples for the scan part currently in progress to the signal processor.
        let accumulateSamples (CwEprSignalProcessor (agent, _)) samples =
            SamplesAvailable samples |> SignalUpdate |> agent.Post

        /// Initialises the next scan part on the signal processor.
        let initiateScanPart (CwEprSignalProcessor (agent, _))  (n, scanPart) =
            ScanPartStarted (n, scanPart) |> SignalUpdate |> agent.Post

        /// Completes the scan part currently in progress on the signal processor.
        let completeScanPart (CwEprSignalProcessor (agent, _)) =
            ScanPartCompleted |> SignalUpdate |> agent.Post

        /// Gets a snapshot of the current signal from the signal processor.
        let getSnapshot (CwEprSignalProcessor (agent, parameters)) = async {
            let! (accumulatedSignal, scanPart) = SignalSnapshotRequest |> agent.PostAndAsyncReply 
            match scanPart with
            | Some (_, scanPart, samples) -> Signal.accumulateScanPart parameters accumulatedSignal (scanPart, samples)
            | None                        -> () 
            
            return accumulatedSignal }

        /// Gets a copy of the raw data from the signal processor.
        let getRawData (CwEprSignalProcessor (agent, _)) =  RawDataRequest |> agent.PostAndAsyncReply

    /// Creates a CW EPR experiment with the specified parameters using the given magnet controller and digitiser.
    let create parameters magnetController picoScope =
        { Parameters              = parameters |> Parameters.withMagnetControllerSettings (MagnetController.settings magnetController)
          MagnetController        = magnetController
          PicoScope               = picoScope
          StatusChanged           = NotificationEvent<_>()
          SignalProcessor         = SignalProcessor.create parameters
          StopAfterScanCapability = new CancellationCapability() }

    /// Gets the signal processor for the experiment.
    let private signalProcessor experiment = experiment.SignalProcessor

    /// Gets the magnet controller for the experiment.
    let private magnetController experiment = experiment.MagnetController

    /// Gets the digitiser for the experiment.
    let private picoScope experiment = experiment.PicoScope
    
    /// Sets the experiment to stop after the current scan.
    let stopAfterScan experiment = experiment.StopAfterScanCapability.Cancel()

    /// Checks whether the experiment is set to stop after the current scan.
    let private isSetToStopAfterScan experiment = experiment.StopAfterScanCapability.IsCancellationRequested

    /// Trigers a status update on the experiment.
    let private triggerStatusChanged experiment status = experiment.StatusChanged.Trigger (Next status)

    /// Triggers an error on the status observable sequence for the experiment.
    let private triggerError experiment exn = experiment.StatusChanged.Trigger (Error exn)

    /// Triggers completion on the status observable sequence for the experiment.
    let private triggerCompleted experiment = experiment.StatusChanged.Trigger Completed

    /// Gets the parameters for the experiment.
    let parameters experiment = experiment.Parameters

    /// Gets an observable sequence of status updates for the experiment.
    let status experiment = 
        experiment.StatusChanged.Publish
        |> Observable.fromNotificationEvent

    /// Gets the message corresponding to a given experiment status.
    let statusMessage = function
        | StartingExperiment         -> "Starting experiment"
        | PerformingExperimentScan n -> sprintf "Performing scan %d" n
        | StoppedAfterScan n         -> sprintf "Stopped after scan %d" n
        | FinishedExperiment         -> "Finished experiment"
        | CancelledExperiment        -> "Cancelled experiment"

    /// Accumulates the samples from the given streaming acquisition to the signal processor.
    let private processSamples experiment =
        Sampling.samplesForAcquisition (parameters experiment)
        >> Observable.subscribe (SignalProcessor.accumulateSamples (signalProcessor experiment))

    /// Performs the n-th scan part of the experiment.
    let private performScanPart n parameters scanPart experiment = async {
        let acquisition              = Acquisition.prepare (picoScope experiment) parameters
        let magnetControllerSettings = MagnetController.settings (magnetController experiment)
        let scanDuration             = ScanPart.rampDuration scanPart
        let fieldSweepParameters     = ScanPart.fieldSweepParameters magnetControllerSettings scanPart
        let fieldSweep               = FieldSweep.create (magnetController experiment) fieldSweepParameters

        (n, scanPart) |> SignalProcessor.initiateScanPart (signalProcessor experiment)

        let! waitToPrepare = FieldSweep.waitToPrepare fieldSweep |> Async.StartChild
        let! sweepHandle   = FieldSweep.prepareAsChild fieldSweep
        do! waitToPrepare
        
        use __ = acquisition |> processSamples experiment
        let! waitForStreaming = Acquisition.waitToStart acquisition |> Async.StartChild
        let! acquisitionHandle = Acquisition.startAsChild acquisition
        do! waitForStreaming
        do! Async.Sleep 10000
        FieldSweep.setReadyToSweep fieldSweep
        
        let delay = 10000 + int (scanDuration * 1000.0M</s>)
        do! Async.Sleep delay
        do! Acquisition.stopAndFinish acquisitionHandle |> Async.Ignore 
        SignalProcessor.completeScanPart (signalProcessor experiment) }

    /// Performs the n-th scan of the experiment.
    let private peformScan n experiment = async {
        PerformingExperimentScan n |> triggerStatusChanged experiment
        let scanParts = ScanPart.fromExperimentParameters (parameters experiment)
        let streamingParameters = Sampling.streamingParameters (parameters experiment)

        match scanParts with
        | OnePart scanPart -> 
            do! performScanPart n streamingParameters scanPart experiment
        | TwoPart (first, second) ->
            do! performScanPart n streamingParameters first  experiment
            do! performScanPart n streamingParameters second experiment }

    /// Recursively performs the scans from n up to the end of the experiment.
    let rec private performScansFrom n experiment = async {
        do! peformScan n experiment 
        if n < Parameters.numberOfScans (parameters experiment) then
            if   isSetToStopAfterScan experiment
            then StoppedAfterScan n |> triggerStatusChanged experiment
            else do! performScansFrom (n + 1) experiment }

    /// Runs the given CW EPR experiment with the specified cancellation token.
    let run experiment cancellationToken =
        let resultChannel = new ResultChannel<_>()

        let experimentWorkflow = async {
            experiment.StatusChanged.Trigger <| Next StartingExperiment
            do! performScansFrom 1 experiment }

        Async.StartWithContinuations (experimentWorkflow,
            (fun () -> 
                FinishedExperiment |> triggerStatusChanged experiment
                triggerCompleted experiment
                resultChannel.RegisterResult ExperimentCompleted),
            (fun exn ->
                exn |> triggerError experiment
                resultChannel.RegisterResult (ExperimentError exn)),
            (fun exn ->
                CancelledExperiment |> triggerStatusChanged experiment
                triggerCompleted experiment
                resultChannel.RegisterResult ExperimentCancelled),
            cancellationToken)
        
        resultChannel.AwaitResult()

    /// Gets a copy of the latest raw data for the experiment.
    let rawData = signalProcessor >> SignalProcessor.getRawData

    /// Gets a snapshot of the latest CW EPR signal point array for the experiment.
    let signalSnapshot = signalProcessor >> SignalProcessor.getSnapshot

    /// Waits for the experiment to finish.
    let waitToFinish experiemnt =
        status experiemnt
        |> Observable.last
        |> Async.AwaitObservable
        |> Async.Ignore