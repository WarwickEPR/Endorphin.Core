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

module CwEprExperiment =
    
    [<AutoOpen>]
    module Model =

        type FieldSweepDirection = Increasing | Decreasing
        type Detection = Quadrature | SinglePhase

        type Notes = { SampleNotes : string ; ExperimentNotes : string }

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

        type ScanPartParameters =
            { InitialField     : decimal<T>
              FinalField       : decimal<T>
              RampRate         : decimal<T/s>
              CurrentDirection : CurrentDirection
              ConversionTime   : int<ms> }

        type ExperimentScanParts =
            | OnePart of scan : ScanPartParameters
            | TwoPart of first : ScanPartParameters * second : ScanPartParameters

        type CwEprSample =
            { MagneticFieldShuntAdc : AdcCount
              ReSignalAdc           : AdcCount
              ImSignalAdc           : AdcCount }

        type CwEprSignalPoint =
            { MagneticField     : decimal<T>
              ReSignalIntensity : decimal
              ImSignalIntensity : decimal }

        type ShuntVoltageRamp =
            { InitialVoltage : float<V>
              FinalVoltage   : float<V>
              StartTime      : float<s>
              Duration       : float<s> }

        type ScanPartInProgress = ScanPartParameters * (CwEprSample list)

        type CwEprSignal = 
            { CompletedScans    : Map<int * ScanPartParameters, CwEprSample array>
              AccumulatedSignal : CwEprSignalPoint array
              CurrentScan       : ScanPartInProgress option }

        type CwEprSignalUpdate =
            | SamplesAvailable  of samples : CwEprSample seq
            | ScanPartStarted   of scanPart : ScanPartParameters
            | ScanPartCompleted of scanNumber : int

        type CwEprExperimentStatus =
            | StartingExperiment
            | PerformingExperimentScan of scan : int
            | StoppedAfterScan of scan : int
            | FinishedExperiment
            | CancelledExperiment

        type CwEprExperimentResult =
            | ExperimentCompleted
            | ExperimentError of exn
            | ExperimentCancelled

        type CwEprExperiment = 
             { Parameters              : CwEprExperimentParameters
               MagnetController        : MagnetController
               PicoScope               : PicoScope5000
               StatusChanged           : NotificationEvent<CwEprExperimentStatus>
               SignalUpdated           : Event<CwEprSignalUpdate>
               StopAfterScanCapability : CancellationCapability }

    module private ScanPart =
        
        let create initialField finalField rampRate currentDirection conversionTime =
            { InitialField     = initialField
              FinalField       = finalField
              RampRate         = rampRate
              CurrentDirection = currentDirection
              ConversionTime   = conversionTime }

        let initialField scanPart = scanPart.InitialField

        let finalField scanPart = scanPart.FinalField

        let rampRate scanPart = scanPart.RampRate

        let currentDirection scanPart = scanPart.CurrentDirection

        let conversionTime scanPart = scanPart.ConversionTime

        let conversionTimeInSeconds scanPart = 0.001M<s> * (decimal (int <| conversionTime scanPart))

        let samplesPerPoint scanPart = (conversionTime scanPart) / 20<ms>
        
        let duration scanPart =
             (abs (finalField scanPart - initialField scanPart)) / (rampRate scanPart)

        let initialFieldIndex magnetControllerSettings = 
            initialField >> MagnetController.Settings.Convert.magneticFieldToStepIndex magnetControllerSettings >> snd

        let finalFieldIndex magnetControllerSettings =
            finalField >> MagnetController.Settings.Convert.magneticFieldToStepIndex magnetControllerSettings >> snd

        let rampRateIndex magnetControllerSettings scanPart =
            rampRate scanPart
            / (abs (MagnetController.Settings.linearFieldCoefficient magnetControllerSettings))
            |> (MagnetController.Settings.RampRate.nearestIndex magnetControllerSettings)

        let fieldSweepParameters magnetControllerSettings scanPart =
            FieldSweep.Parameters.create
            <| currentDirection scanPart
            <| initialFieldIndex magnetControllerSettings scanPart
            <| finalFieldIndex   magnetControllerSettings scanPart
            <| rampRateIndex     magnetControllerSettings scanPart

        let numberOfPoints scanPart = 
            (abs <| initialField scanPart - finalField scanPart) / ((rampRate scanPart) * (conversionTimeInSeconds scanPart))
            |> floor |> int

        let magneticFieldForPoint scanPart n =
            let stepPerSample = 
                if initialField scanPart < finalField scanPart
                then +0.020M<s> * (rampRate scanPart)
                else -0.020M<s> * (rampRate scanPart)

            seq { for sample in 0 .. samplesPerPoint scanPart - 1 -> 
                    initialField scanPart + (decimal (sample + n * samplesPerPoint scanPart)) * stepPerSample }
            |> Seq.average

        let magneticFieldPoints scanPart =
            seq { for n in 0 .. numberOfPoints scanPart - 1 -> magneticFieldForPoint scanPart n }

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

        let rampRate (parameters : CwEprExperimentParameters) = parameters.RampRate

        let numberOfScans parameters = parameters.NumberOfScans

        let conversionTime (parameters : CwEprExperimentParameters) = parameters.ConversionTime

        let detection parameters = parameters.Detection

        let scanDuration parameters = sweepWidth parameters / rampRate parameters

        let experimentDuration parameters =
            (decimal <| numberOfScans parameters) * (scanDuration parameters)

        let notes parameters = parameters.Notes

        let magnetControllerSettings parameters =  parameters.MagnetControllerSettings

        let samplesPerPoint parameters = conversionTime parameters / 20<ms>

        let rec initialFieldIndex parameters = 
            let (currentDirection, stepIndex) =
                initialField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| finalFieldIndex parameters, stepIndex)

        and finalFieldIndex parameters =
            let (currentDirection, stepIndex) =
                finalField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| initialFieldIndex parameters, stepIndex)

        let scanParts parameters =
            let (initialCurrentDirection, initialStepIndex) = initialFieldIndex parameters
            let (finalCurrentDirection,   finalStepIndex  ) = finalFieldIndex   parameters

            if initialCurrentDirection = finalCurrentDirection then
                ScanPart.create 
                <| initialField parameters
                <| finalField parameters
                <| rampRate parameters
                <| initialCurrentDirection
                <| conversionTime parameters
                |> OnePart
            else
                let staticField = magnetControllerSettings parameters |> MagnetController.Settings.staticField
                let first =
                    ScanPart.create
                    <| initialField parameters
                    <| staticField
                    <| rampRate parameters
                    <| initialCurrentDirection
                    <| conversionTime parameters
                
                let second =
                    ScanPart.create
                    <| staticField
                    <| finalField parameters
                    <| rampRate parameters
                    <| finalCurrentDirection
                    <| conversionTime parameters

                TwoPart (first, second)

        let private clipField parameters =
            let clip =
                min    (MagnetController.Settings.Limit.upperField <| magnetControllerSettings parameters)
                >> max (MagnetController.Settings.Limit.lowerField <| magnetControllerSettings parameters)
                
            let initialField' = clip <| initialField parameters
            let finalField'   = clip <| finalField parameters

            { parameters with
                CentreField = (initialField' + finalField') / 2.0M
                SweepWidth  = abs (finalField' - initialField') }

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

        let private clipRampRate parameters =
            let linearFieldCoeffiicent = abs (MagnetController.Settings.linearFieldCoefficient <| magnetControllerSettings parameters)
            let maximumRampRate = MagnetController.Settings.RampRate.maximum <| magnetControllerSettings parameters
            let rampRate' = min (rampRate parameters) (linearFieldCoeffiicent * maximumRampRate)

            { parameters with RampRate = rampRate' }

        let private nearestCalibratedRampRate parameters =
            let linearFieldCoefficient = abs (MagnetController.Settings.linearFieldCoefficient <| magnetControllerSettings parameters)

            let rampRate' =
                (rampRate parameters) / linearFieldCoefficient
                |> (MagnetController.Settings.RampRate.nearest <| magnetControllerSettings parameters)
                |> (*) linearFieldCoefficient

            { parameters with RampRate = rampRate' }

        let private conversionTimeIsMultipleOfMains parameters =
            let conversionTime' = max 20<ms> (((conversionTime parameters) / 20) * 20)
            { parameters with ConversionTime = conversionTime' }

        let private numberOfScansGreaterThanZero parameters =
            if numberOfScans parameters <= 0
            then { parameters with NumberOfScans = 1 }
            else parameters

        let private sweepWidthGreaterThanZero =
            (fun parameters ->
                if sweepWidth parameters <= 0.0M<T>
                then Some "Magnetic field sweep width must be greater than zero."
                else None)
            |> Validators.custom

        let validate = sweepWidthGreaterThanZero
        
        let withCentreField centreField parameters =
            { parameters with CentreField = centreField }
            |> clipField
            |> digitiseField

        let withSweepWidth sweepWidth parameters =
            { parameters with SweepWidth = sweepWidth }
            |> clipField
            |> digitiseField
        
        let withFieldSweepDirection direction parameters =
            { parameters with FieldSweepDirection = direction }

        let withRampRate rampRate (parameters : CwEprExperimentParameters) =
            { parameters with RampRate = rampRate }
            |> clipRampRate
            |> nearestCalibratedRampRate

        let withConversionTime conversionTime (parameters : CwEprExperimentParameters) =
            { parameters with ConversionTime = conversionTime }
            |> conversionTimeIsMultipleOfMains

        let withDetection detection parameters =
            { parameters with Detection = detection }

        let withNumberOfScans numberOfScans parameters =
            { parameters with NumberOfScans = numberOfScans }
            |> numberOfScansGreaterThanZero

        let withNotes notes parameters =
            { parameters with Notes = notes }

        let withDate date parameters = { parameters with Date = date }

        let withMagnetControllerSettings settings parameters =
            { parameters with MagnetControllerSettings = settings }
            |> clipField
            |> digitiseField
            |> clipRampRate
            |> nearestCalibratedRampRate
            |> conversionTimeIsMultipleOfMains
            |> numberOfScansGreaterThanZero

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

    module Notes = 
        
        let sampleNotes notes = notes.SampleNotes

        let experimentNotes notes = notes.ExperimentNotes
        
        let withSampleNotes sampleNotes notes = 
            { notes with SampleNotes = sampleNotes }

        let withExperimentNotes experimentNotes notes =
            { notes with ExperimentNotes = experimentNotes }

    module private ShuntVoltageRamp =

        let create initialVoltage finalVoltage startTime duration =
            { InitialVoltage = initialVoltage
              FinalVoltage   = finalVoltage 
              StartTime      = startTime
              Duration       = duration }

        let estimateForScanPart scanPart magneticFieldToVoltage =
            { InitialVoltage = ScanPart.initialField scanPart |> magneticFieldToVoltage
              FinalVoltage   = ScanPart.finalField   scanPart |> magneticFieldToVoltage
              StartTime      = 10.0<s>
              Duration       = 1.0<s> * float (decimal <| ScanPart.duration scanPart) }

        let initialVoltage ramp = ramp.InitialVoltage

        let finalVoltage ramp = ramp.FinalVoltage

        let startTime ramp = ramp.StartTime

        let duration ramp = ramp.Duration

        /// Linear ramp function between the specified initial and final values starting at the specified time
        /// and lasting the specified duration. Before the start time, the function takes the given initial
        /// value and after the ramp duration it takes the final value.
        let voltage ramp = function
            | x when x <= ramp.StartTime                 -> ramp.InitialVoltage
            | x when x >= ramp.StartTime + ramp.Duration -> ramp.FinalVoltage
            | x -> 
                ramp.InitialVoltage 
                + ((x - ramp.StartTime) * (ramp.FinalVoltage - ramp.InitialVoltage) 
                    / ramp.Duration)

        /// Iteratively fits a linear ramp to the given array of (x, y) points with the specified initial values
        /// for scaling factor a, mean value mu and standard deviation sigma. 
        let fitWithFixedDuration guess (data : (float<s> * float<V>) array) =
            let rampFunc parameters (dys : float array) (_ : System.Collections.Generic.IList<float> []) (data : obj) =
                match parameters with
                | [| initial ; final ; start |] -> 
                    let data' = data :?> (float * float) array
                    let ramp = create (initial * 1.0<V>) (final * 1.0<V>) (start * 1.0<s>) (duration guess)
                    data' |> Array.iteri (fun i (x, y) ->
                        let y' = voltage ramp (x * 1.0<s>)
                        dys.[i] <- float (1.0<V> * y - y'))

                    0
                | _ -> failwith "Parameter array has unexpected length."

            let parameters = [| float guess.InitialVoltage ; float guess.FinalVoltage ; float guess.StartTime |]
            let mutable result = mp_result(3)
            MPFit.Solve(mp_func(rampFunc), Array.length data, 3, parameters, null, null, data, &result) |> ignore
            create (parameters.[0] * 1.0<V>) (parameters.[1] * 1.0<V>) (parameters.[2] * 1.0<s>) (duration guess)
                    
    module private Sampling =
        let sampleInterval       = Interval.fromMicroseconds 20<us>
        let samplingResolution   = Resolution_14bit
        let downsamplingMode     = Averaged
        let downsamplingRatio    = 1000u // downsample data to 50 samples per sec to average mains hum
        let sampleBuffer         = AveragedBuffer
        let magneticFieldChannel = ChannelA
        
        let signalChannels parameters =
            if Parameters.detection parameters = Quadrature
            then [ ChannelB ; ChannelC ]
            else [ ChannelB ]

        let shuntVoltageRange parameters =
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

        let magneticFieldChannelOffset parameters =
            let (minimumVoltage, maximumVoltage) = shuntVoltageRange parameters
            - 1.0f<V> * (float32 <| Decimal.Round((minimumVoltage + maximumVoltage) / 2.0M<V>, 1))
            
        let magneticFieldChannelRange parameters = 
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

        let shuntAdcToVoltage parameters = 
            Voltage.fromAdcCounts
            <| samplingResolution
            <| (magneticFieldChannelRange parameters)
            <| (magneticFieldChannelOffset parameters)

        let signalChannelRange  = Range_10V
        let signalChannelOffset = 0.0f<V>

        let streamingParameters parameters =
            let magneticFieldChannelOffset' = magneticFieldChannelOffset parameters
            let magneticFieldChannelRange'  = magneticFieldChannelRange  parameters
            let signalChannels' = signalChannels parameters

            Streaming.Parameters.create samplingResolution sampleInterval (64u * 1024u)
            |> Streaming.Parameters.enableChannel magneticFieldChannel DC magneticFieldChannelRange' magneticFieldChannelOffset' Bandwidth_20MHz
            |> Streaming.Parameters.enableChannels signalChannels' DC signalChannelRange signalChannelOffset Bandwidth_20MHz
            |> Streaming.Parameters.sampleChannels (magneticFieldChannel :: signalChannels') downsamplingMode
            |> Streaming.Parameters.withDownsamplingRatio downsamplingRatio
    
    module Signal =
        
        let completedScans signal = signal.CompletedScans
        
        let accumulatedSignal signal = signal.AccumulatedSignal

        let currentScan signal = signal.CurrentScan

        let private emptyPoints scanPart =
            ScanPart.magneticFieldPoints scanPart
            |> Seq.map (fun field -> { MagneticField = field ; ReSignalIntensity = 0.0M ; ImSignalIntensity = 0.0M })

        let empty parameters =
            { CompletedScans = Map.empty
              AccumulatedSignal =
                match Parameters.scanParts parameters with
                | OnePart scanPart        -> emptyPoints scanPart |> Array.ofSeq
                | TwoPart (first, second) -> Seq.append (emptyPoints first) (emptyPoints second) |> Array.ofSeq
              CurrentScan = None }

        let private shuntVoltageRamp parameters (scanPart, samples) =
            let shuntToAdcVoltage = 
                Sampling.shuntAdcToVoltage parameters
                >> float >> LanguagePrimitives.FloatWithMeasure<V>  

            let magneticFieldToShuntVoltage = 
                MagnetController.Settings.Convert.magneticFieldToShuntVoltage (Parameters.magnetControllerSettings parameters)
                >> snd >> decimal >> float >> LanguagePrimitives.FloatWithMeasure<V>

            let estimatedRamp = ShuntVoltageRamp.estimateForScanPart scanPart magneticFieldToShuntVoltage
            
            samples
            |> List.rev
            |> Seq.ofList
            |> Seq.mapi (fun i sample -> (float i * 0.020<s>, shuntToAdcVoltage sample.MagneticFieldShuntAdc))
            |> Array.ofSeq
            |> ShuntVoltageRamp.fitWithFixedDuration estimatedRamp 

        let private scanSignal parameters (scanPart, samples) =
            let ramp = shuntVoltageRamp parameters (scanPart, samples)
            let length = List.length samples

            let skipCount = 
                int (round (ShuntVoltageRamp.startTime ramp / 0.020<s>)) - 1 |> min length |> max 0
            
            let takeCount = 
                (ScanPart.numberOfPoints scanPart * ScanPart.samplesPerPoint scanPart) 
                |> min (length - skipCount) |> max 0

            samples
            |> List.rev
            |> Seq.skip skipCount
            |> Seq.take takeCount
            |> Seq.chunkBySize (ScanPart.samplesPerPoint scanPart)
            |> Seq.mapi (fun i pointSamples ->
                { MagneticField     = ScanPart.magneticFieldForPoint scanPart i
                  ReSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ReSignalAdc)
                  ImSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ImSignalAdc) })

        let private initialiseScan signal scanPart =
            match currentScan signal with
            | Some _ -> failwith "Cannot initialise a new scan before the current one is completed."
            | None   -> { signal with CurrentScan = Some (scanPart, List.empty) }

        let private accumulateSamples signal (samples : CwEprSample seq) =
            match currentScan signal with
            | Some (scanPart, samples') -> { signal with CurrentScan = Some(scanPart, samples |> Seq.fold (fun xs x -> x :: xs) samples') }
            | None                      -> failwith "Cannot accumulate data when no scan is in progress."

        let private completeScan n signal parameters =
            match currentScan signal with
            | Some (scanPart, samples) ->
                let completedScanSignal = 
                    signal.AccumulatedSignal
                    |> Seq.ofArray
                    |> Seq.append (scanSignal parameters (scanPart, samples))
                    |> Seq.groupBy (fun signal -> signal.MagneticField)
                    |> Seq.map (fun (magneticField, signals) ->
                        { MagneticField     = magneticField
                          ReSignalIntensity = signals |> Seq.sumBy (fun signal -> signal.ReSignalIntensity)
                          ImSignalIntensity = signals |> Seq.sumBy (fun signal -> signal.ImSignalIntensity) })
                    |> Array.ofSeq

                { CompletedScans    = signal.CompletedScans |> Map.add (n, scanPart) (Array.ofSeq samples)
                  AccumulatedSignal = completedScanSignal
                  CurrentScan       = None }
            | None -> failwith "Cannot complete scan when one is not in progress."

        let update parameters signal = function
            | SamplesAvailable samples -> accumulateSamples signal samples
            | ScanPartStarted scanPart -> initialiseScan signal scanPart
            | ScanPartCompleted n      -> completeScan n signal parameters

        let reSignal parameters signal =
            match currentScan signal with
            | Some (scanPart, samples) ->
                accumulatedSignal signal
                |> Seq.ofArray
                |> Seq.append (scanSignal parameters (scanPart, samples))
                |> Seq.groupBy (fun signal -> signal.MagneticField)
                |> Seq.map (fun (magneticField, signals) -> magneticField, (signals |> Seq.sumBy (fun signal -> signal.ReSignalIntensity)))
            | None ->
                accumulatedSignal signal
                |> Seq.ofArray
                |> Seq.map (fun signal -> (signal.MagneticField, signal.ReSignalIntensity))

        let imSignal parameters signal =
            match currentScan signal with
            | Some (scanPart, samples) -> 
                accumulatedSignal signal
                |> Seq.ofArray
                |> Seq.append (scanSignal parameters (scanPart, samples))
                |> Seq.groupBy (fun signal -> signal.MagneticField)
                |> Seq.map (fun (magneticField, signals) -> magneticField, (signals |> Seq.sumBy (fun signal -> signal.ImSignalIntensity)))
            | None ->
                accumulatedSignal signal
                |> Seq.ofArray
                |> Seq.map (fun signal -> (signal.MagneticField, signal.ImSignalIntensity))

        let signalRows signal = seq {
            yield "Magnetic field (T), Real signal, Imaginary signal"
            
            yield! accumulatedSignal signal
            |> Seq.ofArray
            |> Seq.map (fun signal ->
                [ string <| signal.MagneticField
                  string <| signal.ReSignalIntensity
                  string <| signal.ImSignalIntensity ]
                |> String.concat ", " ) }

        let rawDataRows signal = seq {
            // header row, then samples
            yield "Scan, Current direction, Time (s), Shunt ADC, Real ADC, Imaginary ADC"
            
            yield! completedScans signal
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

    let create parameters magnetController picoScope =
        { Parameters              = parameters
          MagnetController        = magnetController
          PicoScope               = picoScope
          StatusChanged           = NotificationEvent<_>()
          SignalUpdated             = Event<_>()
          StopAfterScanCapability = new CancellationCapability() }

    let status experiment = 
        experiment.StatusChanged.Publish
        |> Observable.fromNotificationEvent

    let statusMessage = function
        | StartingExperiment         -> "Starting experiment"
        | PerformingExperimentScan n -> sprintf "Performing scan %d" n
        | StoppedAfterScan n         -> sprintf "Stopped after scan %d" n
        | FinishedExperiment         -> "Finished experiment"
        | CancelledExperiment        -> "Cancelled experiment"

    let private processSamples acquisition currentDirection experiment =
        let channels = Sampling.magneticFieldChannel :: Sampling.signalChannels experiment.Parameters
        let detection = Parameters.detection experiment.Parameters
        let inputs = channels |> List.map (fun channel -> (channel, Sampling.sampleBuffer)) |> Array.ofList
        
        let samples (block : AdcCount [][]) =
            let blockLength = block.[0] |> Array.length
            seq { for i in 0 .. blockLength - 1 ->
                    { MagneticFieldShuntAdc = block.[0].[i]
                      ReSignalAdc           = block.[1].[i]
                      ImSignalAdc           = match detection with Quadrature -> block.[2].[i] | SinglePhase -> 0s } }
            |> SamplesAvailable

        Streaming.Signal.adcCountsByBlock inputs acquisition
        |> Observable.subscribe (samples >> experiment.SignalUpdated.Trigger)

    let private performScanPart n streamingParameters scanPart experiment = async {
        let acquisition              = Streaming.Acquisition.create experiment.PicoScope streamingParameters
        let magnetControllerSettings = MagnetController.settings experiment.MagnetController
        let currentDirection         = ScanPart.currentDirection scanPart
        let scanDuration             = ScanPart.duration scanPart
        let fieldSweepParameters     = ScanPart.fieldSweepParameters magnetControllerSettings scanPart
        let fieldSweep               = FieldSweep.create experiment.MagnetController fieldSweepParameters

        experiment.SignalUpdated.Trigger (ScanPartStarted scanPart)

        let! waitToPrepare = FieldSweep.waitToPrepare fieldSweep |> Async.StartChild
        let! sweepHandle   = FieldSweep.prepareAsChild fieldSweep
        do! waitToPrepare
        
        use __ = processSamples acquisition currentDirection experiment
        let! waitForStreaming = Streaming.Acquisition.waitToStart acquisition |> Async.StartChild
        let! acquisitionHandle = Streaming.Acquisition.startAsChild acquisition
        do! waitForStreaming
        do! Async.Sleep 10000
        FieldSweep.setReadyToSweep fieldSweep
        
        let delay = 10000 + int (scanDuration * 1000.0M</s>)
        do! Async.Sleep delay
        do! Streaming.Acquisition.stopAndFinish acquisitionHandle |> Async.Ignore 
        
        experiment.SignalUpdated.Trigger (ScanPartCompleted n) }

    let private peformScan n experiment = async {
        experiment.StatusChanged.Trigger <| Next (PerformingExperimentScan n)
        let scanParts = Parameters.scanParts experiment.Parameters
        let streamingParameters = Sampling.streamingParameters experiment.Parameters

        match scanParts with
        | OnePart scanPart -> 
            do! performScanPart n streamingParameters scanPart experiment
        
        | TwoPart (first, second) ->
            do! performScanPart n streamingParameters first experiment
            do! MagnetController.Ramp.waitToReachZero experiment.MagnetController
            do! performScanPart n streamingParameters second experiment }

    let rec private performScansFrom n experiment = async {
        do! peformScan n experiment 
        if n < Parameters.numberOfScans experiment.Parameters then
            if   experiment.StopAfterScanCapability.IsCancellationRequested
            then experiment.StatusChanged.Trigger <| Next (StoppedAfterScan n)
            else do! performScansFrom (n + 1) experiment }

    let run experiment cancellationToken =
        let resultChannel = new ResultChannel<_>()

        let experimentWorkflow = async {
            experiment.StatusChanged.Trigger <| Next StartingExperiment
            do! performScansFrom 1 experiment }

        Async.StartWithContinuations (experimentWorkflow,
            (fun () -> 
                experiment.StatusChanged.Trigger <| Next FinishedExperiment
                experiment.StatusChanged.Trigger <| Completed
                resultChannel.RegisterResult ExperimentCompleted),
            (fun exn ->
                experiment.StatusChanged.Trigger <| Error exn
                resultChannel.RegisterResult (ExperimentError exn)),
            (fun exn ->
                experiment.StatusChanged.Trigger <| Next CancelledExperiment
                experiment.StatusChanged.Trigger <| Completed
                resultChannel.RegisterResult ExperimentCancelled),
            cancellationToken)
        
        resultChannel.AwaitResult()

    let stopAfterScan experiment = experiment.StopAfterScanCapability.Cancel()

    let data experiment =
        experiment.SignalUpdated.Publish
        |> Observable.scanInit (Signal.empty experiment.Parameters) (Signal.update experiment.Parameters)
        |> Observable.takeUntilOther (Observable.last <| status experiment)

    let reSignal experiment = data experiment |> Observable.map (Signal.reSignal experiment.Parameters)
    let imSignal experiment = data experiment |> Observable.map (Signal.imSignal experiment.Parameters)
