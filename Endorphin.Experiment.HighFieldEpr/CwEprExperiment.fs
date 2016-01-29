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

        type Notes = { SampleNotes : string ; ExperimentNotes : string }

        type CwEprExperimentParameters =
            internal { CentreField              : decimal<T>
                       SweepWidth               : decimal<T>
                       FieldSweepDirection      : FieldSweepDirection
                       RampRate                 : decimal<T/s>
                       ConversionTime           : int<ms>
                       QuadratureDetection      : bool
                       NumberOfScans            : int
                       Notes                    : Notes
                       Date                     : DateTime
                       MagnetControllerSettings : TwickenhamSmc.Settings }

        type internal SweepParameters =
            | OnePart of parameters : FieldSweepParameters
            | TwoPart of first : FieldSweepParameters * second : FieldSweepParameters

        type CwEprSample =
            { MagneticFieldShuntAdc : AdcCount
              ReSignalAdc           : AdcCount
              ImSignalAdc           : AdcCount }

        type CwEprSignal =
            { MagneticField     : decimal<T>
              ReSignalIntensity : decimal
              ImSignalIntensity : decimal }

        type ScanPart =
            { ScanNumber       : int 
              CurrentDirection : CurrentDirection
              InitialStepIndex : uint16
              FinalStepIndex   : uint16 }

        type CwEprData = 
            { CompletedScans      : Map<ScanPart, CwEprSample array>
              CompletedScanSignal : CwEprSignal array
              CurrentScan         : (ScanPart * (CwEprSample list)) option }

        type CwEprExperimentStatus =
            | StartingExperiment
            | PerformingExperimentScan of scan : int
            | StoppedAfterScan of scan : int
            | FinishedExperiment
            | CancelledExperiment

            override status.ToString() =
                match status with
                | StartingExperiment         -> "Starting experiment."
                | PerformingExperimentScan n -> sprintf "Performing scan %d." n
                | StoppedAfterScan n         -> sprintf "Stopped after scan %d." n
                | FinishedExperiment         -> "Finished experiment"
                | CancelledExperiment        -> "Cancelled experiment"

        type CwEprExperimentResult =
            | ExperimentCompleted
            | ExperimentError of exn
            | ExperimentCancelled

        type CwEprDataUpdate =
            | SamplesAvailable of samples : CwEprSample seq
            | ScanPartStarted of scanPart : ScanPart
            | ScanPartCompleted

        type CwEprExperiment = 
            internal { Parameters              : CwEprExperimentParameters
                       MagnetController        : MagnetController
                       PicoScope               : PicoScope5000
                       StatusChanged           : NotificationEvent<CwEprExperimentStatus>
                       DataUpdated             : Event<CwEprDataUpdate>
                       StopAfterScanCapability : CancellationCapability }

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

        let scanDuration parameters = (sweepWidth parameters / rampRate parameters)

        let experimentDuration parameters =
            (decimal <| numberOfScans parameters) * (scanDuration parameters)

        let sampleNotes parameters = parameters.Notes.SampleNotes

        let experimentNotes parameters = parameters.Notes.ExperimentNotes

        let magnetControllerSettings parameters =  parameters.MagnetControllerSettings

        let samplesPerPoint parameters = conversionTime parameters / 20<ms>

        // Validation functions

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
        
        // Modification functions

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

        let withConversionTime conversionTime parameters =
            { parameters with ConversionTime = conversionTime }
            |> conversionTimeIsMultipleOfMains

        let withQuadratureDetection quadratureDetection parameters =
            { parameters with QuadratureDetection = quadratureDetection }

        let withNumberOfScans numberOfScans parameters =
            { parameters with NumberOfScans = numberOfScans }
            |> numberOfScansGreaterThanZero

        let withSampleNotes sampleNotes parameters = 
            { parameters with Notes = { parameters.Notes with SampleNotes = sampleNotes } }

        let withExperimentNotes experimentNotes parameters =
            { parameters with Notes = { parameters.Notes with ExperimentNotes = experimentNotes } }

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
              QuadratureDetection      = true
              NumberOfScans            = 1
              Date                     = DateTime.Now
              Notes                    = { SampleNotes = "" ; ExperimentNotes = "" }
              MagnetControllerSettings = magnetControllerSettings }
            |> clipField
            |> digitiseField
            |> clipRampRate
            |> nearestCalibratedRampRate

    module private RampFitting =

        /// Linear ramp function between the specified initial and final values starting at the specified time
        /// and lasting the specified duration. Before the start time, the function takes the given initial
        /// value and after the ramp duration it takes the final value.
        let ramp duration (initial, final, start) = function
            | x when x <= start            -> initial
            | x when x >= start + duration -> final
            | x                            -> initial + (x - start) * (final - initial) / duration

        /// Iteratively fits a linear ramp to the given array of (x, y) points with the specified initial values
        /// for scaling factor a, mean value mu and standard deviation sigma. 
        let fitRamp duration (initial, final, start) (data : (float * float) array) =
            let rampFunc parameters (dys : float array) (_ : System.Collections.Generic.IList<float> []) (data : obj) =
                match parameters with
                | [| initial ; final ; start |] -> 
                    let data' = data :?> (float * float) array
                    data' |> Array.iteri (fun i (x, y) ->
                        let y' = ramp duration (initial, final, start) x
                        dys.[i] <- y - y')

                    0
                | _ -> failwith "Parameter array has unexpected length."

            let parameters = [| initial ; final ; start |]
            let mutable result = mp_result(3)
            MPFit.Solve(mp_func(rampFunc), Array.length data, 3, parameters, null, null, data, &result) |> ignore
            (parameters.[0], parameters.[1], parameters.[2])

            
    module private InstrumentParameters =

        let rec initialFieldIndex parameters = 
            let (currentDirection, stepIndex) =
                Parameters.initialField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (Parameters.magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| finalFieldIndex parameters, stepIndex)

        and finalFieldIndex parameters =
            let (currentDirection, stepIndex) =
                Parameters.finalField parameters
                |> MagnetController.Settings.Convert.magneticFieldToStepIndex (Parameters.magnetControllerSettings parameters)

            if stepIndex <> 0us
            then (currentDirection, stepIndex)
            else (fst <| initialFieldIndex parameters, stepIndex)

        let rampRateIndex parameters =
            Parameters.rampRate parameters
            / (abs (MagnetController.Settings.linearFieldCoefficient <| Parameters.magnetControllerSettings parameters))
            |> (MagnetController.Settings.RampRate.nearestIndex <| Parameters.magnetControllerSettings parameters)

        let sweepParameters parameters =
            let (initialCurrentDirection, initialStepIndex) = initialFieldIndex parameters
            let (finalCurrentDirection,   finalStepIndex  ) = finalFieldIndex   parameters
            let rampRateIndex = rampRateIndex parameters

            if initialCurrentDirection = finalCurrentDirection then
                OnePart <| FieldSweep.Parameters.create 
                    initialCurrentDirection initialStepIndex finalStepIndex rampRateIndex
            else
                let first  = FieldSweep.Parameters.create initialCurrentDirection initialStepIndex 0us rampRateIndex
                let second = FieldSweep.Parameters.create finalCurrentDirection   0us   finalStepIndex rampRateIndex
                TwoPart (first, second)

        let sampleInterval               = Interval.fromMicroseconds 20<us>
        let samplingResolution           = Resolution_14bit
        let downsamplingMode             = Averaged
        let downsamplingRatio parameters = 1000u // downsample data to 20 ms interval to remove mains hum
        let sampleBuffer                 = AveragedBuffer
        let magneticFieldChannel         = ChannelA
        
        let signalChannels parameters =
            if Parameters.quadratureDetection parameters
            then [ ChannelB ; ChannelC ]
            else [ ChannelB ]

        let shuntVoltageRange parameters =
            let settings = Parameters.magnetControllerSettings parameters
            let (initialCurrentDirection, initialStepIndex) = initialFieldIndex parameters
            let (finalCurrentDirection,   finalStepIndex  ) = finalFieldIndex   parameters
            
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
            |> Streaming.Parameters.withDownsamplingRatio (downsamplingRatio parameters)
    
    module Data =
        let private numberOfPoints (sweepWidth : decimal<T>) (rampRate : decimal<T/s>) (conversionTime : int<ms>) = 
            sweepWidth / (rampRate * 0.001M<s> * (decimal (int conversionTime)))
            |> floor |> int

        let private magneticFieldForPoint (initialField : decimal<T>) (rampRate : decimal<T/s>) samplesPerPoint i = // i is zero-based index
            seq { for sample in 0 .. samplesPerPoint - 1 -> initialField + (0.020M<s> * (decimal (sample + i * samplesPerPoint))) * rampRate }
            |> Seq.average

        let private emptyPoints (initialField : decimal<T>) (finalField : decimal<T>) (rampRate : decimal<T/s>) (conversionTime : int<ms>) =
            let samplesPerPoint = conversionTime / 20<ms>
            [| for i in 0 .. numberOfPoints (abs (finalField - initialField)) rampRate conversionTime - 1->
                magneticFieldForPoint initialField rampRate samplesPerPoint i |] 
            |> Array.map (fun field -> { MagneticField = field ; ReSignalIntensity = 0.0M ; ImSignalIntensity = 0.0M })

        let internal empty parameters =
            let settings = Parameters.magnetControllerSettings parameters
            let (initialCurrentDirection, initialIndex) = InstrumentParameters.initialFieldIndex parameters
            let (finalCurrentDirection,   finalIndex  ) = InstrumentParameters.finalFieldIndex parameters
            let rampRate = Parameters.rampRate parameters
            let conversionTime = Parameters.conversionTime parameters
            let emptySignal =
                if initialCurrentDirection = finalCurrentDirection then
                    let initialField = MagnetController.Settings.Convert.stepIndexToMagneticField settings (initialCurrentDirection, initialIndex)
                    let finalField   = MagnetController.Settings.Convert.stepIndexToMagneticField settings (finalCurrentDirection, finalIndex)
                    emptyPoints initialField finalField rampRate conversionTime
                else
                    let initialField = MagnetController.Settings.Convert.stepIndexToMagneticField settings (initialCurrentDirection, initialIndex)
                    let centreField  = MagnetController.Settings.staticField settings
                    let finalField   = MagnetController.Settings.Convert.stepIndexToMagneticField settings (finalCurrentDirection, finalIndex)
                    Array.append (emptyPoints initialField centreField rampRate conversionTime) (emptyPoints centreField finalField rampRate conversionTime) 

            { CompletedScans      = Map.empty
              CompletedScanSignal = emptySignal
              CurrentScan         = None }

        let initialiseScan data scanPart =
            match data.CurrentScan with
            | Some _ -> failwith "Cannot initialise a new scan before the current one is completed."
            | None   -> { data with CurrentScan = Some (scanPart, List.empty) }

        let internal accummulate (data : CwEprData) sample =
            match data.CurrentScan with
            | Some (scan, samples) -> { data with CurrentScan = Some (scan, sample :: samples) }
            | None                 -> failwith "Cannot accumulate data when no scan is in progress."

        let internal accumulateSeq data = Seq.fold accummulate data

        let private scanSignal parameters (scanPart, samples) =
            let shuntAdcToVoltage = float << InstrumentParameters.shuntAdcToVoltage parameters
            let settings = Parameters.magnetControllerSettings parameters
            let iniitalVoltage = float <| decimal (MagnetController.Settings.Convert.stepIndexToShuntVoltage settings scanPart.InitialStepIndex)
            let finalVoltage   = float <| decimal (MagnetController.Settings.Convert.stepIndexToShuntVoltage settings scanPart.FinalStepIndex)
            let duration = float <| decimal (Parameters.scanDuration parameters)
            let samplesPerPoint = Parameters.samplesPerPoint parameters
            let initialField = MagnetController.Settings.Convert.stepIndexToMagneticField settings (scanPart.CurrentDirection, scanPart.InitialStepIndex)
            let finalField   = MagnetController.Settings.Convert.stepIndexToMagneticField settings (scanPart.CurrentDirection, scanPart.FinalStepIndex)
            let sweepWidth   = abs (finalField - initialField)
            let rampRate = Parameters.rampRate parameters
            let conversionTime = Parameters.conversionTime parameters
            let numberOfPoints = numberOfPoints sweepWidth rampRate conversionTime

            let shuntVoltages = 
                List.rev samples
                |> Seq.ofList
                |> Seq.mapi (fun i sample -> (float i * 0.020, shuntAdcToVoltage sample.MagneticFieldShuntAdc))
                |> Array.ofSeq

            let (_, _, rampStart) = RampFitting.fitRamp duration (iniitalVoltage, finalVoltage, 10.0) shuntVoltages
            let skipCount = int (round (rampStart / 0.020)) - 1 |> min (List.length samples)
            let takeCount = (numberOfPoints * samplesPerPoint) |> min (List.length samples - skipCount) |> max 0

            List.rev samples
            |> Seq.ofList
            |> Seq.skip skipCount
            |> Seq.take takeCount
            |> Seq.chunkBySize samplesPerPoint
            |> Seq.mapi (fun i pointSamples ->
                { MagneticField     = magneticFieldForPoint initialField rampRate samplesPerPoint i
                    ReSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ReSignalAdc)
                    ImSignalIntensity = pointSamples |> Seq.sumBy (fun sample -> decimal sample.ImSignalAdc) })

        let completeScan data parameters =
            match data.CurrentScan with
            | Some (scanPart, samples) ->
                let completedScanSignal = 
                    data.CompletedScanSignal
                    |> Seq.ofArray
                    |> Seq.append (scanSignal parameters (scanPart, samples))
                    |> Seq.groupBy (fun signal -> signal.MagneticField)
                    |> Seq.map (fun (magneticField, signals) ->
                        { MagneticField     = magneticField
                          ReSignalIntensity = signals |> Seq.sumBy (fun signal -> signal.ReSignalIntensity)
                          ImSignalIntensity = signals |> Seq.sumBy (fun signal -> signal.ImSignalIntensity) })
                    |> Array.ofSeq

                { CompletedScans      = data.CompletedScans |> Map.add scanPart (List.rev samples |> Array.ofList)
                  CompletedScanSignal = completedScanSignal
                  CurrentScan         = None }
            | None -> failwith "Cannot complete scan when one is not in progress."

        let accumulateScanUpdate parameters data = function
            | SamplesAvailable samples -> accumulateSeq data samples
            | ScanPartStarted scanPart -> initialiseScan data scanPart
            | ScanPartCompleted        -> completeScan data parameters

        let reSignal parameters (data : CwEprData) =
            match data.CurrentScan with
            | Some (scanPart, samples) ->
                data.CompletedScanSignal
                |> Seq.ofArray
                |> Seq.append (scanSignal parameters (scanPart, samples))
                |> Seq.groupBy (fun signal -> signal.MagneticField)
                |> Seq.map (fun (magneticField, signals) -> magneticField, (signals |> Seq.sumBy (fun signal -> signal.ReSignalIntensity)))
            | None ->
                data.CompletedScanSignal
                |> Seq.ofArray
                |> Seq.map (fun signal -> (signal.MagneticField, signal.ReSignalIntensity))

        let imSignal parameters (data : CwEprData) =
            match data.CurrentScan with
            | Some (scanPart, samples) -> 
                data.CompletedScanSignal
                |> Seq.ofArray
                |> Seq.append (scanSignal parameters (scanPart, samples))
                |> Seq.groupBy (fun signal -> signal.MagneticField)
                |> Seq.map (fun (magneticField, signals) -> magneticField, (signals |> Seq.sumBy (fun signal -> signal.ImSignalIntensity)))
            | None ->
                data.CompletedScanSignal
                |> Seq.ofArray
                |> Seq.map (fun signal -> (signal.MagneticField, signal.ImSignalIntensity))

        let signalRows (data : CwEprData) = seq {
            yield "Magnetic field (T), Real signal, Imaginary signal"
            
            yield! data.CompletedScanSignal
            |> Seq.ofArray
            |> Seq.map (fun signal ->
                [ string <| signal.MagneticField
                  string <| signal.ReSignalIntensity
                  string <| signal.ImSignalIntensity ]
                |> String.concat ", " ) }

        let rawDataRows (data : CwEprData) = seq {
            // header row, then samples
            yield "Scan, Current direction, Shunt ADC, Real ADC, Imaginary ADC"
            
            yield! data.CompletedScans
            |> Map.toSeq
            |> Seq.collect (fun (scanPart, samples) ->
                samples
                |> Seq.map (fun sample -> 
                    [ string scanPart.ScanNumber
                      (match scanPart.CurrentDirection with Forward -> "Forward" | Reverse -> "Reverse")
                      string <| sample.MagneticFieldShuntAdc
                      string <| sample.ReSignalAdc
                      string <| sample.ImSignalAdc ]
                    |> String.concat ", ")) }

    let create parameters magnetController picoScope =
        { Parameters              = parameters
          MagnetController        = magnetController
          PicoScope               = picoScope
          StatusChanged           = NotificationEvent<_>()
          DataUpdated             = Event<_>()
          StopAfterScanCapability = new CancellationCapability() }

    let status experiment = 
        experiment.StatusChanged.Publish
        |> Observable.fromNotificationEvent

    let private processSamples acquisition currentDirection experiment =
        let channels = InstrumentParameters.magneticFieldChannel :: InstrumentParameters.signalChannels experiment.Parameters
        let quadratureDetection = Parameters.quadratureDetection experiment.Parameters
        let inputs = channels |> List.map (fun channel -> (channel, InstrumentParameters.sampleBuffer)) |> Array.ofList
        
        let samples (block : AdcCount [][]) =
            let blockLength = block.[0] |> Array.length
            seq { for i in 0 .. blockLength - 1 ->
                    { MagneticFieldShuntAdc = block.[0].[i]
                      ReSignalAdc           = block.[1].[i]
                      ImSignalAdc           = if quadratureDetection then block.[2].[i] else 0s } }
            |> SamplesAvailable

        Streaming.Signal.adcCountsByBlock inputs acquisition
        |> Observable.subscribe (samples >> experiment.DataUpdated.Trigger)

    let private scanPart n streamingParameters fieldSweepParameters experiment = async {
        let acquisition = Streaming.Acquisition.create experiment.PicoScope streamingParameters
        let fieldSweep  = FieldSweep.create experiment.MagnetController fieldSweepParameters
        let currentDirection = fieldSweep |> FieldSweep.parameters |> FieldSweep.Parameters.currentDirection

        let scanPart =
            { ScanNumber       = n
              CurrentDirection = currentDirection
              InitialStepIndex = FieldSweep.Parameters.initialStepIndex fieldSweepParameters 
              FinalStepIndex   = FieldSweep.Parameters.finalStepIndex   fieldSweepParameters }

        experiment.DataUpdated.Trigger (ScanPartStarted scanPart)

        let! waitToPrepare = FieldSweep.waitToPrepare fieldSweep |> Async.StartChild
        let! sweepHandle   = FieldSweep.prepareAsChild fieldSweep
        do! waitToPrepare
        
        use __ = processSamples acquisition currentDirection experiment
        let! waitForStreaming = Streaming.Acquisition.waitToStart acquisition |> Async.StartChild
        let! acquisitionHandle = Streaming.Acquisition.startAsChild acquisition
        do! waitForStreaming
        do! Async.Sleep 10000
        FieldSweep.setReadyToSweep fieldSweep
        
        let! fieldSweepResult = FieldSweep.waitToFinish sweepHandle
        do! Async.Sleep 10000
        do! Streaming.Acquisition.stopAndFinish acquisitionHandle |> Async.Ignore 
        
        experiment.DataUpdated.Trigger ScanPartCompleted }

    let private scan n experiment = async {
        experiment.StatusChanged.Trigger <| Next (PerformingExperimentScan n)
        let sweepParameters     = InstrumentParameters.sweepParameters experiment.Parameters
        let streamingParameters = InstrumentParameters.streamingParameters experiment.Parameters

        match sweepParameters with
        | OnePart fieldSweep -> 
            do! scanPart n streamingParameters fieldSweep experiment
        
        | TwoPart (firstSweep, secondSweep) ->
            do! scanPart n streamingParameters firstSweep experiment
            do! MagnetController.Ramp.waitToReachZero experiment.MagnetController
            do! scanPart n streamingParameters secondSweep experiment }

    let rec private scanLoop n experiment = async {
        do! scan n experiment 
        if n < Parameters.numberOfScans experiment.Parameters then
            if   experiment.StopAfterScanCapability.IsCancellationRequested
            then experiment.StatusChanged.Trigger <| Next (StoppedAfterScan n)
            else do! scanLoop (n + 1) experiment }

    let run experiment cancellationToken =
        let resultChannel = new ResultChannel<_>()

        let experimentWorkflow = async {
            experiment.StatusChanged.Trigger <| Next StartingExperiment
            do! scanLoop 1 experiment }

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
        experiment.DataUpdated.Publish
        |> Observable.scanInit (Data.empty experiment.Parameters) (Data.accumulateScanUpdate experiment.Parameters)
        |> Observable.takeUntilOther (Observable.last <| status experiment)

    let reSignal experiment = data experiment |> Observable.map (Data.reSignal experiment.Parameters)
    let imSignal experiment = data experiment |> Observable.map (Data.imSignal experiment.Parameters)
