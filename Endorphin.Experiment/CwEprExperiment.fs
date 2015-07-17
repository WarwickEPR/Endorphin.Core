namespace Endorphin.Experiment

open Endorphin.Core
open Endorphin.Core.Units
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open log4net
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open System.Threading

/// Defines the parameters for a CW EPR experiment.
type CwEprExperiment =
    { /// The signed index for the starting magnet controller field in digital magnet contorller steps.
      StartingFieldIndex : int
      /// The signed index for the final magnet controller field in digital magnet controller steps.
      FinalFieldIndex : int
      /// The index of the calibrated magnet controller ramp rate to be used during the experiment.
      RampRateIndex : int
      /// A flag indicating whether the magnet controller should return to zero current once the experiment is completed.
      ReturnToZero : bool
      /// The time interval between PicoScope samples used to sample the magnetic field and signal during the experiment.
      SampleInterval : int<us>
      /// An optional downsampling ratio to be used to average the magnetic field and signal channels. Set None to acquire
      /// all samples.
      DownsamplingRatio : uint32 option
      /// A flag indicating whether the quadrature channel should be sampled during the experiment.
      QuadratureDetection : bool
      /// The number of scans to be performed during the experiment
      NumberOfScans : int }

/// Stores the value of an accumulated data point in a CW EPR experiment.
type CwEprDataPointSignal =
    { /// The accumulated real signal channel ADC value.
      AccumulatedRe : int
      /// The accumulated imaginary signal channnel ADC value.
      AccumulatedIm : int
      /// The number of accumulated counts.
      Count : int }

/// Stores an accumulated CW EPR signal.
type CwEprData =
    { /// Conversion function from the magnet controller shunt ADC value to magnetic field in Tesla.
      ShuntAdcToField : int16 -> float<T>
      /// A map from magnet controller shunt ADC values to the corresponding accumulated signal accumulated values. Note
      /// that the sign of shunt ADC values should be flipped for samples acquired in reverse magnet controller current
      /// polarity.
      Signal : Map<int16, CwEprDataPointSignal> }

    /// Return a new CwEprData value with the additional sample added to the data.
    member data.AddSampleToData (sample : CwEprSample) =
        { data with 
            Signal =
                // check if a bin with the same magnetic field shunt ADC value already exists 
                let bin =
                    data.Signal
                    |> Map.tryFind sample.MagneticFieldShuntAdc

                // create the new bin
                let newBin = 
                    match bin with
                    | None ->
                        // if no bin already exists then create a bin with a single sample count
                        // and the sample real and imaginary signal values
                        { AccumulatedRe = int sample.ReSignalAdc
                          AccumulatedIm = int sample.ImSignalAdc
                          Count = 1 }
                    | Some value ->
                        // otherwise increment the counter and accumulate the singals
                        { AccumulatedRe = value.AccumulatedRe + int sample.ReSignalAdc
                          AccumulatedIm = value.AccumulatedIm + int sample.ImSignalAdc
                          Count = value.Count + 1 }
                
                // add or replace the bin
                data.Signal
                |> Map.add sample.MagneticFieldShuntAdc newBin }

    /// Returns the real signal part as a sequence of magnetic field and signal pairs.
    member data.ReSignal() =
        data.Signal
        |> Map.toSeq
        |> Seq.map (fun (shuntAdc, dataPoint) -> 
            (data.ShuntAdcToField shuntAdc, (float dataPoint.AccumulatedRe) / (float dataPoint.Count)))
    
    /// Returns the imaginary signal part as a sequence of magnetic field and signal pairs.
    member data.ImSignal() =
        data.Signal
        |> Map.toSeq
        |> Seq.map (fun (shuntAdc, dataPoint) -> 
            (data.ShuntAdcToField shuntAdc, (float dataPoint.AccumulatedIm) / (float dataPoint.Count)))
    
    /// Returns the rows of CSV data for each point in the accumulated data as a sequence of strings.
    member data.CsvRows() = seq {
        // first yield the header row
        yield "Shunt ADC, Magnetic field (T), Re signal, Im signal, Sample count"

        // then yield a row for each sample in the acquired signal
        yield! 
            data.Signal
            |> Map.toSeq
            |> Seq.map (fun (shuntAdc, dataPoint) ->
                String.Join(", ", // join the required 
                    [ shuntAdc.ToString()
                      (data.ShuntAdcToField shuntAdc).ToString()
                      ((float dataPoint.AccumulatedRe) / (float dataPoint.Count)).ToString()
                      ((float dataPoint.AccumulatedIm) / (float dataPoint.Count)).ToString()
                      (dataPoint.Count).ToString() ]
                    |> List.toSeq)) }

/// Indicates the status of a CwEprExperimentWorker
type CwEprExperimentStatus =
    /// Indicates that the worker is starting the experiment.
    | StartingExperiment
    /// Indicates that the worker is preparing for the specfied scan number.
    | PreparingExperimentScan of scan : int
    /// Indicates that the worker has started the specified scan number.
    | StartedExperimentScan of scan : int
    /// Indicates that the worker has finished the specified number of scans.
    | FinishedExperimentScan of scan : int
    /// Indicates that the worker has succesfully finished the experiment.
    | FinishedExperiment
    /// Indicates that the experiment failed due to an error.
    | FailedExperiment of exn : exn
    /// Indicates taht the experiment was canceled while it was in progerss.
    | CanceledExperiment of exn : OperationCanceledException
    /// Indicates that the experiment was stopped after the specified number of scans but completed succesfully.
    | StoppedAfterScan of scan : int

    /// Returns a string describing the status.
    member status.MessageString() =
        match status with
        | StartingExperiment -> "starting"
        | PreparingExperimentScan n -> sprintf "preparing scan %d" n
        | StartedExperimentScan n -> sprintf "performing scan %d" n
        | FinishedExperimentScan n -> sprintf "finished scan %d" n
        | FinishedExperiment -> "finished"
        | FailedExperiment _ -> "failed"
        | CanceledExperiment _ -> "canceled"
        | StoppedAfterScan n -> sprintf "stopped after scan %d" n

/// Performs a CW EPR experiment with the specified experiment parameters using the provided magnet controller and PicoScope.
type CwEprExperimentWorker(experiment : CwEprExperiment, magnetController : MagnetController, pico : PicoScope5000) =
    static let log = LogManager.GetLogger typeof<CwEprExperimentWorker> // logger

    // events
    let statusChanged = new Event<CwEprExperimentStatus>()
    let stoppedAfterScan = new Event<int>()
    let scanFinished = new Event<int>()
    let sampleObserved = new Event<CwEprSample>() // triggered by CwEprSample observations for each scan during the experiment.
    
    // create a handle which is used to indicate whether the scan is ready to start once it is prepared
    let readyToStart = new ManualResetEvent(false)
    // create a cancellation capability which provides the cancellation token for the experiment workflow
    let cancellationCapability = new CancellationCapability<RampCancellationOptions>()
    // create a cancellation capability which will be checked after each scan to see if the experiment
    // should be stopped after the current scan
    let stopAfterScanCapability = new CancellationCapability()
    
    // compute the minimum expected shunt voltage for the given experimental parameters
    let minShuntVoltage =
        if sign experiment.StartingFieldIndex <> sign experiment.FinalFieldIndex then
            magnetController.MagnetControllerParameters.ShuntOffset
        else
            magnetController.MagnetControllerParameters.ShuntOffset
                + magnetController.MagnetControllerParameters.ShuntStep * float (min (abs experiment.StartingFieldIndex) (abs experiment.FinalFieldIndex))
    
    // compute the maximum expected shunt voltage for the given experimental parameters
    let maxShuntVoltage =
        magnetController.MagnetControllerParameters.ShuntOffset 
            + magnetController.MagnetControllerParameters.ShuntStep * float (max (abs experiment.StartingFieldIndex) (abs experiment.FinalFieldIndex))
    
    let minRangeForOffsetVoltage =
        function
        | v when abs v <= 0.25<V> -> Range._100mV
        | v when abs v <= 2.5<V>  -> Range._500mV
        | v when abs v <= 20.0<V> -> Range._5V
        | v                       -> failwithf "Required voltage offset out of bounds: %f." (float v)

    // choose the PicoScope shunt input voltage offset so that it is in the middle of the expected shunt voltage range
    let shuntVoltageOffset =
        - Math.Round(0.5 * float (minShuntVoltage + maxShuntVoltage), 1) * 1.0<V>
        
    // choose the PicoScope shunt input range so that is as small as possible without clipping
    let shuntVoltageRange = 
        Range.SmallestInputRangeForVoltage (magnetController.MagnetControllerParameters.ShuntNoise + (maxShuntVoltage - minShuntVoltage) / 2.0)
        |> max (minRangeForOffsetVoltage shuntVoltageOffset)

    // create an empty CwEprData with the shunt ADC -> magnetic field conversion function
    let emptyData =
        { ShuntAdcToField = fun adc ->
            // note that the ADC values are multiplied by a negative sign if they are aquired with Reverse current polarity
            adc
            |> pico.GetAdcCountToVoltageConversion(shuntVoltageRange, shuntVoltageOffset)
            |> magnetController.MagnetControllerParameters.FieldForShuntVoltage Forward
          Signal = Map.empty }

    // Returns the specified experiment parameters.
    member __.Experiment = experiment
    
    /// Event fires when the status of the experiment worker changes. Events are fired on the System.Threading.SynchronizationContext 
    /// which instantiates the worker. 
    member __.StatusChanged =
        Observable.CreateFromStatusEvent(
            statusChanged.Publish,
            // fire OnCompleted when the experiment finishes or is stopped after some number of scans
            (fun status ->
                match status with
                | FinishedExperiment | StoppedAfterScan _ -> true
                | _ -> false), 
            // fire OnError when the experiment fails or is canceled
            (fun status ->
                match status with
                | FailedExperiment exn -> Some exn
                | CanceledExperiment exn -> Some (exn :> exn)
                | _ -> None))

    /// Event fires when a new sample is added to the data. Events are fired on the System.Threading.SynchronizationContext which
    /// instantiates the worker. 
    member experimentWorker.DataUpdated =
        // take all sample observations and add them to the data, starting with the empty CwEprData
       (sampleObserved.Publish
        |> Observable.scan (fun (data : CwEprData) -> data.AddSampleToData) emptyData)
            .TakeUntil(experimentWorker.StatusChanged.LastAsync())

    /// Cancels an experiment which is in progress, specifying whether the magnet controller should return to zero current or pause.
    member __.Cancel returnToZero =
        "CW EPR worker stopping..." |> log.Info
        cancellationCapability.Cancel returnToZero
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    /// Sets a flag indicating that the experiment should stop after the current scan.
    member __.StopAfterScan() =
        "Setting CW EPR worker to stop after scan..." |> log.Info
        stopAfterScanCapability.Cancel()

    /// Initiates an experiment, first ramping the magnet controller to the initial current and then starts ramping and acuqiring data
    /// immediately.
    member experimentWorker.PrepareAndStart() =
        experimentWorker.SetReadyToStart()
        experimentWorker.Prepare()

    /// Sets the ready-to-start flag so that the experiment will start once prepared at the initial magnet controller current.
    member __.SetReadyToStart() =
        "Setting CW EPR worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    /// Initiates the experiment workflow, ramping to the initial magnet controller current to prepare for the first scan, where it will
    /// wait for the ready-to-start flag to be set.
    member __.Prepare() =
        "CW EPR experiment worker preparing..." |> log.Info
        sprintf "Experiment parameters:\n%A" experiment |> log.Info
        sprintf "Number of scans: %d." experiment.NumberOfScans |> log.Info
        
        // define the experiment workflow
        let workflow =

            // define a workflow which will perform the n-th scan of the experiment
            let performScan scanNumber = async {
                sprintf "CW EPR experiment performing scan (%d of %d)." scanNumber (experiment.NumberOfScans) |> log.Info
                // fire an event indicating that the experiment worker is preparing for the n-th scan
                statusChanged.Trigger (PreparingExperimentScan scanNumber)
                    
                // create a new scan worker with the experiment parameters which will perform the scan
                let scanWorker = 
                    new CwEprScanWorker(magnetController, pico,
                        { StartingFieldIndex = experiment.StartingFieldIndex
                          FinalFieldIndex  = experiment.FinalFieldIndex
                          RampRateIndex = experiment.RampRateIndex
                          ReturnToZero = experiment.ReturnToZero && (scanNumber = experiment.NumberOfScans)
                          SampleInterval = experiment.SampleInterval
                          DownsamplingRatio = experiment.DownsamplingRatio
                          QuadratureDetection = experiment.QuadratureDetection
                          ShuntVoltageRange = shuntVoltageRange
                          ShuntVoltageOffset = shuntVoltageOffset })

                // if the experiment is cancelled within this scope then cancel the scan workflow
                use! __ = Async.OnCancel (fun () -> 
                    sprintf "CW EPR experiment cancelling scan (%d of %d)." scanNumber experiment.NumberOfScans |> log.Info
                    scanWorker.Cancel cancellationCapability.Options)

                // start a child workflow which will wait for the scan to complete
                let! waitForScanCompleted =
                    scanWorker.StatusChanged.LastAsync()
                    |> Async.AwaitObservable
                    |> Async.StartChild
                   
                // at every sample obersvation during the scan, fire the experiment's sampleObserved event
                scanWorker.SampleObserved
                |> Observable.add (fun scanSample -> sampleObserved.Trigger scanSample)
                    
                // listen for the scan worker to indicate that it has started performing the scan and fire a
                // status-changed event indicating that the n-th scan is in progress
                scanWorker.StatusChanged
                |> Observable.filter ((=) Scanning)
                |> Observable.add (fun _ -> statusChanged.Trigger (StartedExperimentScan scanNumber))

                // prepare and start the scan worker
                scanWorker.PrepareAndStart()
                // and wait for the scan to finish, checking the result
                let! result = waitForScanCompleted
                if result <> FinishedScan then
                    let message = sprintf "Scan (%d of %d) failed with status %A." scanNumber experiment.NumberOfScans result
                    let exn = new Exception(message)
                    log.Error(message, exn)

                sprintf "CW EPR experiment finished scan (%d of %d)." scanNumber experiment.NumberOfScans |> log.Info
                // fire events indicating that the experiment has finished the n-th scan
                statusChanged.Trigger (FinishedExperimentScan scanNumber)
                scanFinished.Trigger scanNumber }
            
            // define a recursive workflow which will loop over all the scans
            let rec scanLoop currentScanNumber = async {
                // perform the current scan
                do! performScan currentScanNumber
                    
                // check if there are more scans in the experiment
                if currentScanNumber < experiment.NumberOfScans then
                    // if the stop-after-scan flag is set, then stop the experiment
                    if stopAfterScanCapability.IsCancellationRequested then
                        sprintf "CW EPR experiment stopping after scan (%d of %d)." currentScanNumber (experiment.NumberOfScans) |> log.Info
                        statusChanged.Trigger (StoppedAfterScan currentScanNumber)
                        stoppedAfterScan.Trigger currentScanNumber
                    // otherwise, proceed with the next scan
                    else 
                        do! scanLoop (currentScanNumber + 1) }

            // now use the workflows defined above to define the complete experiment workflow
            async {
                "CW EPR experiment starting." |> log.Info
                // raise an event indicating that the experiment is starting
                statusChanged.Trigger StartingExperiment
                
                // perform all scans, starting from the first
                do! scanLoop 1 }
    
        // start the scan workflow with the specified continuations for success, failure and cancellation and with
        // the provided cancellation token
        "Starting experiment workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> 
                "CW EPR experiment finished successfully." |> log.Info
                statusChanged.Trigger FinishedExperiment),
            (fun exn ->
                log.Error (sprintf "CW EPR experiment failed due to error: %A." exn, exn)
                statusChanged.Trigger (FailedExperiment exn)),
            (fun exn ->
                "CW EPR experiment cancelled." |> log.Info
                statusChanged.Trigger (CanceledExperiment exn)),
            cancellationCapability.Token)