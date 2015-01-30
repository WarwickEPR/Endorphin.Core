namespace Endorphin.Experiment

open Endorphin.Core
open Endorphin.Core.CSharpInterop
open Endorphin.Core.Units
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open FSharp.Charting
open FSharp.Charting.ChartTypes
open log4net
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.IO
open System.Reactive.Linq
open System.Threading
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices

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

/// Defines the possible states of a CW EPR experiment worker.
type CwEprExperimentStatus =
    | StartingExperiment
    | PreparingExperimentScan of scan : int
    | StartedExperimentScan of scan : int
    | FinishedExperimentScan of scan : int
    | FinishedExperiment
    | FailedExperiment
    | CanceledExperiment
    | StoppedAfterScan of scan : int

/// Performs a CW EPR experiment with the specified experiment parameters using the provided magnet controller and PicoScope.
type CwEprExperimentWorker(experiment : CwEprExperiment, magnetController : MagnetController, pico : PicoScope5000) =
    static let log = LogManager.GetLogger typeof<CwEprExperimentWorker> // logger

    // events
    let statusChanged = new Event<CwEprExperimentStatus>()
    let completed = new Event<unit>()
    let failed = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let stoppedAfterScan = new Event<int>()
    let scanFinished = new Event<int>()

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
    
    // choose the PicoScope shunt input range so that is as small as possible without clipping
    let shuntVoltageRange = 
        Range.SmallestInputRangeForVoltage (magnetController.MagnetControllerParameters.ShuntNoise + (maxShuntVoltage - minShuntVoltage) / 2.0)

    // choose the PicoScope shunt input voltage offset so that it is in the middle of the expected shunt voltage range
    let shuntVoltageOffset =
        Math.Round(0.5 * float (minShuntVoltage + maxShuntVoltage), 1) * 1.0<V>

    // create an event which will be triggered by CwEprSample observations for each scan during the experiment.
    let sampleObserved = new Event<CwEprSample>()
    
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
    
    /// Event fires when the status of the experiment worker changes.
    member __.StatusChanged = statusChanged.Publish

    /// Event fires when the experiment worker completes the experiment successfully.
    member __.Completed = completed.Publish

    /// Event fires when an error occurs during the experiment.
    member __.Failed = failed.Publish

    /// Event fires when the experiment is cancelled while it is in progress.
    member __.Canceled = canceled.Publish

    /// Event fires when the experiment is stopped after a scan.
    member __.StoppedAfterScan = stoppedAfterScan.Publish

    /// Event fires after each scan finishes.
    member __.ScanFinished = scanFinished.Publish
    
    /// Event fires when a new sample is added to the data.
    member __.DataUpdated =
        // take all sample observations and add them to the data, starting with the empty CwEprData
        sampleObserved.Publish
        |> Event.scan (fun (data : CwEprData) -> data.AddSampleToData) emptyData

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
        
        // capture the current synchronisation context so that events can be fired on the UI thread or thread pool accordingly
        let syncContext = SynchronizationContext.CaptureCurrent()

        // define the experiment workflow
        let workflow =

            // define a workflow which will perform the n-th scan of the experiment
            let performScan scanNumber = async {
                sprintf "CW EPR experiment performing scan (%d of %d)." scanNumber (experiment.NumberOfScans) |> log.Info
                // fire an event indicating that the experiment worker is preparing for the n-th scan
                syncContext.RaiseEvent statusChanged (PreparingExperimentScan scanNumber)
                    
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
                    sprintf "CW EPR experiment cancelling scan (%d of %d)." scanNumber (experiment.NumberOfScans) |> log.Info
                    scanWorker.Cancel cancellationCapability.Options)

                // start a child workflow which will wait for the scan to complete
                let! waitForScanCompleted =
                    scanWorker.Completed
                    |> Async.AwaitEvent
                    |> Async.StartChild
                   
                // at every sample obersvation during the scan, fire the experiment's sampleObserved event
                scanWorker.SampleObserved
                |> Event.add (fun scanSample -> syncContext.RaiseEvent sampleObserved scanSample)
                    
                // listen for the scan worker to indicate that it has started performing the scan and fire a
                // status-changed event indicating that the n-th scan is in progress
                scanWorker.StatusChanged
                |> Event.filter ((=) Scanning)
                |> Event.add (fun _ -> syncContext.RaiseEvent statusChanged (StartedExperimentScan scanNumber))

                // prepare and start the scan worker
                scanWorker.PrepareAndStart()
                // and wait for the scan to finish
                do! waitForScanCompleted
                    
                sprintf "CW EPR experiment finished scan (%d of %d)." scanNumber experiment.NumberOfScans |> log.Info
                // fire events indicating that the experiment has finished the n-th scan
                syncContext.RaiseEvent statusChanged (FinishedExperimentScan scanNumber)
                syncContext.RaiseEvent scanFinished scanNumber }
            
            // define a recursive workflow which will loop over all the scans
            let rec scanLoop currentScanNumber = async {
                // perform the current scan
                do! performScan currentScanNumber
                    
                // check if there are more scans in the experiment
                if currentScanNumber < experiment.NumberOfScans then
                    // if the stop-after-scan flag is set, then stop the experiment
                    if stopAfterScanCapability.IsCancellationRequested then
                        sprintf "CW EPR experiment stopping after scan (%d of %d)." currentScanNumber (experiment.NumberOfScans) |> log.Info
                        syncContext.RaiseEvent statusChanged (StoppedAfterScan currentScanNumber)
                        syncContext.RaiseEvent stoppedAfterScan currentScanNumber
                    // otherwise, proceed with the next scan
                    else 
                        do! scanLoop (currentScanNumber + 1) }

            // now use the workflows defined above to define the complete experiment workflow
            async {
                // if cancellation occurs
                use! __ = Async.OnCancel(fun () -> 
                    "CW EPR experiment cancelled." |> log.Info
                    // raise an event indicating that the experiment status has changed to canceled
                    syncContext.RaiseEvent statusChanged CanceledExperiment)

                "CW EPR experiment starting." |> log.Info
                // raise an event indicating that the experiment is starting
                syncContext.RaiseEvent statusChanged StartingExperiment
                
                // perform all scans, starting from the first
                do! scanLoop 1

                "CW EPR experiment finished successfully." |> log.Info
                // raise an event indicating that the experiment has finished
                syncContext.RaiseEvent statusChanged FinishedExperiment }
    
        // start the scan workflow with the specified continuations for success, failure and cancellation and with
        // the provided cancellation token
        "Starting experiment workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> syncContext.RaiseEvent completed ()),
            (fun exn ->
                log.Error (sprintf "CW EPR experiment failed due to error: %A." exn, exn)
                syncContext.RaiseEvent statusChanged FailedExperiment
                syncContext.RaiseEvent failed exn),
            (fun exn -> syncContext.RaiseEvent canceled exn),
            cancellationCapability.Token)