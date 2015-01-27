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
    { StartingFieldIndex : int
      FinalFieldIndex : int
      RampRateIndex : int
      ReturnToZero : bool
      ConversionTime : int<us>
      Downsampling : uint32 option
      QuadratureDetection : bool 
      NumberOfScans : int }

/// Stores the value of a data point in a CW EPR experiment.
type CwEprDataPoint =
    { // accumulated signal real part
      AccumulatedRe : int
      // accumulated signal imaginary part
      AccumulatedIm : int
      // number of accumulated counts
      Count : int }

/// Stores an accumulated CW EPR signal.
type CwEprData =
    { // mapping from the magnet controller shunt ADC value to a magnetic field
      ShuntAdcToField : int16 -> float<T>
      // a map from magnet controller shunt ADC values to data points 
      Signal : Map<int16, CwEprDataPoint> }

    /// Return a new CwEprData value with the additional sample added to the data.
    member this.AddSampleToData (sample : CwEprSample) =
        { this with 
            Signal =
                // check if a bin with the same magnetic field shunt ADC value already exists 
                let bin =
                    this.Signal
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
                this.Signal
                |> Map.add sample.MagneticFieldShuntAdc newBin }

    /// Creates a real-time updating chart from a CwEprData event stream with a specified
    /// refresh interval.
    static member LiveChart
        (dataUpdateEvent : IEvent<CwEprData>,
         quadratureDetection : bool,
         refreshInterval : TimeSpan,
         [<Optional; DefaultParameterValue("CW EPR experiment")>] title : string,
         [<Optional; DefaultParameterValue("Magnetic field (T)")>] xTitle : string,
         [<Optional; DefaultParameterValue("")>] yTitle : string) =
        
        // chart the real channel data
        let reData =
            (dataUpdateEvent
                |> Event.map (fun data -> data.ReSignal()))
                .Sample(refreshInterval)
                .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe updates on the current synchronization context so that UI can be updated 
        let reChart = LiveChart.FastLine(reData, Name="Real signal", Title=title, XTitle=xTitle, YTitle=yTitle)

        // if quadrature detection is not enabled then only plot the real part of the signal
        if not quadratureDetection then reChart
        else
            // otherwise also plot the imaginary part
            let imData =
                (dataUpdateEvent
                    |> Event.map (fun data -> data.ImSignal()))
                    .Sample(refreshInterval)
                    .ObserveOn(SynchronizationContext.CaptureCurrent())
            let imChart = LiveChart.FastLine(imData, Name="Imaginary signal", Title=title, XTitle=xTitle, YTitle=yTitle)

            // and combine the plots
            Chart.Combine [ reChart ; imChart ]

    /// Returns the real signal part as a sequence of magnetic field and signal pairs.
    member this.ReSignal() =
        this.Signal
        |> Map.toSeq
        |> Seq.map (fun (shuntAdc, dataPoint) -> 
            (this.ShuntAdcToField shuntAdc, (float dataPoint.AccumulatedRe) / (float dataPoint.Count)))
    
    /// Returns the imaginary signal part as a sequence of magnetic field and signal pairs.
    member this.ImSignal() =
        this.Signal
        |> Map.toSeq
        |> Seq.map (fun (shuntAdc, dataPoint) -> 
            (this.ShuntAdcToField shuntAdc, (float dataPoint.AccumulatedIm) / (float dataPoint.Count)))
    
    /// Returns the rows of CSV data for each point in the accumulated data as a sequence of strings.
    member this.CsvData() = seq {
        // first yield the header row
        yield "Shunt ADC, Magnetic field (T), Re signal, Im signal, Sample count"

        // then yield a row for each sample in the acquired signal
        yield! 
            this.Signal
            |> Map.toSeq
            |> Seq.map (fun (shuntAdc, dataPoint) ->
                String.Join(", ", 
                    [ shuntAdc.ToString()
                      (this.ShuntAdcToField shuntAdc).ToString()
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
            magnetController.DeviceParameters.ShuntOffset
        else
            magnetController.DeviceParameters.ShuntOffset
                + magnetController.DeviceParameters.ShuntStep * float (min (abs experiment.StartingFieldIndex) (abs experiment.FinalFieldIndex))
    
    // compute the maximum expected shunt voltage for the given experimental parameters
    let maxShuntVoltage =
        magnetController.DeviceParameters.ShuntOffset 
            + magnetController.DeviceParameters.ShuntStep * float (max (abs experiment.StartingFieldIndex) (abs experiment.FinalFieldIndex))
    
    // choose the PicoScope shunt input range so that is as small as possible without clipping
    let shuntVoltageRange = 
        Range.SmallestInputRangeForVoltage (magnetController.DeviceParameters.ShuntNoise + (maxShuntVoltage - minShuntVoltage) / 2.0)

    // choose the PicoScope shunt input voltage offset so that it is in the middle of the expected shunt voltage range
    let shuntVoltageOffset =
        Math.Round(0.5 * float (minShuntVoltage + maxShuntVoltage), 1) * 1.0<V>

    // create an event which will be triggered by CwEprSample observations for each scan during the experiment.
    let sampleObserved = new Event<CwEprSample>()
    
    // create an empty CwEprData with the shunt ADC -> magnetic field conversion function
    let emptyData =
        { ShuntAdcToField = fun adc ->
            adc
            |> pico.GetAdcCountToVoltageConversion(shuntVoltageRange, shuntVoltageOffset)
            |> magnetController.DeviceParameters.FieldForShuntVoltage
          Signal = Map.empty }
    
    /// Event fires when the status of the experiment worker changes.
    member this.StatusChanged = statusChanged.Publish

    /// Event fires when the experiment worker completes the experiment successfully.
    member this.Completed = completed.Publish

    /// Event fires when an error occurs during the experiment.
    member this.Failed = failed.Publish

    /// Event fires when the experiment is cancelled while it is in progress.
    member this.Canceled = canceled.Publish

    /// Event fires when the experiment is stopped after a scan.
    member this.StoppedAfterScan = stoppedAfterScan.Publish

    /// Event fires after each scan finishes.
    member this.ScanFinished = scanFinished.Publish
    
    /// Event fires when a new sample is added to the data.
    member this.DataUpdated =
        // take all sample observations and add them to the data, starting with the empty CwEprData
        sampleObserved.Publish
        |> Event.scan (fun (data : CwEprData) -> data.AddSampleToData) emptyData

    /// Cancels an experiment which is in progress, specifying whether the magnet controller should return to zero current or pause.
    member this.Cancel returnToZero =
        "CW EPR worker stopping..." |> log.Info
        cancellationCapability.Cancel returnToZero
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    /// Sets a flag indicating that the experiment should stop after the current scan.
    member this.StopAfterScan() =
        "Setting CW EPR worker to stop after scan..." |> log.Info
        stopAfterScanCapability.Cancel()

    /// Initiates an experiment, first ramping the magnet controller to the initial current and then starts ramping and acuqiring data
    /// immediately.
    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    /// Sets the ready-to-start flag so that the experiment will start once prepared at the initial magnet controller current.
    member this.SetReadyToStart() =
        "Setting CW EPR worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    /// Initiates the experiment workflow, ramping to the initial magnet controller current to prepare for the first scan, where it will
    /// wait for the ready-to-start flag to be set.
    member this.Prepare() =
        "CW EPR experiment worker preparing..." |> log.Info
        sprintf "Experiment parameters:\n%A" experiment |> log.Info
        sprintf "Number of scans: %d." experiment.NumberOfScans |> log.Info
        
        // capture the current synchronisation context so that events can be fired on the UI thread or thread pool accordingly
        let syncContext = SynchronizationContext.CaptureCurrent()

        // define the experiment workflow
        let workflow =

            // define a workflow which will perform the n-th scan of the experiment
            let performScan scanNumber =
                async {
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
                              ConversionTime = experiment.ConversionTime
                              Downsampling = experiment.Downsampling
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