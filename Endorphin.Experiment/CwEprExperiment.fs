namespace Endorphin.Experiment

open Endorphin.Core
open Endorphin.Core.Units
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open System.Threading
open log4net
open System.Collections.Generic

type CwEprExperiment =
    { startingFieldIndex : int
      finalFieldIndex : int
      rampRateIndex : int
      returnToZero : bool
      conversionTime : int<ms>
      downsampling : uint32 option
      quadratureDetection : bool 
      numberOfScans : int }

type CwEprDataPoint =
    { accumulatedX : int
      accumulatedY : int
      count : int }

type CwEprData =
    { shuntAdcToField : int16 -> float<T>
      signal : Map<int16, CwEprDataPoint> }

type CwEprExperimentStatus =
    | StartingExperiment
    | PreparingExperimentScan of scan : int
    | StartedExperimentScan of scan : int
    | FinishedExperimentScan of scan : int
    | FinishedExperiment
    | FailedExperiment
    | CanceledExperiment
    | StoppedAfterScan of scan : int

type CwEprExperimentWorker(experiment, magnetController : MagnetController, pico : PicoScope5000) =
    static let log = LogManager.GetLogger typeof<CwEprExperimentWorker>

    let statusChanged = new Event<CwEprExperimentStatus>()
    let success = new Event<unit>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let stoppedAfterScan = new Event<int>()
    let scanFinished = new Event<int>()
    let dataUpdated = new Event<CwEprData>()

    let readyToStart = new ManualResetEvent(false)
    let cancellationCapability = new CancellationCapability<RampCancellationOptions>()
    let stopAfterScanCapability = new CancellationCapability()
        
    let minShuntVoltage =
        if sign experiment.startingFieldIndex <> sign experiment.finalFieldIndex then
            magnetController.DeviceParameters.shuntOffset
        else
            magnetController.DeviceParameters.shuntOffset + 
                magnetController.DeviceParameters.ShuntStep * float (min (abs experiment.startingFieldIndex) (abs experiment.finalFieldIndex))
    
    let maxShuntVoltage =
        magnetController.DeviceParameters.shuntOffset + 
            magnetController.DeviceParameters.ShuntStep * float (max (abs experiment.startingFieldIndex) (abs experiment.finalFieldIndex))

    let shuntVoltageRange = (0.5 * (maxShuntVoltage - minShuntVoltage)).SmallestInputRangeForVoltage()
    let shuntVoltageOffset = 0.5 * (minShuntVoltage + maxShuntVoltage)

    let processSample (data : CwEprData) (sample : CwEprSample) =
        { data with 
            signal = 
                data.signal
                |> Map.updateKey sample.magneticFieldShuntAdc (function
                    | None ->
                        { accumulatedX = int sample.xSignalAdc
                          accumulatedY = int sample.ySignalAdc
                          count = 1 }
                    | Some value ->
                        { accumulatedX = value.accumulatedX + int sample.xSignalAdc
                          accumulatedY = value.accumulatedY + int sample.ySignalAdc
                          count = value.count + 1 }) }

    let sample = new Event<CwEprSample>()
    let emptyData =
        { shuntAdcToField = fun adc -> 
            adc 
            |> pico.GetAdcCountToVoltageConversion(shuntVoltageRange, shuntVoltageOffset)
            |> magnetController.DeviceParameters.FieldForShuntVoltage
          signal = Map.empty }

    member this.StatusChanged = statusChanged.Publish
    member this.Success = success.Publish
    member this.Error = error.Publish
    member this.Canceled = canceled.Publish
    member this.StoppedAfterScan = stoppedAfterScan.Publish
    member this.ScanFinished = scanFinished.Publish
    
    member this.DataUpdated =
        sample.Publish
        |> Event.scan processSample emptyData

    member this.Cancel returnToZero =
        "CW EPR worker stopping..." |> log.Info
        cancellationCapability.Cancel returnToZero
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting CW EPR worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        "CW EPR experiment worker preparing..." |> log.Info
        sprintf "Experiment parameters:\n%A" experiment |> log.Info
        sprintf "Number of scans: %d." experiment.numberOfScans |> log.Info

        let syncContext = SynchronizationContext.CaptureCurrent()

        let workflow =
            let performScan scanNumber =
                async {
                    sprintf "CW EPR experiment performing scan (%d of %d)." scanNumber (experiment.numberOfScans) |> log.Info
                    syncContext.RaiseEvent statusChanged (PreparingExperimentScan scanNumber)
                    
                    let scanWorker = 
                        new CwEprScanWorker(magnetController, pico,
                            { startingFieldIndex = experiment.startingFieldIndex
                              finalFieldIndex  = experiment.finalFieldIndex
                              rampRateIndex = experiment.rampRateIndex
                              returnToZero = experiment.returnToZero
                              conversionTime = experiment.conversionTime
                              downsampling = experiment.downsampling
                              quadratureDetection = experiment.quadratureDetection
                              shuntVoltageRange = shuntVoltageRange
                              shuntVoltageOffset = shuntVoltageOffset })

                    use! __ = Async.OnCancel (fun () -> 
                        sprintf "CW EPR experiment cancelling scan (%d of %d)." scanNumber (experiment.numberOfScans) |> log.Info
                        scanWorker.Cancel cancellationCapability.Options)

                    let! waitForScanCompleted =
                        scanWorker.Success
                        |> Async.AwaitEvent
                        |> Async.StartChild
                   
                    scanWorker.Sample
                    |> Event.add (fun scanSample -> syncContext.RaiseEvent sample scanSample)
                    
                    scanWorker.StatusChanged
                    |> Event.filter ((=) Scanning)
                    |> Event.add (fun _ -> syncContext.RaiseEvent statusChanged (StartedExperimentScan scanNumber))
                    
                    scanWorker.PrepareAndStart()
                    do! waitForScanCompleted
                    
                    sprintf "CW EPR experiment finished scan (%d of %d)." scanNumber (experiment.numberOfScans) |> log.Info
                    syncContext.RaiseEvent statusChanged (FinishedExperimentScan scanNumber)
                    syncContext.RaiseEvent scanFinished scanNumber }

            let rec scanLoop currentScanNumber = async {
                do! performScan currentScanNumber
                    
                if stopAfterScanCapability.IsCancellationRequested then
                    sprintf "CW EPR experiment stopping after scan (%d of %d)." currentScanNumber (experiment.numberOfScans) |> log.Info
                    syncContext.RaiseEvent statusChanged (StoppedAfterScan currentScanNumber)
                    syncContext.RaiseEvent stoppedAfterScan currentScanNumber
                    
                if currentScanNumber < experiment.numberOfScans then
                    do! scanLoop (currentScanNumber + 1) }

            async {
                use! __ = Async.OnCancel(fun () -> 
                    "CW EPR experiment cancelled." |> log.Info
                    syncContext.RaiseEvent statusChanged CanceledExperiment)

                "CW EPR experiment starting." |> log.Info
                syncContext.RaiseEvent statusChanged StartingExperiment
                
                do! scanLoop 1

                "CW EPR experiment finished successfully." |> log.Info
                syncContext.RaiseEvent statusChanged FinishedExperiment }

        "Starting experiment workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> syncContext.RaiseEvent success ()),
            (fun exn ->
                log.Error (sprintf "CW EPR experiment failed due to error: %A." exn, exn)
                syncContext.RaiseEvent statusChanged FailedExperiment
                syncContext.RaiseEvent error exn),
            (fun exn -> syncContext.RaiseEvent canceled exn),
            cancellationCapability.Token)