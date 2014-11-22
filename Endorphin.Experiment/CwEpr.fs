namespace Endorphin.Experiment

open Endorphin.Core.Units
open Endorphin.Core.Utils
open Endorphin.Core.ObservableExtensions
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open System.Threading
open log4net

type CwEprExperiment =
    { startingFieldIndex : int
      finalFieldIndex : int
      rampRateIndex : int
      returnToZero : bool
      conversionTime : int<ms>
      quadratureDetection : bool
      numberOfScans : int }

type CwEprDataPoint =
    { accumulatedValue : int
      count : int
      magneticField : float<T> }

type CwEprExperimentStatus =
    | PreparingFieldForScan of scan : int
    | PreparingAcquisitionForScan of scan : int
    | StartedScan of scan : int
    | FinishedScan of scan : int
    | FinishedExperiment
    | StoppedExperiment

type CwEprCancellationCapability() =
    static let log = LogManager.GetLogger typeof<CwEprCancellationCapability>

    let cancellationRequested = new Event<bool>()

    let cancellationCapability = new CancellationTokenSource()
    let stopAfterScanCapability = new CancellationTokenSource()

    let returnToZeroAfterCancellation = ref false
    let started = ref false
    let cancelling = ref false

    member this.ReturnToZero = !returnToZeroAfterCancellation
    member this.IsCancellationRequested = cancellationCapability.IsCancellationRequested
    member this.IsStopAfterScanRequested = stopAfterScanCapability.IsCancellationRequested
    member this.CancellationRequested = cancellationRequested.Publish

    member this.Cancel(returnToZero) =
        if !cancelling then
            failwith "CW EPR experiment is already stopping."
        if not !started then
            failwith "Cannot cancel CW EPR experiment before it has been initiated."
        "Cancelling CW EPR experiment token." |> log.Info
        cancelling := true
        returnToZeroAfterCancellation := returnToZero
        cancellationCapability.Cancel()
        stopAfterScanCapability.Cancel()
        cancellationRequested.Trigger(returnToZero)
    
    member this.StopAfterScan(returnToZero) =
        if !cancelling then
            failwith "Cannot request stop after scan: CW EPR experiment is already stopping."
        if not !started then
            failwith "Cannot stop CW EPR experiment after current scan as it has not been started."
        returnToZeroAfterCancellation := returnToZero
        stopAfterScanCapability.Cancel()

    member this.StartExperimentUsingToken() =
        if !started then
            failwith "Cannot reuse CW EPR experiment token."
        "Started using experiment token." |> log.Info
        started := true 
        cancellationCapability.Token

    member this.StartScanUsingToken() =
        if not !started then
            failwith "Cannot use scan token before CW EPR experiment has been started."
        "Started using scan token." |> log.Info
        stopAfterScanCapability.Token

type CwEprWorker(experiment, magnetController : MagnetController, pico : PicoScope5000) =
    static let log = LogManager.GetLogger typeof<CwEprWorker>

    let statusChanged = new Event<CwEprExperimentStatus>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let completed = new Event<unit>()
            
    let readyToStart = new ManualResetEvent(false)
    let cancellationCapability = new CwEprCancellationCapability()

    let ramp : Ramp = {
        startingFieldIndex = experiment.startingFieldIndex
        finalFieldIndex = experiment.finalFieldIndex
        rampRateIndex = experiment.rampRateIndex
        returnToZero = experiment.returnToZero }
    
    let rampVoltageRange = magnetController.ShuntStep * float (abs (ramp.startingFieldIndex - ramp.finalFieldIndex))
    let rampVoltageOffset = magnetController.ShuntStep * 0.5 * (float (ramp.startingFieldIndex + ramp.finalFieldIndex))

    let magneticFieldInputSettings : InputSettings = { 
        coupling = Coupling.DC
        range = (0.5 * rampVoltageRange).SmallestInputRangeForVoltage()
        analogueOffset = rampVoltageOffset
        bandwidthLimit = BandwidthLimit._20MHz }

    let lockinSignalInputSettings : InputSettings = { 
        coupling = Coupling.DC
        range = Range._10V
        analogueOffset = 0.0<V>
        bandwidthLimit = BandwidthLimit._20MHz }

    let magneticFieldChannel = (Channel.A, magneticFieldInputSettings)
    let signalChannels =
        if experiment.quadratureDetection then
            [ (Channel.B, lockinSignalInputSettings) ; (Channel.C, lockinSignalInputSettings) ]
        else
            [ (Channel.B, lockinSignalInputSettings) ]

    let magneticFieldStream = ChannelData(Channel.A, Downsampling.None)
    let signalStreams =
        if experiment.quadratureDetection then
            [ ChannelData(Channel.B, Downsampling.None) ; ChannelData(Channel.C, Downsampling.None) ]
        else
            [ ChannelData(Channel.B, Downsampling.None) ]

    let stream : StreamingParmaeters = {  
        sampleInterval = experiment.conversionTime * 1000000<ns/ms>
        downsamplingRatio = 1u
        streamStop = ManualStop
        triggerSettings = AutoTrigger 1s<ms> // fastest possible
        activeChannels = magneticFieldChannel :: signalChannels
        channelStreams = magneticFieldStream :: signalStreams
        channelAggregateStreams = []
        memorySegment = 0u }

    [<CLIEvent>]
    member this.StatusChanged = statusChanged.Publish

    [<CLIEvent>]
    member this.Error = error.Publish

    [<CLIEvent>]
    member this.Canceled = canceled.Publish

    [<CLIEvent>]
    member this.Completed = completed.Publish
    
    member this.Cancel(returnToZero) =
        "CW EPR worker stopping..." |> log.Info
        cancellationCapability.Cancel(returnToZero)
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting CW EPR worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        "CW EPR worker preparing..." |> log.Info
        (sprintf "Ramp parameters:\n%A" ramp) |> log.Info
        (sprintf "Streaming parameters:\n%A" stream) |> log.Info
        (sprintf "Magnetic field input settings:\n%A" magneticFieldInputSettings) |> log.Info
        (sprintf "Lock-in signal input settings:\n%A" lockinSignalInputSettings) |> log.Info

        let syncContext = SynchronizationContext.CaptureCurrent()

        let workflow =
            let scan n =
                let rampWorker = new RampWorker(ramp, magnetController)
                let streamWorker = new StreamWorker(stream, pico)

                let prepareRamp = async {
                    "Preparing ramp." |> log.Info
                    let waitForReadyToRamp =
                        rampWorker.StatusChanged
                        |> Event.filter ((=) ReadyToRamp)

                    rampWorker.Prepare()
                    do! Async.AwaitEvent(waitForReadyToRamp)
                        |> Async.Ignore }

                let startAcquisition = async {
                    "Prepariing and starting stream acquisition." |> log.Info 
                    let waitForStart =
                        streamWorker.StatusChanged
                        |> Observable.filter ((=) (Started stream.sampleInterval))
                        |> Observable.waitHandleForNext
                    
                    streamWorker.PrepareAndStart()
                    do! Async.AwaitWaitHandle(waitForStart)
                        |> Async.Ignore
                    do! Async.Sleep(1) } // sleep 1ms for auto trigger to kick in
            
                let beginRamp = async {
                    "Beginning ramp." |> log.Info
                    rampWorker.SetReadyToStart() }
                
                let stopAcquisition = async {
                    "Stopping acquisition." |> log.Info
                    let waitForCompleted =
                        streamWorker.Completed
                        |> Observable.waitHandleForNext

                    streamWorker.Stop()
                    do! Async.AwaitWaitHandle(waitForCompleted)
                        |> Async.Ignore }

                let performRamp = async {
                    let canceled =
                        cancellationCapability.CancellationRequested
                        |> Event.map (fun returnToZero -> Some(returnToZero))
                    
                    let completed =
                        rampWorker.Completed
                        |> Event.map (fun () -> None) // no cancellation

                    let! cancellation =
                        Event.merge canceled completed
                        |> Async.AwaitEvent
                      
                    match cancellation with
                    | Some(returnToZero) -> 
                        rampWorker.Cancel(returnToZero)
                        do! stopAcquisition
                    | None ->
                        do! stopAcquisition }

                async {
                    do! prepareRamp
                    do! startAcquisition
                    do! beginRamp
                    do! performRamp }
            

            async {
                ()
                (* use! cancelHandler = Async.OnCancel(cancelRamp)

                do! prepareForRamp
                do! awaitReadyForRamp
                do! performRamp
                if ramp.returnToZero then 
                    do! magnetController.RampToZeroAsync()

                if not cancellationCapability.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged Finished *) }
        
        "Starting ramp workflow." |> log.Info
        Async.StartWithContinuations
            (workflow,
                (fun () -> syncContext.RaiseEvent completed ()), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> 
                    syncContext.RaiseEvent statusChanged (Cancelling cancellationCapability.ReturnToZero)
                    syncContext.RaiseEvent canceled exn),
                    cancellationCapability.StartUsingToken())