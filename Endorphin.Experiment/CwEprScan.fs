namespace Endorphin.Experiment

open Endorphin.Core
open Endorphin.Core.Units
open Endorphin.Core.ObservableExtensions
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open System.Threading
open log4net

type CwEprScan =
    { startingFieldIndex : int
      finalFieldIndex : int
      rampRateIndex : int
      returnToZero : bool
      conversionTime : int<ms>
      downsampling : uint32 option
      quadratureDetection : bool 
      shuntVoltageRange : Range
      shuntVoltageOffset : float<V> }

type CwEprSample =
    { magneticFieldShuntAdc : Sample
      xSignalAdc : Sample
      ySignalAdc : Sample }

type CwEprScanStatus =
    | PreparingScan
    | ReadyToScan
    | Scanning
    | FinishedScan
    | CanceledScan of returnToZero : bool
    | FailedScan

type CwEprScanWorker(magnetController : MagnetController, pico : PicoScope5000, scan : CwEprScan) =
    static let log = LogManager.GetLogger typeof<CwEprScanWorker>

    let statusChanged = new Event<CwEprScanStatus>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let success = new Event<unit>()
    let sample = new Event<CwEprSample>()

    let readyToStart = new ManualResetEvent(false)
    let cancellationCapability = new CancellationCapability<RampCancellationOptions>()

    let ramp : Ramp = { 
        startingFieldIndex = scan.startingFieldIndex
        finalFieldIndex = scan.finalFieldIndex
        rampRateIndex = scan.rampRateIndex
        returnToZero = scan.returnToZero }
    
    let minShuntVoltage =
        if sign scan.startingFieldIndex <> sign scan.finalFieldIndex then
            magnetController.DeviceParameters.shuntOffset
        else
            magnetController.DeviceParameters.shuntOffset + 
                magnetController.DeviceParameters.ShuntStep * float (min (abs scan.startingFieldIndex) (abs scan.finalFieldIndex))
    
    let maxShuntVoltage =
        magnetController.DeviceParameters.shuntOffset + 
            magnetController.DeviceParameters.ShuntStep * float (max (abs scan.startingFieldIndex) (abs scan.finalFieldIndex))

    let shuntVoltageRange = (0.5 * (maxShuntVoltage - minShuntVoltage)).SmallestInputRangeForVoltage()
    let shuntVoltageOffset = 0.5 * (minShuntVoltage + maxShuntVoltage)

    let downsampling =
        match scan.downsampling with
        | None -> Downsampling.None
        | Some _ -> Downsampling.Averaged

    let buffer =
        match scan.downsampling with
        | None -> AllSamples
        | Some _ -> Averaged

    let downsamplingRatio =
        match scan.downsampling with
        | None -> 1u
        | Some ratio -> ratio

    let magneticFieldChannelStream = {
        inputSettings = 
            { coupling = Coupling.DC
              range = shuntVoltageRange
              analogueOffset = shuntVoltageOffset
              bandwidthLimit = BandwidthLimit._20MHz }
        downsamplingModes =
            [ downsampling ] |> Set.ofList }

    let lockinSignalChannelStream = {
        inputSettings =
            { coupling = Coupling.DC
              range = Range._10V
              analogueOffset = 0.0<V>
              bandwidthLimit = BandwidthLimit._20MHz }
        downsamplingModes =
            [ downsampling ] |> Set.ofList }

    let magneticFieldChannel = (Channel.A, magneticFieldChannelStream)
    let signalChannels =
        if scan.quadratureDetection then
            [ (Channel.B, lockinSignalChannelStream) ; (Channel.C, lockinSignalChannelStream) ]
        else
            [ (Channel.B, lockinSignalChannelStream) ]

    let sampleInterval = scan.conversionTime * 1000000<ns/ms>

    let stream = {
        sampleInterval = sampleInterval
        downsamplingRatio = downsamplingRatio
        streamStop = ManualStop
        triggerSettings = AutoTrigger 1s<ms> // fastest possible
        activeChannels = (magneticFieldChannel :: signalChannels) |> Map.ofList
        memorySegment = 0u }

    member this.StatusChanged = statusChanged.Publish
    member this.Success = success.Publish
    member this.Error = error.Publish
    member this.Canceled = canceled.Publish
    member this.Sample = sample.Publish

    member this.Cancel(returnToZero) =
        "CW EPR scan worker stopping..." |> log.Info
        cancellationCapability.Cancel(returnToZero)
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting CW EPR scan worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare CW EPR scan as cancellation was already requested."

        "CW EPR worker preparing..." |> log.Info
        sprintf "Ramp parameters:\n%A" ramp |> log.Info
        sprintf "Magnetic field channel:\n%A" magneticFieldChannel |> log.Info
        sprintf "Signal channels:\n%A" signalChannels |> log.Info
        sprintf "Sample interval: %A." sampleInterval |> log.Info
        sprintf "Downsampling: %A." scan.downsampling |> log.Info
        sprintf "Quadrature detection: %s" (if scan.quadratureDetection then "yes" else "no") |> log.Info
        sprintf "Magnetic field channel stream:\n%A" magneticFieldChannelStream |> log.Info
        sprintf "Lock-in signal channel stream:\n%A" lockinSignalChannelStream |> log.Info

        let syncContext = SynchronizationContext.CaptureCurrent()
        
        let rampWorker = new RampWorker(magnetController, ramp)
        
        let workflow = async {
            let prepareRamp = async {
                "Preparing ramp." |> log.Info
                syncContext.RaiseEvent statusChanged PreparingScan

                let! waitForReadyToRamp =
                    rampWorker.StatusChanged
                    |> Event.filter (fun status ->
                        match status with
                        | ReadyToRamp _ -> true
                        | _ -> false)
                    |> Async.AwaitEvent
                    |> Async.Ignore
                    |> Async.StartChild

                rampWorker.Prepare()
                do! waitForReadyToRamp }

            let awaitReadyToStart = async {
                "Waiting for ready-to-start signal..." |> log.Info
                syncContext.RaiseEvent statusChanged ReadyToScan
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore
                "Received ready-to-start signal." |> log.Info }

            let startAcquisition (streamWorker : StreamWorker) (magneticFieldSign : int) = async {
                streamWorker.SampleSlice
                |> Event.add (fun slice ->
                    syncContext.RaiseEvent sample
                        { magneticFieldShuntAdc = (int16 magneticFieldSign) * slice.[Channel.A, buffer]
                          xSignalAdc = slice.[Channel.B, buffer]
                          ySignalAdc = if scan.quadratureDetection then slice.[Channel.C, buffer] else 0s } )

                let! waitForStart =
                    streamWorker.StatusChanged
                    |> Event.filter ((=) (Streaming sampleInterval))
                    |> Async.AwaitEvent
                    |> Async.Ignore
                    |> Async.StartChild
                    
                streamWorker.PrepareAndStart()
                do! waitForStart
                do! Async.Sleep 1 // sleep 1ms for auto trigger to kick in
                
                return! streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild }

            let scanStartToZero = async { 
                let streamWorker = new StreamWorker(pico, stream)
                use! __ = Async.OnCancel (fun () -> streamWorker.Stop())
                
                rampWorker.StatusChanged
                |> Event.filter (function
                    | ChangingCurrentDirection _ -> true
                    | _ -> false)
                |> Event.add (fun _ -> streamWorker.Stop())

                let! waitForReadyToContinue =
                    rampWorker.StatusChanged
                    |> Event.filter (function
                        | ReadyToContinue _ -> true
                        | _ -> false)
                    |> Async.AwaitEvent
                    |> Async.Ignore
                    |> Async.StartChild

                let! waitForStreamFinished = startAcquisition streamWorker (sign scan.startingFieldIndex)
                rampWorker.SetReadyToStart()
                do! waitForStreamFinished
                do! waitForReadyToContinue }

            let scanToFinish = async {
                let streamWorker = new StreamWorker(pico, stream)
                use! __ = Async.OnCancel (fun () -> streamWorker.Stop())

                let! waitForRampFinished =
                    rampWorker.StatusChanged
                    |> Event.filter ((=) FinishedRamp)
                    |> Async.AwaitEvent
                    |> Async.Ignore
                    |> Async.StartChild

                let! waitForStreamFinished = startAcquisition streamWorker (sign scan.finalFieldIndex)
                rampWorker.SetReadyToStart()
                rampWorker.SetReadyToContinue()

                do! waitForRampFinished
                streamWorker.Stop()
                do! waitForStreamFinished }

            let performScan = async {
                "Starting CW EPR scan." |> log.Info
                syncContext.RaiseEvent statusChanged Scanning
                
                if (sign scan.startingFieldIndex <> sign scan.finalFieldIndex) then
                    do! scanStartToZero
                do! scanToFinish }
            
            use! __ = Async.OnCancel (fun () ->
                sprintf "Cancelling CW EPR scan %s return to zero." 
                    (if cancellationCapability.Options.returnToZero then "with" else "without") |> log.Info
                    
                rampWorker.Cancel cancellationCapability.Options.returnToZero
                    
                "CW EPR scan canceled." |> log.Info
                syncContext.RaiseEvent statusChanged (CanceledScan cancellationCapability.Options.returnToZero))

            do! prepareRamp
            do! awaitReadyToStart
            
            "Worker successfully finished CW EPR scan." |> log.Info
            syncContext.RaiseEvent statusChanged FinishedScan }

        "Starting scan workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> syncContext.RaiseEvent success ()),
            (fun exn -> 
                log.Error (sprintf "CW EPR Scan failed due to error: %A." exn, exn)
                syncContext.RaiseEvent statusChanged FailedScan
                syncContext.RaiseEvent error exn),
            (fun exn -> syncContext.RaiseEvent canceled exn),
            cancellationCapability.Token)