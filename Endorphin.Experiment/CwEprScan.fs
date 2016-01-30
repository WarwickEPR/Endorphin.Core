// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Experiment

open Endorphin.Core
open Endorphin.Core.Units
open Endorphin.Core.ObservableExtensions
open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net

/// Defines the parameters for a CW EPR scan.
type CwEprScan =
    { /// The signed index for the starting magnet controller field in digital magnet contorller steps.
      StartingFieldIndex : int
      /// The signed index for the final magnet controller field in digital magnet controller steps.
      FinalFieldIndex : int
      /// The index of the calibrated magnet controller ramp rate to be used during the scan.
      RampRateIndex : int
      /// A flag indicating whether the magnet controller should return to zero current once the scan is completed.
      ReturnToZero : bool
      /// The time interval between PicoScope samples used to sample the magnetic field and signal during the scan.
      SampleInterval : int<us>
      /// An optional downsampling ratio to be used to average the magnetic field and signal channels. Set None to 
      /// acquire all samples.
      DownsamplingRatio : uint32 option
      /// A flag indicating whether the quadrature channel should be sampled during the scan.
      QuadratureDetection : bool
      /// The input voltage range to be used for sampling the magnet controller shunt.
      ShuntVoltageRange : Range
      /// The analogue voltage offset to be used for sampling the magnet controller shunt.
      ShuntVoltageOffset : float<V> }

/// Defines a data point in a CW EPR scan.
type CwEprSample =
    { /// The magnetic field shunt channel ADC value for the sample.
      MagneticFieldShuntAdc : int16
      /// The real signal channel ADC value.
      ReSignalAdc : int16
      /// The imaginary singal channel ADC value.
      ImSignalAdc : int16 }

/// Defines the possible status messages sent by a CwEprScanWorker.
type CwEprScanStatus =
    /// The CwEprScanWorker is preparing for the scan by ramping to the initial current.
    | PreparingScan
    /// The CwEprScanWorker is ready to start the signal acquisition and begin ramping.
    | ReadyToScan
    /// The CwEprScanWorker has started the scan.
    | Scanning
    /// The CwEprScanWorker has finished the scan.
    | FinishedScan
    /// The CwEprScanWorker scan has been cancelled. The returnToZero flag indicates whether the magnet controller has been
    /// set to ramp to zero current.
    | CanceledScan of exn : OperationCanceledException * returnToZero : bool
    /// The CwEprScanWorker has failed due to an error.
    | FailedScan of exn : exn

/// Performs a CW EPR scan with the specified parameters using the provided Twickenham magnet controller and PicoScope.
type CwEprScanWorker(magnetController : MagnetController, pico : PicoScope5000, scan : CwEprScan) =
    static let log = LogManager.GetLogger typeof<CwEprScanWorker> // logger

    // events
    let statusChanged = new Event<CwEprScanStatus>()
    let sampleObserved = new Event<CwEprSample>()

    // handle which is used to indicate whether the scan should start once it is prepared
    let readyToStart = new ManualResetHandle(false)
    // cancellation capability which provides the cancellation token for the scan workflow
    let cancellationCapability = new CancellationCapability<RampCancellationOptions>()

    // create a magnet controller ramp with the parameters specified by the scan
    let ramp : Ramp = { 
        StartingFieldIndex = scan.StartingFieldIndex
        FinalFieldIndex = scan.FinalFieldIndex
        RampRateIndex = scan.RampRateIndex
        ReturnToZero = scan.ReturnToZero }
    
    // choose the downsampling mode for the streaming acquisition according to the specified downsampling for the scan
    let downsamplingMode =
        match scan.DownsamplingRatio with
        | None -> DownsamplingMode.None
        | Some _ -> DownsamplingMode.Averaged

    // choose the stream bufffer which will be sampled according to the specified downsampling for the scan
    let buffer =
        match scan.DownsamplingRatio with
        | None -> NoDownsampling
        | Some _ -> Averaged

    // define the magnet controller channel stream properties (input settings and downsampling mode)
    let magneticFieldChannelStream = {
        InputSettings = 
            { Coupling = Coupling.DC
              Range = scan.ShuntVoltageRange
              AnalogueOffset = scan.ShuntVoltageOffset
              BandwidthLimit = BandwidthLimit._20MHz }
        DownsamplingModes =
            [ downsamplingMode ] |> Set.ofList }

    // define the lockin channel stream properties (input settings and downsampling mode)
    let lockinSignalChannelStream = {
        InputSettings =
            { Coupling = Coupling.DC
              Range = Range._10V
              AnalogueOffset = 0.0<V>
              BandwidthLimit = BandwidthLimit._20MHz }
        DownsamplingModes =
            [ downsamplingMode ] |> Set.ofList }

    // define the magnetic field input channel
    let magneticFieldChannel = (Channel.A, magneticFieldChannelStream)
    
    // define the lock-in signal input channels
    let signalChannels =
        if scan.QuadratureDetection then
            [ (Channel.B, lockinSignalChannelStream) ; (Channel.C, lockinSignalChannelStream) ] // two if quadrature detection is set
        else
            [ (Channel.B, lockinSignalChannelStream) ] // one otherwise

    // define the acquisition channel buffers
    let magneticFieldChannelBuffer = { Channel = Channel.A ; BufferDownsampling = buffer }
    let realSignalChannelBuffer = { Channel = Channel.B ; BufferDownsampling = buffer }
    let imaginarySignalChannelBuffer = { Channel = Channel.C ; BufferDownsampling = buffer }

    // convert the conversion time to a sample interval in nanoseconds
    let sampleInterval = scan.SampleInterval * 1000<ns/us>

    // create the stream parameters
    let stream = {
        SampleInterval = sampleInterval
        DownsamplingRatio = scan.DownsamplingRatio
        StreamStop = ManualStop // the scan will be stopped manually once the magnet controller reaches the ramp target
        TriggerSettings = AutoTrigger 1s<ms> // trigger automatically, with minimum delay
        ActiveChannels = (magneticFieldChannel :: signalChannels) |> Map.ofList // magnetic field channel and signal channels
        MemorySegment = 0u }

    /// Event fires when the status of the scan worker changes.
    member __.StatusChanged =
        Observable.CreateFromStatusEvent(
            statusChanged.Publish,
            // fire OnCompleted when the scan finishes
            ((=) FinishedScan), 
            // fire OnError when the scan fails or is canceled
            (fun status ->
                match status with
                | FailedScan exn -> Some exn
                | CanceledScan (exn, _) -> Some (exn :> exn)
                | _ -> None))

    /// Event fires when a CwEprSample is observed.
    member scanWorker.SampleObserved =
        sampleObserved.Publish
            .TakeUntil(scanWorker.StatusChanged.LastAsync())

    /// Cancels a scan which is in progress, specifying whether the magnet controller should return to zero current or pause.
    member __.Cancel returnToZero =
        "CW EPR scan worker stopping..." |> log.Info
        cancellationCapability.Cancel returnToZero
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    /// Initiates the scan workflow and sets the ready-to-start flag so that the scan will start immediately once prepared to the
    /// initial magnet controller current.
    member scanWorker.PrepareAndStart() =
        scanWorker.SetReadyToStart()
        scanWorker.Prepare()

    /// Sets the ready-to-start flag so that the scan will start once prepared at the initial magnet controller current.
    member __.SetReadyToStart() =
        "Setting CW EPR scan worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    /// Initiates the scan workflow, ramping to the initial magnet controller current, where it will wait for the ready-to-start
    /// flag to be set.
    member __.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare CW EPR scan as cancellation was already requested."

        "CW EPR worker preparing..." |> log.Info
        sprintf "Ramp parameters:\n%A" ramp |> log.Info
        sprintf "Magnetic field channel:\n%A" magneticFieldChannel |> log.Info
        sprintf "Signal channels:\n%A" signalChannels |> log.Info
        sprintf "Conversion time: %A us." scan.SampleInterval |> log.Info
        sprintf "Downsampling: %A." scan.DownsamplingRatio |> log.Info
        sprintf "Quadrature detection: %s" (if scan.QuadratureDetection then "yes" else "no") |> log.Info
        sprintf "Magnetic field channel stream:\n%A" magneticFieldChannelStream |> log.Info
        sprintf "Lock-in signal channel stream:\n%A" lockinSignalChannelStream |> log.Info
        
        // create the ramp worker with the specified magnet controller and ramp settings
        let rampWorker = new RampWorker(magnetController, ramp)
        
        // define the scan workflow
        let workflow =           
            // define a workflow which prepares for the magnet controller ramp by going to the initial current
            let prepareRamp = async {
                "Preparing ramp." |> log.Info
                // fire an event indicating that the scan worker is preparing
                statusChanged.Trigger PreparingScan

                // start a child workflow which waits for a status from the ramp worker indicating that it is ready
                // to start the ramp
                let! waitForReadyToRamp =
                    rampWorker.StatusChanged
                    |> Observable.filter (fun status ->
                        match status with
                        | ReadyToRamp _ -> true
                        | _ -> false)
                    |> Async.AwaitObservable
                    |> Async.Ignore
                    |> Async.StartChild
                
                // initiate the ramp workflow
                rampWorker.Prepare()

                // and wait for the child workflow to complete
                do! waitForReadyToRamp }

            // define a workflow which will wait for the ready-to-start flag to be set
            let awaitReadyToStart = async {
                "Waiting for ready-to-start signal..." |> log.Info
                // fire an event indicating that the scan worker is now ready to start the scan
                statusChanged.Trigger ReadyToScan

                // wait for the ready-to-start flag to be set
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore
                "Received ready-to-start signal." |> log.Info }

            // define a workflow which will start the streaming acquisition and add an event handler to process each sample
            let startAcquisition (streamWorker : StreamWorker) (magneticFieldSign : int) = async {                
                // for every slice of samples which is observed
                streamWorker.SampleSliceObserved
                |> Observable.add (fun slice ->
                    { // the magnetic field shunt readout doesn't change sign with the current direction so multiply the
                      // channel A sample with a sign
                      MagneticFieldShuntAdc = (int16 magneticFieldSign) * slice.[magneticFieldChannelBuffer]
                      // read the real part of the signal from channel B
                      ReSignalAdc = slice.[realSignalChannelBuffer]
                      // and read the imaginary part of the signal from channel C if quadrature detection is enabled
                      // or set the imaginary signal to zero otherwise
                      ImSignalAdc = if scan.QuadratureDetection then slice.[imaginarySignalChannelBuffer] else 0s }
                    |> sampleObserved.Trigger) // and fire the sampleObserved event with this new value

                // start a child workflow, waiting for the stream worker status to indicate that acquisition has started
                let! waitForStart =
                    streamWorker.StatusChanged
                    |> Observable.filter ((=) (Streaming sampleInterval))
                    |> Async.AwaitObservable
                    |> Async.Ignore
                    |> Async.StartChild
                   
                // initiate the acquisition stream
                streamWorker.PrepareAndStart()
                // and wait for the child workflow to complete
                do! waitForStart
                
                // start a new child workflow, waiting for the stream worker status to indicate that it has completed successfully
                let! waitForStreamFinished =
                    streamWorker.StatusChanged.LastAsync()
                    |> Async.AwaitObservable
                    |> Async.StartChild
                
                // and return an async workflow which will wait for the stream to finish
                return waitForStreamFinished }

            // define a workflow which will perform the scan from the initial magnet controller current to zero, if the scan crosses
            // zero current
            let scanStartToZero = async {
                // create a stream worker with the specified streaming parameters and the provided PicoScope
                let streamWorker = new StreamWorker(pico, stream)
                
                // if cancellation occurs in this scope, stop the stream worker
                use! __ = Async.OnCancel (fun () -> streamWorker.Stop())
                
                // start a child workflow which will wait for the ramp worker to indicate that it is about to change current direction
                let! waitForRampChangingCurrentDirection =
                    rampWorker.StatusChanged
                    |> Observable.filter (function
                        | ChangingCurrentDirection _ -> true
                        | _ -> false)
                    |> Async.AwaitObservable
                    |> Async.Ignore
                    |> Async.StartChild

                // start a child workflow which will wait for the ramp worker status to indicate that it is ready to continue
                // the ramp after crossing zero current
                let! waitForReadyToContinue =
                    rampWorker.StatusChanged
                    |> Observable.filter (function
                        | ReadyToContinue _ -> true
                        | _ -> false)
                    |> Async.AwaitObservable
                    |> Async.Ignore
                    |> Async.StartChild

                // start the acquisition stream
                let! waitForStreamFinished = startAcquisition streamWorker ramp.StartingCurrentSign
                // and set the ramp worker ready-to-start flag
                rampWorker.SetReadyToStart()

                // wait for the ramp worker to indicate that it is changing current direction
                do! waitForRampChangingCurrentDirection
                // and stop the acquisition stream
                streamWorker.Stop()

                // wait for the stream to be stopped, checking the result
                let! result = waitForStreamFinished
                if result <> (FinishedStream false) then
                    let message = sprintf "Streaming acquisition failed with status: %A." result
                    let exn = new Exception(message)
                    log.Error(message, exn)

                /// and wait for the ramp worker to inidicate that it is ready to continue after it has finished changing direction
                do! waitForReadyToContinue }

            // define a workflow which will ramp to the final current, acquiring data
            let scanToFinish = async {
                // create a stream worker with the specified streaming parameters and the provided PicoScope
                let streamWorker = new StreamWorker(pico, stream)
                
                // if cancellation occurs in this scope, stop the stream worker
                use! __ = Async.OnCancel (fun () -> streamWorker.Stop())

                // start a child workflow which will wait for the ramp worker to indicate that it has finished ramping
                let! waitForRampFinished =
                    rampWorker.StatusChanged
                    |> Observable.filter ((=) FinishedRamp)
                    |> Async.AwaitObservable
                    |> Async.Ignore
                    |> Async.StartChild

                // start the acquisition stream
                let! waitForStreamFinished = startAcquisition streamWorker ramp.FinalCurrentSign

                // set both ready-to-start and ready-to-continue flags (the latter is used if the ramp worker crosses zero current)
                rampWorker.SetReadyToStart()
                rampWorker.SetReadyToContinue()

                // wait for the ramp to finish
                do! waitForRampFinished
                // then stop the acquisition stream
                streamWorker.Stop()
                // and wait for the acquisition stream to finish, checking the result
                let! result = waitForStreamFinished
                if result <> (FinishedStream false) then
                    let message = sprintf "Streaming acquisition failed with status: %A." result
                    let exn = new Exception(message)
                    log.Error(message, exn) }

            // define a workflow which will perfor the CW EPR scan
            let performScan = async {
                "Starting CW EPR scan." |> log.Info
                // fire an event indicating that the scan worker is now performing the scan
                statusChanged.Trigger Scanning 
                
                // if the cramp crosses zero
                if ramp.StartingCurrentDirection <> ramp.FinalCurrentDirection then
                    do! scanStartToZero // then first scan from the starting current to zero and acquire data

                // scan to the final current
                do! scanToFinish }
            
            // now use the workflows defined above to define the complete workflow for the scan worker
            async {
                // if cancellation occurs
                use! __ = Async.OnCancel (fun () ->
                    sprintf "Cancelling CW EPR scan %s return to zero." 
                        (if cancellationCapability.Options.ReturnToZero then "with" else "without") |> log.Info
                   
                    // cancel the ramp worker and return to zero if specified
                    rampWorker.Cancel cancellationCapability.Options.ReturnToZero)
                
                do! prepareRamp // prepare the magnet controller ramp to zero current
                do! awaitReadyToStart // wait for the ready-to-start flag to be set
                do! performScan } // and perform the scan

        // start the scan workflow with the specified continuations for success, failure and cancellation and with
        // the provided cancellation token
        "Starting scan workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> // success
                "Worker successfully finished CW EPR scan." |> log.Info
                statusChanged.Trigger FinishedScan),
            (fun exn -> // error
                log.Error (sprintf "CW EPR Scan failed due to error: %A." exn, exn)
                statusChanged.Trigger (FailedScan exn)),
            (fun exn -> 
                "CW EPR scan canceled." |> log.Info
                statusChanged.Trigger (CanceledScan (exn, cancellationCapability.Options.ReturnToZero))),
            cancellationCapability.Token)