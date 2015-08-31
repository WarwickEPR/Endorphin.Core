namespace Endorphin.Instrument.PicoScope5000

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open ExtCore.Control
open FSharp.Control.Reactive
open log4net

/// Functions related to running a general purpose streaming acquisition on a PicoScope 5000 series
/// device and collecting the data.
module Streaming =
    
    /// Contains information about the way a streaming acquisition was stopped, indicating whether
    /// it was stopped manually or automatically.
    type StreamStopOptions = { AcquisitionDidAutoStop : bool }
    
    /// Status of a streaming acquisition emitted through a status event while the acquisition is
    /// in progress.
    type StreamStatus =
        | PreparingStream
        | Streaming of sampleInterval : Interval
        | FinishedStream of options : StreamStopOptions
        | CancelledStream

    /// Contains a block of streaming samples obtained after polling the device.
    type StreamingSamples =
        internal { Samples          : Map<InputChannel * BufferDownsampling, AdcCount array>
                   Length           : SampleCount
                   VoltageOverflows : Set<InputChannel> }

    /// Defines a streaming acquisition which has been set up on a PicoScope 5000 series device. This
    /// is created once after the streaming parameters are defined and a connection to the device is
    /// established.
    type StreamingAcquisition =
        private { Parameters      : StreamingParameters
                  PicoScope       : PicoScope5000
                  DataBuffers     : AcquisitionBuffers
                  StopCapability  : CancellationCapability<StreamStopOptions>
                  StatusChanged   : NotificationEvent<StreamStatus>
                  SamplesObserved : Event<StreamingSamples> }

    /// Represents a handle to a streaming acquisition which has been started on a PicoScope 5000 series
    /// device. This can be used to stop the acquisition manually and/or wait for it to finish.
    type StreamingAcquisitionHandle = 
        private { Acquisition  : StreamingAcquisition 
                  WaitToFinish : AsyncChoice<unit, string> }

    /// Functions for specifying streaming acquisition parameters.
    module Parameters =
        
        /// Creates streaming acquisition parameters with the specified resolution, sample interval and
        /// buffer length and default values for everything else. The default memory segment is 0. The 
        /// device is set to trigger automatically after 1 ms. Acquisition is set to stop manually and
        /// no downsampling is used. No inputs are enabled.
        let create resolution sampleInterval bufferLength =
            { Resolution        = resolution
              SampleInterval    = sampleInterval
              BufferLength      = bufferLength
              MemorySegment     = MemorySegment.zero
              TriggerSettings   = Trigger.auto 1s<ms>
              StreamStop        = ManualStop
              DownsamplingRatio = None
              Inputs            = Inputs.none }
        
        /// Returns modified streaming acquisition parameters with the given vertical resolution.
        let withResolution resolution (parameters : StreamingParameters) = { parameters with Resolution = resolution }

        /// Returns modified streaming acquisition parameters with the given sample interval.
        let withSampleInterval sampleInterval (parameters : StreamingParameters) = { parameters with SampleInterval = sampleInterval }

        /// Returns modified streaming acquisition parameters with the specified acquisition buffer length.
        let withBufferLength bufferLength (parameters : StreamingParameters) = { parameters with BufferLength = bufferLength }

        /// Returns modified streaming acquisition parameters with the specified memory segment.
        let withMemorySegment memorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = memorySegment }

        /// Returns modified streaming acquisition parameters with an incremented memory segment.
        let withNextMemorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = MemorySegment.next parameters.MemorySegment }
        
        /// Returns modified streaming acquisition parameters with the specified trigger settings.
        let withTrigger trigger (parameters : StreamingParameters) = { parameters with TriggerSettings = trigger }

        /// Returns modified streaming acquisition parameters specifying that the acquisition will not stop
        /// automatically after a fixed number of samples has been acquired but have to be stopped manually.
        let withManualStop (parameters : StreamingParameters) = { parameters with StreamStop = ManualStop }

        /// Returns modified streaming acquisition parameters specifying that the acquisition will stop
        /// automatically after the specified number of samples before and after the trigger has been
        /// captured.
        let withAutoStop maxPreTriggerSamples maxPostTriggerSamples (parameters : StreamingParameters) = 
            { parameters with StreamStop = AutoStop(maxPreTriggerSamples, maxPostTriggerSamples) }
    
        /// Returns modified streaming acquisition parameters with the specified downsampling ratio.
        let withDownsamplingRatio downsamplingRatio (parameters : StreamingParameters) =
            if not (Inputs.hasDownsampling parameters.Inputs) then
                failwith "Cannot specifiy a downsampling ratio for an acquisition which has no downsampled inputs."

            { parameters with DownsamplingRatio = Some downsamplingRatio }

        /// Returns modified streaming acquisition parameters with the specified input channel enabled with the
        /// given input settings. Fails if the channel is already enabled.
        let enableChannel channel coupling range voltageOffset bandwidth (parameters : StreamingParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.enableChannel channel coupling range voltageOffset bandwidth }

        /// Returns modified streaming acquisition parameters with the specified list of input channels enabled
        /// with the given input settings. Fails if any of the channels in the set is already enabled.
        let enableChannels channels coupling range voltageOffset bandwidth (parameters : StreamingParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.enableChannel channels coupling range voltageOffset bandwidth }
            
        /// Returns modified streaming acquisition parameters with the specified input channel sampled with
        /// the given downsampling mode. Fails if the channel is not enabled. Also fails if the acquisition
        /// has inputs which are sampled with no downsampling while another downsampling mode is given or
        /// vice-versa.
        let sampleChannel channel downsamplingMode (parameters : StreamingParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.sampleChannel channel downsamplingMode }
    
        /// Returns modified streaming acquisition parameters with the specified list of input channels
        /// sampled with the given downsampling mode. Fails if the channel is not enabled. Also fails if the
        /// acquisition has inputs which are sampled with no downsampling while another downsampling mode is
        /// given or vice-versa.
        let sampleChannels channels downsamplingMode (parameters : StreamingParameters) = 
            { parameters with Inputs = parameters.Inputs |> Inputs.sampleChannels channels downsamplingMode }

    module Acquisition =
        let create picoScope streamingParameters =
            if streamingParameters.Inputs |> Inputs.hasDownsampling && streamingParameters.DownsamplingRatio = None then
                failwith "Failed to create streaming acquisition: specified inputs have downsampling but no downsampling ratio is specified."

            if not (streamingParameters.Inputs |> Inputs.hasDownsampling) && streamingParameters.DownsamplingRatio <> None then
                failwith "Failed to create streaming acquisition: specified inputs have no downsampling but a downsampling ratio is specified."

            { Parameters  = streamingParameters
              PicoScope   = picoScope
              DataBuffers = 
                Inputs.Buffers.allocateAcquisitionBuffers 
                <| streamingParameters.MemorySegment
                <| streamingParameters.BufferLength
                <| streamingParameters.Inputs
              StopCapability  = new CancellationCapability<StreamStopOptions>()
              StatusChanged   = new NotificationEvent<StreamStatus>()
              SamplesObserved = new Event<StreamingSamples>() }

        let status acquisition =
            acquisition.StatusChanged.Publish
            |> Observable.fromNotificationEvent

        let private prepareDevice acquisition =
            asyncChoice {
                acquisition.StatusChanged.Trigger (Next PreparingStream)
                do! PicoScope.ChannelSettings.setAcquisitionInputChannels acquisition.PicoScope acquisition.Parameters.Inputs
                do! PicoScope.Triggering.setTriggerSettings acquisition.PicoScope acquisition.Parameters.TriggerSettings
                do! PicoScope.Sampling.setResolution acquisition.PicoScope acquisition.Parameters.Resolution

                for inputSampling in acquisition.Parameters.Inputs.InputSampling do
                    do! PicoScope.DataBuffers.setDataBuffer acquisition.PicoScope inputSampling.InputChannel inputSampling.DownsamplingMode acquisition.Parameters.MemorySegment
                        <| Inputs.Buffers.findByInputSampling inputSampling acquisition.DataBuffers }

        let private startStreaming acquisition = asyncChoice {
            let! sampleInterval = PicoScope.Acquisition.startStreaming acquisition.PicoScope acquisition.Parameters
            acquisition.StatusChanged.Trigger (Next <| Streaming sampleInterval) }

        let private createSampleBlock streamingValuesReady acquisition =
            let sampleCount = streamingValuesReady.NumberOfSamples 
            { Samples =
                acquisition.DataBuffers.Buffers
                |> Map.keys
                |> Seq.map (fun sampling -> (sampling, Array.zeroCreate<AdcCount> sampleCount))
                |> Map.ofSeq
              Length           = streamingValuesReady.NumberOfSamples 
              VoltageOverflows = streamingValuesReady.VoltageOverflows }

        let private copySampleBlockValues streamingValuesReady acquisition sampleBlock =
            Map.toSeq acquisition.DataBuffers.Buffers
            |> Seq.map (fun (inputSampling, buffer) -> async {
                let sampleArray = Map.find inputSampling sampleBlock.Samples
                let startIndex  = streamingValuesReady.StartIndex
                let sampleCount = streamingValuesReady.NumberOfSamples
                Array.Copy(buffer, int startIndex, sampleArray, 0, int sampleCount) })
            |> Async.Parallel
            |> Async.Ignore

        let private handleStreamingValuesReady acquisition streamingValuesReady =
            if not acquisition.StopCapability.IsCancellationRequested then
                if streamingValuesReady.DidAutoStop then
                    acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = true }

                if streamingValuesReady.NumberOfSamples <> 0 then

                    let sampleBlock = createSampleBlock streamingValuesReady acquisition
                    Async.StartWithContinuations(
                        copySampleBlockValues streamingValuesReady acquisition sampleBlock,
                        (fun () -> acquisition.SamplesObserved.Trigger sampleBlock),
                        ignore, ignore)

        let rec private pollUntilFinished acquisition = asyncChoice {
            if not acquisition.StopCapability.IsCancellationRequested then
                let! __ = 
                    PicoScope.Acquisition.pollStreamingLatestValues acquisition.PicoScope
                    <| handleStreamingValuesReady acquisition
                
                do! pollUntilFinished acquisition }
        
        let startWithCancellationToken acquisition cancellationToken =
            if acquisition.StopCapability.IsCancellationRequested then
                failwith "Cannot start an acquisition which has already been stopped."
            
            let resultChannel = new ResultChannel<_>()

            let finishAcquisition acquisitionResult = Async.StartImmediate <| async {
                let! stopResult = PicoScope.Acquisition.stop acquisition.PicoScope
                match acquisitionResult, stopResult with
                | Success (), Success () -> 
                    acquisition.StatusChanged.Trigger (Next <| FinishedStream acquisition.StopCapability.Options)
                    acquisition.StatusChanged.Trigger Completed
                    resultChannel.RegisterResult (succeed ())
                | _, Failure f -> 
                    let error = sprintf "Acquisition failed to stop: %s" f
                    acquisition.StatusChanged.Trigger (Error (Exception error))
                    resultChannel.RegisterResult (fail error)
                | Failure f, _ ->
                    acquisition.StatusChanged.Trigger (Error <| Exception f)
                    resultChannel.RegisterResult (fail f) }

            let stopAcquisitionAfterError exn = Async.StartImmediate <| async {
                let! stopResult = PicoScope.Acquisition.stop acquisition.PicoScope
                match stopResult with
                | Success () ->
                    acquisition.StatusChanged.Trigger (Error exn)
                    resultChannel.RegisterResult (fail exn.Message)
                | Failure f ->
                    let error = sprintf "Acquisition failed to stop after exception: %s" exn.Message
                    acquisition.StatusChanged.Trigger (Error (Exception error))
                    resultChannel.RegisterResult (fail error) }

            let stopAcquisitionAfterCancellation exn = Async.StartImmediate <| async { 
                let! stopResult =  PicoScope.Acquisition.stop acquisition.PicoScope
                match stopResult with
                | Success () ->
                    acquisition.StatusChanged.Trigger (Next <| CancelledStream)
                    acquisition.StatusChanged.Trigger Completed
                    resultChannel.RegisterResult (succeed ())
                | Failure f ->
                    let error = sprintf "Acquisition failed to stop after cancellation: %s" f
                    acquisition.StatusChanged.Trigger (Error <| Exception error)
                    resultChannel.RegisterResult (fail error) }

            let acquisitionWorkflow =
                Async.StartWithContinuations(
                    asyncChoice {
                            use __ = Inputs.Buffers.createPinningHandle acquisition.DataBuffers
                            do! prepareDevice acquisition
                            do! startStreaming acquisition
                            do! pollUntilFinished acquisition }, 
                    
                        finishAcquisition,
                        stopAcquisitionAfterError, 
                        stopAcquisitionAfterCancellation,
                        cancellationToken)

            { Acquisition = acquisition ; WaitToFinish = resultChannel.AwaitResult () }
    
        let start acquisition = startWithCancellationToken acquisition (Async.DefaultCancellationToken)

        let startAsChild acquisition = asyncChoice {
            let! ct = Async.CancellationToken |> AsyncChoice.liftAsync
            return startWithCancellationToken acquisition ct }

        let waitToFinish acquisitionHandle = acquisitionHandle.WaitToFinish   
              
        let stop acquisitionHandle =
            if not acquisitionHandle.Acquisition.StopCapability.IsCancellationRequested then
                acquisitionHandle.Acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = false }

        let stopAndFinish acquisitionHandle =
            stop acquisitionHandle
            waitToFinish acquisitionHandle

    module Signal =
        let private takeUntilFinished acquisition =
            Observable.takeUntilOther (Observable.last (Acquisition.status acquisition))
        
        let private time interval index = (Interval.asSeconds interval) * (float index)
        
        let private adcCountToVoltage (inputChannel, _) acquisition = 
            let channelSettings = Inputs.settingsForChannel inputChannel acquisition.Parameters.Inputs
            match channelSettings with
            | EnabledChannel settings ->
                let (VoltageInVolts voltageRange)   = Range.voltage settings.Range
                let (VoltageInVolts analogueOffset) = settings.AnalogueOffset
                let maximumAdcCounts                = Resolution.maximumAdcCounts acquisition.Parameters.Resolution
                fun (adcCounts : int16) -> voltageRange * (float32 adcCounts) / (float32 maximumAdcCounts) - analogueOffset
            | DisabledChannel -> failwithf "Cannot calculate voltage for channel %A as it is not enabled." inputChannel

        let private adcCountsToVoltages (inputs : (InputChannel * BufferDownsampling) array) acquisition =
            Array.mapi (fun i adcCount -> adcCountToVoltage inputs.[i] acquisition adcCount)

        let private takeInputs inputs samples = seq {
            let length = samples.Length
            for i in 0 .. length - 1 ->
                samples.Samples
                |> Map.findArray inputs
                |> Array.map (fun block -> block.[i]) }

        let nth n                 = Observable.map (fun samples      -> Array.get samples n)
        let nthIndexed n          = Observable.map (fun (t, samples) -> (t, Array.get samples n))
        let takeXY n m            = Observable.map (fun samples      -> (Array.get samples n, Array.get samples m))
        let takeXYFromIndexed n m = Observable.map (fun (_, samples) -> (Array.get samples n, Array.get samples m))
        let scan signal           = Observable.fold List.cons List.empty signal |> Observable.map Seq.ofList

        let private adcCountEvent input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)
            |> Event.collectSeq Seq.ofArray

        let adcCount input acquisition =
            adcCountEvent input acquisition
            |> takeUntilFinished acquisition

        let voltage input acquisition =
            adcCountEvent input acquisition
            |> Event.map (adcCountToVoltage input acquisition)
            |> takeUntilFinished acquisition

        let adcCountBy f input acquisition =
            adcCountEvent input acquisition
            |> Event.mapi (fun i adcCount -> (f i, adcCount))
            |> takeUntilFinished acquisition

        let voltageBy f input acquisition =
            adcCountEvent input acquisition
            |> Event.mapi (fun i adcCount -> (f i, adcCountToVoltage input acquisition adcCount))
            |> takeUntilFinished acquisition

        let adcCountByIndex = adcCountBy id
        let adcCountByTime input acquisition = adcCountBy (time acquisition.Parameters.SampleInterval) input acquisition

        let voltageByIndex = voltageBy id
        let voltageByTime input acquisition = voltageBy (time acquisition.Parameters.SampleInterval) input acquisition
        
        let private adcCountsEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)

        let adcCounts inputs acquisition =
            adcCountsEvent inputs acquisition
            |> takeUntilFinished acquisition

        let voltages inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.map (adcCountsToVoltages inputs acquisition)
            |> takeUntilFinished acquisition

        let adcCountsBy f inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.mapi (fun i samples -> (f i, samples))
            |> takeUntilFinished acquisition

        let voltagesBy f inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.mapi (fun i adcCounts -> (f i, adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let adcCountsByIndex = adcCountsBy id
        let adcCountsByTime inputs acquisition = adcCountsBy (time acquisition.Parameters.SampleInterval) inputs acquisition

        let voltagesByInex = voltagesBy id
        let voltagesByTime inputs acquisition = voltagesBy (time acquisition.Parameters.SampleInterval) inputs acquisition

        let adcCountXY xInput yInput acquisition =
            adcCounts [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        let voltageXY xInput yInput acquisition =
            voltages [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        let private adcCountByBlockEvent input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)

        let adcCountByBlock input acquisition =
            adcCountByBlockEvent input acquisition
            |> takeUntilFinished acquisition

        let voltageByBlock input acquisition =
            adcCountByBlockEvent input acquisition
            |> Event.map (Array.map (adcCountToVoltage input acquisition))
            |> takeUntilFinished acquisition

        let private adcCountsByBlockEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.findArray inputs)

        let adcCountsByBlock inputs acquisition =
            adcCountsByBlockEvent inputs acquisition
            |> takeUntilFinished acquisition

        let voltagesByBlock inputs acquisition = 
            adcCountsByBlockEvent inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let private adcCountBufferedEvent count input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (fun samples -> samples.Samples |> Map.find input)
            |> Event.bufferCountOverlapped count

        let adcCountBuffered count input acquisition =
            adcCountBufferedEvent count input acquisition
            |> takeUntilFinished acquisition

        let voltageBuffered windowSize input acquisition =
            adcCountBufferedEvent windowSize input acquisition
            |> Event.map (Array.map (adcCountToVoltage input acquisition))
            |> takeUntilFinished acquisition

        let private adcCountsBufferedEvent count inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)
            |> Event.bufferCountOverlapped count

        let adcCountsBuffered ount inputs acquisition =
            adcCountBufferedEvent ount inputs acquisition
            |> takeUntilFinished acquisition

        let voltagesBuffered count inputs acquisition =
            adcCountsBufferedEvent count inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let voltageOverflow inputChannel acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.filter (fun block -> Set.contains inputChannel block.VoltageOverflows)
            |> Event.map (fun block -> block.VoltageOverflows)
            |> takeUntilFinished acquisition