namespace Endorphin.Instrument.PicoScope5000

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
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

    /// Contains the result of a streaming acquisition, indicating whether the stream finished
    /// successfully, failed or was cancelled.
    type StreamResult =
        | StreamCompleted
        | StreamError of exn
        | StreamCancelled

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
                  WaitToFinish : Async<StreamResult> }

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
    
    /// Functions related to streaming acquisition control flow.
    module Acquisition =
        
        /// Creates a streaming acquisition on the given PicoScope 5000 series device with the specified
        /// streaming parameters.
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

        /// Returns an observable which fires when the streaming acquistion status changes.
        let status acquisition =
            acquisition.StatusChanged.Publish
            |> Observable.fromNotificationEvent

        /// Prepares the device for a streaming acquisition by setting up the channel settings, triggering,
        /// device resolution and data buffers.
        let private prepareDevice acquisition =
            async {
                acquisition.StatusChanged.Trigger (Next PreparingStream)
                do! PicoScope.ChannelSettings.setAcquisitionInputChannels acquisition.PicoScope acquisition.Parameters.Inputs
                do! PicoScope.Triggering.setTriggerSettings acquisition.PicoScope acquisition.Parameters.TriggerSettings
                do! PicoScope.Sampling.setResolution acquisition.PicoScope acquisition.Parameters.Resolution

                for inputSampling in acquisition.Parameters.Inputs.InputSampling do
                    do! PicoScope.DataBuffers.setDataBuffer acquisition.PicoScope inputSampling.InputChannel inputSampling.DownsamplingMode acquisition.Parameters.MemorySegment
                        <| Inputs.Buffers.findByInputSampling inputSampling acquisition.DataBuffers }

        /// Initiates the streaming acquisition.
        let private startStreaming acquisition = async {
            let! sampleInterval = PicoScope.Acquisition.startStreaming acquisition.PicoScope acquisition.Parameters
            acquisition.StatusChanged.Trigger (Next <| Streaming sampleInterval) }

        /// Creates a sample block for the given StreamingValuesReady callback parameters with blank arrays
        /// which have to be filled by copying values from the streaming buffers.
        let private createSampleBlock streamingValuesReady acquisition =
            let sampleCount = streamingValuesReady.NumberOfSamples 
            { Samples =
                acquisition.DataBuffers.Buffers
                |> Map.keys
                |> Seq.map (fun sampling -> (sampling, Array.zeroCreate<AdcCount> sampleCount))
                |> Map.ofSeq
              Length           = streamingValuesReady.NumberOfSamples 
              VoltageOverflows = streamingValuesReady.VoltageOverflows }
        
        /// Copies values from the acquisition buffer into the given sample block for the given StreamingValuesReady
        /// callback parameters. Copying is parallelised.
        let private copySampleBlockValues streamingValuesReady acquisition sampleBlock =
            Map.toSeq acquisition.DataBuffers.Buffers
            |> Seq.map (fun (inputSampling, buffer) -> async {
                let sampleArray = Map.find inputSampling sampleBlock.Samples
                let startIndex  = streamingValuesReady.StartIndex
                let sampleCount = streamingValuesReady.NumberOfSamples
                Array.Copy(buffer, int startIndex, sampleArray, 0, int sampleCount) })
            |> Async.Parallel
            |> Async.Ignore

        /// Handles a callback with the given StreamingValues ready parameters.
        let private handleStreamingValuesReady acquisition streamingValuesReady =
            // check if the acquisition has been stopped manually
            if not acquisition.StopCapability.IsCancellationRequested then
                if streamingValuesReady.DidAutoStop then // or automatically
                    acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = true }

                // if there are samples to copy then create a sample block and copy the values into it before
                // triggering the samples observed event
                if streamingValuesReady.NumberOfSamples <> 0 then
                    let sampleBlock = createSampleBlock streamingValuesReady acquisition
                    Async.StartWithContinuations(
                        copySampleBlockValues streamingValuesReady acquisition sampleBlock,
                        (fun () -> acquisition.SamplesObserved.Trigger sampleBlock),
                        ignore, ignore)

        /// Polls the device continuously until the acquisition is stopped manually or automatically.
        let rec private pollUntilFinished acquisition = async {
            if not acquisition.StopCapability.IsCancellationRequested then
                let! __ = 
                    PicoScope.Acquisition.pollStreamingLatestValues acquisition.PicoScope
                    <| handleStreamingValuesReady acquisition
                
                // TODO: Add some kind of variable delay here to prevent the polling from prepetually
                // using a CPU thread, while also being able to cope with fast sample intervals
                do! pollUntilFinished acquisition }
        
        /// Runs an acquisition with the given cancellation token. If the token is cancelled, then acquisition
        /// will be aborted. To ensure that the acquisition is stopped cleanly, no other cancellation 
        /// compensations should be registered with this cancellation token which will post commands to the
        /// PicoScope. Returns a handle which can be used to stop the acquisition manually or wait for it to
        /// finish asynchronously.
        let startWithCancellationToken acquisition cancellationToken =
            if acquisition.StopCapability.IsCancellationRequested then
                failwith "Cannot start an acquisition which has already been stopped."
            
            // create a result channel which will be used to register the acquisition result and await
            // it asynchronously
            let resultChannel = new ResultChannel<_>()

            // if the acquisition finishes normally, stop it and register the result
            let finishAcquisition acquisitionResult = 
                Async.StartWithContinuations(
                    PicoScope.Acquisition.stop acquisition.PicoScope,
                    (fun () ->
                        acquisition.StatusChanged.Trigger (Next <| FinishedStream acquisition.StopCapability.Options)
                        acquisition.StatusChanged.Trigger Completed
                        resultChannel.RegisterResult StreamCompleted),
                    (fun stopExn ->
                        acquisition.StatusChanged.Trigger (Error stopExn)
                        resultChannel.RegisterResult (StreamError stopExn)),
                    ignore)

            // if an error occurs stop the acquisition and register result result
            let stopAcquisitionAfterError exn =
                Async.StartWithContinuations(
                    PicoScope.Acquisition.stop acquisition.PicoScope,
                    (fun () ->
                        acquisition.StatusChanged.Trigger (Error exn)
                        resultChannel.RegisterResult (StreamError exn)),
                    (fun stopExn ->
                        acquisition.StatusChanged.Trigger (Error stopExn)
                        resultChannel.RegisterResult (StreamError stopExn)),
                    ignore)
  
            // if the acquisition is cancelled, stop it and register the result
            let stopAcquisitionAfterCancellation _ = 
                Async.StartWithContinuations(
                    PicoScope.Acquisition.stop acquisition.PicoScope,
                    (fun () ->
                        acquisition.StatusChanged.Trigger (Next <| CancelledStream)
                        acquisition.StatusChanged.Trigger Completed
                        resultChannel.RegisterResult StreamCancelled),
                    (fun stopExn ->
                        acquisition.StatusChanged.Trigger (Error stopExn)
                        resultChannel.RegisterResult (StreamError stopExn)),
                    ignore)

            // define the acquisition workflow
            let acquisitionWorkflow = async {
                use __ = Inputs.Buffers.createPinningHandle acquisition.DataBuffers
                do! prepareDevice acquisition
                do! startStreaming acquisition
                do! pollUntilFinished acquisition }
            
            // start it with the continuations defined above and the given cancellation token
            Async.StartWithContinuations(
                    acquisitionWorkflow,
                    finishAcquisition,
                    stopAcquisitionAfterError, 
                    stopAcquisitionAfterCancellation,
                    cancellationToken)

            // and return an acquisition handle which can be used to await the result
            { Acquisition = acquisition ; WaitToFinish = resultChannel.AwaitResult () }
        
        /// Starts a streaming acquisition. If the ability to abort the acquisition via a cancellation
        /// token is required, then start the acquisition using startWithCancellationToken instead. Returns
        /// a handle which can be used to stop the acquisition manually or wait for it to finish
        /// asynchronously.
        let start acquisition = startWithCancellationToken acquisition Async.DefaultCancellationToken

        /// Starts a streaming acquisition as a child (sharing the same cancellation token) to the current
        /// asynchronous workflow. To ensure that the acquisition is stopped cleanly, no other cancellation 
        /// compensations should be registered with this cancellation token which will post commands to the
        /// PicoScope. Returns a handle which can be used to stop the acquisition manually or wait for it to
        /// finish asynchronously.
        let startAsChild acquisition = async {
            let! ct = Async.CancellationToken
            return startWithCancellationToken acquisition ct }

        /// Asynchronously waits for the acquisition associated with the given handle to finish and returns
        /// the result.
        let waitToFinish acquisitionHandle = acquisitionHandle.WaitToFinish   
        
        /// Manually stops the streaming acquisition associated with the given handle.
        let stop acquisitionHandle =
            if not acquisitionHandle.Acquisition.StopCapability.IsCancellationRequested then
                acquisitionHandle.Acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = false }

        /// Manually stops the streaming acquisition associated with the given handle and asynchronously
        /// waits for it to finish.
        let stopAndFinish acquisitionHandle =
            stop acquisitionHandle
            waitToFinish acquisitionHandle
    
    /// Functions for obtaining the signal projections for an acquisition.
    module Signal =

        /// Completes the given observable when the acquisition completes.
        let private takeUntilFinished acquisition =
            Observable.takeUntilOther (Observable.last (Acquisition.status acquisition))
        
        /// Computes the timestamp of a sample for the given index and sample interval.
        let private time interval (downsampling : DownsamplingRatio option) index =
            let ratio =
                match downsampling with
                | None -> 1.0
                | Some ratio -> float ratio
            (Interval.asSeconds interval) * (float index) * ratio
        
        /// Returns a function which converts an ADC count to a voltage for the given input sampling in an
        /// acquisition.
        let private adcCountToVoltage (inputChannel, _) acquisition = 
            let channelSettings = Inputs.settingsForChannel inputChannel acquisition.Parameters.Inputs
            match channelSettings with
            | EnabledChannel settings ->
                let (Voltage_V voltageRange)   = Range.voltage settings.Range
                let (Voltage_V analogueOffset) = settings.AnalogueOffset
                let maximumAdcCounts                = Resolution.maximumAdcCounts acquisition.Parameters.Resolution
                fun (adcCounts : int16) -> voltageRange * (float32 adcCounts) / (float32 maximumAdcCounts) - analogueOffset
            | DisabledChannel -> failwithf "Cannot calculate voltage for channel %A as it is not enabled." inputChannel

        /// Returns a function which converts an array of samples from ADC counts to voltages according to
        /// the given array of input channel acquisition settings.
        let private adcCountsToVoltages (inputs : (InputChannel * BufferDownsampling) array) acquisition =
            Array.mapi (fun i adcCount -> adcCountToVoltage inputs.[i] acquisition adcCount)

        /// Returns a sequence of sample arrays in the given sample block for the specified array of inputs.
        let private takeInputs inputs samples = seq {
            let length = samples.Length
            for i in 0 .. length - 1 ->
                samples.Samples
                |> Map.findArray inputs
                |> Array.map (fun block -> block.[i]) }
        
        /// Returns the n-th signal from an observable which emits an array of signals at each observation.
        let nth n = Observable.map (fun samples -> Array.get samples n)

        /// Takes the n-th and m-th signals from an observable which emits an array of signals at each
        /// observation and combines them into a tuple.
        let takeXY n m = Observable.map (fun samples -> (Array.get samples n, Array.get samples m))

        /// Takes the n-th and m-th signals from an observable which emits an array of indexed or timestamped
        /// signals at each observation and combines the values into a tuple, discarding the indecies.
        let takeXYFromIndexed n m = Observable.map (fun (_, samples) -> (Array.get samples n, Array.get samples m))

        /// Accumulates a signal, emitting the sequence of samples observed so far at each observation.
        let scan signal = Observable.scanInit List.empty List.cons signal |> Observable.map Seq.ofList

        /// Helper function for constructing observables based on the ADC count samples for a given input.
        let private adcCountEvent input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)
            |> Event.collectSeq Seq.ofArray
        
        /// Returns an observable which emits the ADC count for every sample observed on the specified input
        /// in an acquisition.
        let adcCount input acquisition =
            adcCountEvent input acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits the voltage for every sample observed on the specified input in
        /// an acquisition.
        let voltage input acquisition =
            adcCountEvent input acquisition
            |> Event.map (adcCountToVoltage input acquisition)
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
        /// function and ADC count for every sample observed on the specified input in an acquisition.
        let adcCountBy indexMapping input acquisition =
            adcCountEvent input acquisition
            |> Event.mapi (fun i adcCount -> (indexMapping i, adcCount))
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
        /// function and voltage for every sample observed on the specified input in an acquisition.
        let voltageBy indexMapping input acquisition =
            adcCountEvent input acquisition
            |> Event.mapi (fun i adcCount -> (indexMapping i, adcCountToVoltage input acquisition adcCount))
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index and ADC count for every sample observed
        /// on the specified input in an acquisition.
        let adcCountByIndex = adcCountBy id

        /// Returns an observable which emits a tuple of timestamp and ADC count for every sample observed on
        /// the specified input in an acquisition.
        let adcCountByTime input acquisition =
            adcCountBy
            <| time acquisition.Parameters.SampleInterval acquisition.Parameters.DownsamplingRatio
            <| input
            <| acquisition

        /// Returns an observable which emits a tuple of sample index and voltage for every sample observed on
        /// the specified input in an acquisition.
        let voltageByIndex = voltageBy id

        /// Returns an observable which emits a tuple of timestamp and voltage for every sample observed on the
        /// specified input in an acquisition.
        let voltageByTime input acquisition =
            voltageBy
            <| time acquisition.Parameters.SampleInterval acquisition.Parameters.DownsamplingRatio
            <| input
            <| acquisition
        
        /// Helper function for constructing observables based on the ADC count samples for a given array of
        /// inputs.
        let private adcCountsEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)

        /// Returns an observable which emits an array of ADC counts for every sample observed on the specified
        /// array of inputs in an acquisition.
        let adcCounts inputs acquisition =
            adcCountsEvent inputs acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits an array of voltages for every sample observed on the specified
        /// array of inputs in an acquisition.
        let voltages inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.map (adcCountsToVoltages inputs acquisition)
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
        /// function and array of ADC counts for every sample observed on the specified array of inputs
        /// in an acquisition.
        let adcCountsBy indexMapping inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.mapi (fun i samples -> (indexMapping i, samples))
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
        /// function and array of voltages for every sample observed on the specified array of inputs in an
        /// acquisition.
        let voltagesBy indexMapping inputs acquisition =
            adcCountsEvent inputs acquisition
            |> Event.mapi (fun i adcCounts -> (indexMapping i, adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a tuple of sample index and array of ADC counts for every sample
        /// observed on the specified array of inputs in an acquisition.
        let adcCountsByIndex = adcCountsBy id

        /// Returns an observable which emits a tuple of timestamp and array of ADC counts for every sample
        /// observed on the specified array of inputs in an acquisition.
        let adcCountsByTime inputs acquisition =
            adcCountsBy
            <| time acquisition.Parameters.SampleInterval acquisition.Parameters.DownsamplingRatio
            <| inputs
            <| acquisition

        /// Returns an observable which emits a tuple of sample index and array of voltages for every sample
        /// observed on the specified array of inputs in an acquisition.
        let voltagesByInex = voltagesBy id

        /// Returns an observable which emits a tuple of timestamp and array of voltages for every sample
        /// observed on the specified array of inputs in an acquisition.
        let voltagesByTime inputs acquisition =
            voltagesBy
            <| time acquisition.Parameters.SampleInterval acquisition.Parameters.DownsamplingRatio
            <| inputs
            <| acquisition

        /// Returns an observable which emits a tuple of ADC counts for each pair of samples observed on the
        /// specified inputs in an acquisition.
        let adcCountXY xInput yInput acquisition =
            adcCounts [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        /// Returns an observable which emits a tuple of voltages for each pair of samples observed on the
        /// specified inputs in an acquisition.
        let voltageXY xInput yInput acquisition =
            voltages [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        /// Helper function for constructing observables based on sample blocks for a given input in an
        /// acquisition.
        let private adcCountByBlockEvent input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)

        /// Returns an observable which emits an array of ADC counts for each block of samples observed on the
        /// specified input in an acquisition.
        let adcCountByBlock input acquisition =
            adcCountByBlockEvent input acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits an array of voltages for each block of samples observed on the
        /// specified input in an acquisition.
        let voltageByBlock input acquisition =
            adcCountByBlockEvent input acquisition
            |> Event.map (Array.map (adcCountToVoltage input acquisition))
            |> takeUntilFinished acquisition

        /// Helper function for constructing observables based on the sample blocks for a given array of inputs
        /// in an acquisition.
        let private adcCountsByBlockEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.findArray inputs)

        /// Returns an observable which emits an array of ADC count blocks for each sample block observed for
        /// the given array of inputs in an acquisition.
        let adcCountsByBlock inputs acquisition =
            adcCountsByBlockEvent inputs acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits an array of voltage blocks for each sample block observed for the
        /// given array of inputs in an acquisition.
        let voltagesByBlock inputs acquisition = 
            adcCountsByBlockEvent inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        /// Helper function for constructing observables which buffer a specified number of the latest samples
        /// on a given input in acquisition.
        let private adcCountBufferedEvent count input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (fun samples -> samples.Samples |> Map.find input)
            |> Event.bufferCountOverlapped count

        /// Returns an observable which emits the latest specified number of ADC counts sampled on a given
        /// input after each sample block in an acquisition.
        let adcCountBuffered count input acquisition =
            adcCountBufferedEvent count input acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits the latest specified number of voltages sampled on a given input
        /// after each sample block in an acquisition.
        let voltageBuffered windowSize input acquisition =
            adcCountBufferedEvent windowSize input acquisition
            |> Event.map (Array.map (adcCountToVoltage input acquisition))
            |> takeUntilFinished acquisition

        /// Helper function for constructing observables which buffer a specified number of the latest samples
        /// on a given array of inputs in an acquisition.
        let private adcCountsBufferedEvent count inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)
            |> Event.bufferCountOverlapped count

        /// Returns an observable which emits an array of the latest specified number of ADC counts sampled on
        /// a given array of inputs after each sample block in an acquisition.
        let adcCountsBuffered count inputs acquisition =
            adcCountBufferedEvent count inputs acquisition
            |> takeUntilFinished acquisition

        /// Returns an observable which emits an array of the latest specified number of voltages sampled on a
        /// given array of inputs after each sample block in an acquisition.
        let voltagesBuffered count inputs acquisition =
            adcCountsBufferedEvent count inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        /// Returns an observable which emits a set of channels on which voltage overflow occurred if voltage 
        /// overflow occurs on any input channel in an acquisition. 
        let voltageOverflow acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.filter (fun block -> not <| Set.isEmpty block.VoltageOverflows)
            |> Event.map (fun block -> block.VoltageOverflows)
            |> takeUntilFinished acquisition