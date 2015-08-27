namespace Endorphin.Instrument.PicoScope5000

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open ExtCore.Control
open FSharp.Control.Reactive
open log4net

module Streaming =
    type StreamStopOptions = { AcquisitionDidAutoStop : bool }
    
    type StreamStatus =
        | PreparingStream
        | Streaming of sampleInterval : Interval
        | FinishedStream of options : StreamStopOptions
        | CancelledStream

    type StreamingSamples =
        internal { Samples          : Map<InputChannel * BufferDownsampling, AdcCount array>
                   Length           : SampleCount
                   VoltageOverflows : Set<InputChannel> }

    type StreamingAcquisition =
        private { Parameters      : StreamingParameters
                  PicoScope       : PicoScope5000
                  DataBuffers     : AcquisitionBuffers
                  StopCapability  : CancellationCapability<StreamStopOptions>
                  StatusChanged   : NotificationEvent<StreamStatus>
                  SamplesObserved : Event<StreamingSamples> }

    type StreamingAcquisitionHandle = 
        private { Acquisition  : StreamingAcquisition 
                  WaitToFinish : AsyncChoice<AsyncResult<Choice<unit, string>>, string> }

    module Parameters =
        let create resolution sampleInterval bufferLength =
            { Resolution        = resolution
              SampleInterval    = sampleInterval
              BufferLength      = SampleIndex bufferLength
              MemorySegment     = MemorySegment.zero
              TriggerSettings   = Trigger.auto 1s<ms>
              StreamStop        = ManualStop
              DownsamplingRatio = None
              Inputs            = Inputs.none }

        let withResolution resolution                               (parameters : StreamingParameters) = { parameters with Resolution = resolution }
        let withSampleInterval sampleInterval                       (parameters : StreamingParameters) = { parameters with SampleInterval = sampleInterval }
        let withBufferLength bufferLength                           (parameters : StreamingParameters) = { parameters with BufferLength = bufferLength }
        let withMemorySegment memorySegment                         (parameters : StreamingParameters) = { parameters with MemorySegment = memorySegment }
        let withNextMemorySegment                                   (parameters : StreamingParameters) = { parameters with MemorySegment = MemorySegment.next parameters.MemorySegment }
        let withTrigger trigger                                     (parameters : StreamingParameters) = { parameters with TriggerSettings = trigger }
        let withManualStop                                          (parameters : StreamingParameters) = { parameters with StreamStop = ManualStop }
        let withAutoStop maxPreTriggerSamples maxPostTriggerSamples (parameters : StreamingParameters) = { parameters with StreamStop = AutoStop(maxPreTriggerSamples, maxPostTriggerSamples) }
    
        let withDownsampling downsamplingRatio inputs (parameters : StreamingParameters) =
            if not (Inputs.hasDownsampling inputs) then
                failwith "Cannot specifiy a downsampling ratio for an acquisition which has no downsampled inputs."

            { parameters with
                DownsamplingRatio = Some downsamplingRatio
                Inputs = inputs }

        let withNoDownsampling inputs (parameters : StreamingParameters) =
            if Inputs.hasDownsampling inputs then
                failwith "Cannot specify downsampled inputs without specifying a downsampling ratio."

            { parameters with
                DownsamplingRatio = None
                Inputs = inputs }

    module Acquisition =
        let create picoScope streamingParameters =
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
            let (SampleCount sampleCount) = streamingValuesReady.NumberOfSamples 
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
                let (SampleIndex startIndex)  = streamingValuesReady.StartIndex
                let (SampleCount sampleCount) = streamingValuesReady.NumberOfSamples
                Array.Copy(buffer, int startIndex, sampleArray, 0, int sampleCount) })
            |> Async.Parallel
            |> Async.Ignore

        let private handleStreamingValuesReady acquisition streamingValuesReady =
            if not acquisition.StopCapability.IsCancellationRequested then
                if streamingValuesReady.DidAutoStop then
                    acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = true }

                if streamingValuesReady.NumberOfSamples <> SampleCount 0 then

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

            let finishAcquisition cont econt acquisitionResult = Async.StartImmediate <| async {
                let! stopResult = PicoScope.Acquisition.stop acquisition.PicoScope
                match acquisitionResult, stopResult with
                | Success (), Success () -> 
                    acquisition.StatusChanged.Trigger (Next <| FinishedStream acquisition.StopCapability.Options)
                    acquisition.StatusChanged.Trigger Completed
                    cont (succeed ())
                | _, Failure f -> 
                    let error = Exception <| sprintf "Acquisition failed to stop: %s" f
                    acquisition.StatusChanged.Trigger (Error error)
                    econt error
                | Failure f, _ ->
                    acquisition.StatusChanged.Trigger (Error <| Exception f)
                    cont (fail f) }

            let stopAcquisitionAfterCancellation ccont econt exn = Async.StartImmediate <| async { 
                let! stopResult =  PicoScope.Acquisition.stop acquisition.PicoScope
                match stopResult with
                | Success () ->
                    acquisition.StatusChanged.Trigger (Next <| CancelledStream)
                    acquisition.StatusChanged.Trigger Completed
                    ccont exn
                | Failure f ->
                    let error = sprintf "Acquisition failed to stop after cancellation: %s" f
                    acquisition.StatusChanged.Trigger (Error <| Exception error)
                    econt (Exception error) }

            let acquisitionWorkflow =
                Async.FromContinuations (fun (cont, econt, ccont) ->
                    Async.StartWithContinuations(
                        asyncChoice {
                            use __ = Inputs.Buffers.pinningHandle acquisition.DataBuffers
                            do! prepareDevice acquisition
                            do! startStreaming acquisition
                            do! pollUntilFinished acquisition }, 
                    
                        finishAcquisition cont econt, 
                        econt, 
                        stopAcquisitionAfterCancellation ccont econt,
                        cancellationToken))

            let waitToFinish = Async.StartWithResultHandle (acquisitionWorkflow, cancellationToken)
            { Acquisition = acquisition ; WaitToFinish = waitToFinish |> AsyncChoice.liftAsync }
    
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
            let (SampleCount length) = samples.Length
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