namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net
open ExtCore.Control
open Endorphin.Core

module Streaming =
    type StreamStopOptions = { AcquisitionDidAutoStop : bool }
    
    type StreamStatus =
        | PreparingStream
        | Streaming of sampleInterval : Interval
        | FinishedStream of options : StreamStopOptions
        | FailedStream of error : string
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
                  StatusChanged   : Event<StreamStatus>
                  SamplesObserved : Event<StreamingSamples> }

    type StreamingAcquisitionHandle = private StreamingAcquisitionHandle of acquisition : StreamingAcquisition * waitToStop : AsyncChoice<unit, string>

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
              StatusChanged   = new Event<StreamStatus>()
              SamplesObserved = new Event<StreamingSamples>() }

        let status acquisition =
            Endorphin.Core.Observable.createFromStatusEvent(
                acquisition.StatusChanged.Publish,
                (fun status -> 
                    match status with
                    | FinishedStream _ 
                    | CancelledStream  -> true
                    | _                -> false),
                (fun status -> 
                    match status with
                    | FailedStream message -> Some (Exception message)
                    | _                    -> None ))

        let private prepareDevice acquisition =
            asyncChoice {
                acquisition.StatusChanged.Trigger PreparingStream
                do! PicoScope.ChannelSettings.setAcquisitionInputChannels acquisition.PicoScope acquisition.Parameters.Inputs
                do! PicoScope.Triggering.setTriggerSettings acquisition.PicoScope acquisition.Parameters.TriggerSettings
                do! PicoScope.Sampling.setResolution acquisition.PicoScope acquisition.Parameters.Resolution

                for inputSampling in acquisition.Parameters.Inputs.InputSampling do
                    do! PicoScope.DataBuffers.setDataBuffer acquisition.PicoScope inputSampling.InputChannel inputSampling.DownsamplingMode acquisition.Parameters.MemorySegment
                        <| Inputs.Buffers.findByInputSampling inputSampling acquisition.DataBuffers }

        let private startStreaming acquisition = asyncChoice {
            let! sampleInterval = PicoScope.Acquisition.startStreaming acquisition.PicoScope acquisition.Parameters
            acquisition.StatusChanged.Trigger (Streaming sampleInterval) }

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
                if  streamingValuesReady.DidAutoStop then
                    acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = true }

                if streamingValuesReady.NumberOfSamples <> (SampleCount 0) then
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
            
                do! Async.Sleep 100 |> AsyncChoice.liftAsync
                do! pollUntilFinished acquisition }
        
        let start acquisition : AsyncChoice<StreamingAcquisitionHandle, string> =
            if acquisition.StopCapability.IsCancellationRequested then
                failwith "Cannot start an acquisition which has already been stopped."
        
            let acquisitionWorkflow = async { 
                let! acquisitionResult = asyncChoice {
                    use! __ = AsyncChoice.liftAsync <| Async.OnCancel(fun () ->
                        Async.StartWithContinuations(PicoScope.Acquisition.stop acquisition.PicoScope,
                            (fun _ -> acquisition.StatusChanged.Trigger CancelledStream), 
                            ignore, ignore)) 

                    use __ = Inputs.Buffers.pinningHandle acquisition.DataBuffers
                    do! prepareDevice acquisition
                    do! startStreaming acquisition
                    do! pollUntilFinished acquisition
                    do! PicoScope.Acquisition.stop acquisition.PicoScope }

                match acquisitionResult with
                | Success () -> acquisition.StatusChanged.Trigger (FinishedStream acquisition.StopCapability.Options)
                | Failure message -> acquisition.StatusChanged.Trigger (FailedStream <| sprintf "Acquisition failed to stop: %s" message)
                
                return acquisitionResult }

            async {
                let! waitToStop = acquisitionWorkflow |> Async.StartChild
                return StreamingAcquisitionHandle (acquisition, waitToStop) }
            |> AsyncChoice.liftAsync
    
        let waitToFinish (StreamingAcquisitionHandle (acquisition, waitToStop)) = waitToStop   
              
        let stop (StreamingAcquisitionHandle (acquisition, waitToStop)) =
            if not acquisition.StopCapability.IsCancellationRequested then
                acquisition.StopCapability.Cancel { AcquisitionDidAutoStop = false }

        let stopAndFinish acquisitionHandle =
            stop acquisitionHandle
            waitToFinish acquisitionHandle

    module Signal =
        let private takeUntilFinished acquisition (obs : IObservable<'T>) =
            obs.TakeUntil ((Acquisition.status acquisition).LastAsync())
        
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
        let scan signal           = Observable.scan List.cons List.empty signal |> Observable.map Seq.ofList

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
        
        let private adcCountManyEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)

        let adcCountMany inputs acquisition =
            adcCountManyEvent inputs acquisition
            |> takeUntilFinished acquisition

        let voltageMany inputs acquisition =
            adcCountManyEvent inputs acquisition
            |> Event.map (adcCountsToVoltages inputs acquisition)
            |> takeUntilFinished acquisition

        let adcCountManyBy f inputs acquisition =
            adcCountManyEvent inputs acquisition
            |> Event.mapi (fun i samples -> (f i, samples))
            |> takeUntilFinished acquisition

        let voltageManyBy f inputs acquisition =
            adcCountManyEvent inputs acquisition
            |> Event.mapi (fun i adcCounts -> (f i, adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let adcCountManyByIndex = adcCountManyBy id
        let adcCountManyByTime inputs acquisition = adcCountManyBy (time acquisition.Parameters.SampleInterval) inputs acquisition

        let voltageManyByInex = voltageManyBy id
        let voltageManyByTime inputs acquisition = voltageManyBy (time acquisition.Parameters.SampleInterval) inputs acquisition

        let adcCountXY xInput yInput acquisition =
            adcCountMany [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        let voltageXY xInput yInput acquisition =
            voltageMany [| xInput ; yInput |] acquisition
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

        let private adcCountManyByBlockEvent inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.findArray inputs)

        let adcCountManyByBlock inputs acquisition =
            adcCountManyByBlockEvent inputs acquisition
            |> takeUntilFinished acquisition

        let voltageManyByBlock inputs acquisition = 
            adcCountManyByBlockEvent inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let private adcCountWindowedEvent n input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.windowMapSeq n (fun samples -> samples.Samples |> Map.find input)

        let adcCountWindowed n input acquisition =
            adcCountWindowedEvent n input acquisition
            |> takeUntilFinished acquisition

        let voltageWindowed n input acquisition =
            adcCountWindowedEvent n input acquisition
            |> Event.map (Array.map (adcCountToVoltage input acquisition))
            |> takeUntilFinished acquisition

        let private adcCountManyWindowedEvent n inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.windowMapSeq n (takeInputs inputs)

        let adcCountManyWindowed n inputs acquisition =
            adcCountWindowedEvent n inputs acquisition
            |> takeUntilFinished acquisition

        let voltageManyWindowed n inputs acquisition =
            adcCountManyWindowedEvent n inputs acquisition
            |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs acquisition adcCounts))
            |> takeUntilFinished acquisition

        let voltageOverflow inputChannel acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.filter (fun block -> Set.contains inputChannel block.VoltageOverflows)
            |> Event.map (fun block -> block.VoltageOverflows)
            |> takeUntilFinished acquisition