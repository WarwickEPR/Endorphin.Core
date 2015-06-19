namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net
open ExtCore.Control
open Endorphin.Core

module Streaming =
    type StreamStopOptions = DidAutoStop of didAutoStop : bool
    
    type StreamStatus =
        | PreparingStream
        | Streaming of sampleInterval : Interval
        | FinishedStream of options : StreamStopOptions
        | FailedStream of error : string

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
              Inputs            = Inputs.empty }

        let withResolution resolution (parameters : StreamingParameters) = { parameters with Resolution = resolution }
        let withSampleInterval sampleInterval (parameters : StreamingParameters) = { parameters with SampleInterval = sampleInterval }
        let withBufferLength bufferLength (parameters : StreamingParameters) = { parameters with BufferLength = bufferLength }
        let withMemorySegment memorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = memorySegment }
        let withNextMemorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = MemorySegment.next parameters.MemorySegment }
        let withTrigger trigger (parameters : StreamingParameters) = { parameters with TriggerSettings = trigger }
        let withManualStop (parameters : StreamingParameters) = { parameters with StreamStop = ManualStop }
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
                    | FinishedStream _ -> true
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
                    acquisition.StopCapability.Cancel (DidAutoStop true)

                if streamingValuesReady.NumberOfSamples <> (SampleCount 0) then
                    let sampleBlock = createSampleBlock streamingValuesReady acquisition
                    Async.StartWithContinuations(
                        copySampleBlockValues streamingValuesReady acquisition sampleBlock,
                        (fun () -> acquisition.SamplesObserved.Trigger sampleBlock),
                        ignore, ignore)

        let rec private pollUntilFinished acquisition = asyncChoice {
            if not acquisition.StopCapability.IsCancellationRequested then
                let! availability = 
                    PicoScope.Acquisition.pollStreamingLatestValues acquisition.PicoScope
                    <| handleStreamingValuesReady acquisition
            
                do! Async.Sleep 100 |> AsyncChoice.liftAsync
                do! pollUntilFinished acquisition }

        let private finishAcquisition acquisition acquisitionResult = async {
            let! stopResult = PicoScope.Acquisition.stop acquisition.PicoScope
            match acquisitionResult, stopResult with
            | Success (), Success () ->
                acquisition.StatusChanged.Trigger (FinishedStream acquisition.StopCapability.Options)
                return succeed ()
            | _, Failure message ->
                acquisition.StatusChanged.Trigger (FailedStream <| sprintf "Acquisition failed to stop: %s" message)
                return fail message
            | Failure message, _ ->
                acquisition.StatusChanged.Trigger (FailedStream <| sprintf "Acquisition failed: %s" message)
                return fail message }
        
        let start acquisition =
            if acquisition.StopCapability.IsCancellationRequested then
                failwith "Cannot start an acquisition which has already been stopped."
        
            let acquisitionWorkflow = asyncChoice {
                use __ = Inputs.Buffers.pinningHandle acquisition.DataBuffers
                do! prepareDevice acquisition
                do! startStreaming acquisition
                do! pollUntilFinished acquisition }

            asyncChoice {
                let! waitToStop = acquisitionWorkflow |> Async.StartChild |> AsyncChoice.liftAsync 
                return StreamingAcquisitionHandle (acquisition, waitToStop) }
    
        let waitToFinish (StreamingAcquisitionHandle (acquisition, waitToStop)) = async {
            let! acquisitionResult = waitToStop
            return! finishAcquisition acquisition acquisitionResult }       
              
        let stop (StreamingAcquisitionHandle (acquisition, waitToStop)) =
            if not acquisition.StopCapability.IsCancellationRequested then
                acquisition.StopCapability.Cancel (DidAutoStop false)

        let stopAndFinish acquisitionHandle =
            stop acquisitionHandle
            waitToFinish acquisitionHandle 

    module Signals =
        let private takeUntilFinished acquisition (obs : IObservable<'T>) =
            obs.TakeUntil ((Acquisition.status acquisition).LastAsync())
        
        let private time interval index = (Interval.asSeconds interval) * (float index)
        
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

        let sample input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)
            |> Event.collectSeq Seq.ofArray
            |> takeUntilFinished acquisition

        let sampleBy f input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)
            |> Event.collectSeq Seq.ofArray
            |> Event.mapi (fun i sample -> (f i, sample))
            |> takeUntilFinished acquisition

        let sampleByIndex = sampleBy id
        let sampleByTime input acquisition = sampleBy (time acquisition.Parameters.SampleInterval) input acquisition
        
        let sampleMany inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)
            |> takeUntilFinished acquisition
            
        let sampleManyBy f inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.collectSeq (takeInputs inputs)
            |> Event.mapi (fun i samples -> (f i, samples))
            |> takeUntilFinished acquisition

        let sampleManyByIndex = sampleManyBy id
        let sampleManyByTime inputs acquisition = sampleManyBy (time acquisition.Parameters.SampleInterval) inputs acquisition

        let sampleXY xInput yInput acquisition =
            sampleMany [| xInput ; yInput |] acquisition
            |> takeXY 0 1

        let sampleByBlock input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.find input)
            |> takeUntilFinished acquisition

        let sampleManyByBlock inputList acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.map (fun samples -> samples.Samples |> Map.findArray inputList)
            |> takeUntilFinished acquisition

        let windowLatest n input acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.windowMapSeq n (fun samples -> samples.Samples |> Map.find input)
            |> takeUntilFinished acquisition

        let windowLatestMany n inputs acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.windowMapSeq n (takeInputs inputs)
            |> takeUntilFinished acquisition

        let voltageOverflow inputChannel acquisition =
            acquisition.SamplesObserved.Publish
            |> Event.filter (fun block -> Set.contains inputChannel block.VoltageOverflows)
            |> Event.map (fun block -> block.VoltageOverflows)
            |> takeUntilFinished acquisition