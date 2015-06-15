namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open log4net
open System.Reactive.Linq
open ExtCore.Control

module StreamingAcquisition =
    type StreamStopOptions = DidAutoStop of didAutoStop : bool
    
    type StreamStatus =
        | PreparingStream
        | Streaming of sampleInterval : Interval
        | FinishedStream of options : StreamStopOptions
        | FailedStream of error : string

    type SampleBlock =
        internal { Samples          : Map<InputChannel * BufferDownsampling, AdcCount array>
                   Length           : SampleCount
                   VoltageOverflows : Set<InputChannel> }
                   
    type StreamingAcquisition =
        private { Parameters           : StreamingParameters
                  DataBuffers          : AcquisitionBuffers
                  StopCapability       : CancellationCapability<StreamStopOptions>
                  StatusChanged        : Event<StreamStatus>
                  SampleBlockObserved  : Event<SampleBlock>
                  mutable WaitToFinish : AsyncChoice<unit, string> option }

    let createParameters resolution sampleInterval bufferLength =
        { Resolution        = resolution
          SampleInterval    = sampleInterval
          BufferLength      = SampleIndex bufferLength
          MemorySegment     = MemorySegment.zero
          TriggerSettings   = Trigger.auto 1s<ms>
          StreamStop        = ManualStop
          DownsamplingRatio = None
          Inputs            = Acquisition.empty }

    let withResolution resolution (parameters : StreamingParameters) = { parameters with Resolution = resolution }
    let withSampleInterval sampleInterval (parameters : StreamingParameters) = { parameters with SampleInterval = sampleInterval }
    let withBufferLength bufferLength (parameters : StreamingParameters) = { parameters with BufferLength = bufferLength }
    let withMemorySegment memorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = memorySegment }
    let withNextMemorySegment (parameters : StreamingParameters) = { parameters with MemorySegment = MemorySegment.next parameters.MemorySegment }
    let withTrigger trigger (parameters : StreamingParameters) = { parameters with TriggerSettings = trigger }
    let withManualStop (parameters : StreamingParameters) = { parameters with StreamStop = ManualStop }
    let withAutoStop maxPreTriggerSamples maxPostTriggerSamples (parameters : StreamingParameters) = { parameters with StreamStop = AutoStop(maxPreTriggerSamples, maxPostTriggerSamples) }
    
    let withDownsampling downsamplingRatio inputs (parameters : StreamingParameters) =
        if not (Acquisition.hasDownsampling inputs) then
            failwith "Cannot specifiy a downsampling ratio for an acquisition which has no downsampled inputs."

        { parameters with
            DownsamplingRatio = Some downsamplingRatio
            Inputs = inputs }

    let withNoDownsampling inputs (parameters : StreamingParameters) =
        if Acquisition.hasDownsampling inputs then
            failwith "Cannot specify downsampled inputs without specifying a downsampling ratio."

        { parameters with
            DownsamplingRatio = None
            Inputs = inputs }

    let createAcquisition streamingParameters =
        { Parameters  = streamingParameters 
          DataBuffers = 
            Acquisition.Buffers.allocateAcquisitionBuffers 
            <| streamingParameters.MemorySegment
            <| streamingParameters.BufferLength
            <| streamingParameters.Inputs
          StopCapability         = new CancellationCapability<StreamStopOptions>()
          StatusChanged          = new Event<StreamStatus>()
          SampleBlockObserved    = new Event<SampleBlock>()
          WaitToFinish           = None }

    let statusChanged acquisition =
        Observable.CreateFromStatusEvent(
            acquisition.StatusChanged.Publish,
            (fun status -> 
                match status with
                | FinishedStream _ -> true
                | _                -> false),
            (fun status -> 
                match status with
                | FailedStream message -> Some (Exception message)
                | _                    -> None ))

    let private takeUntilFinished acquisition (obs : IObservable<'T>) =
        obs.TakeUntil ((statusChanged acquisition).LastAsync())

    let sampleBlockObserved acquisition =
        acquisition.SampleBlockObserved.Publish
        |> takeUntilFinished acquisition

    let sampleObserved inputChannel bufferDownsampling acquisition =
        acquisition.SampleBlockObserved.Publish
        |> Event.collectSeq (fun block ->
            block.Samples
            |> Map.find (inputChannel, bufferDownsampling)
            |> Seq.ofArray)
        |> takeUntilFinished acquisition

    let voltageOverflow inputChannel acquisition =
        acquisition.SampleBlockObserved.Publish
        |> Event.filter (fun block -> Set.contains inputChannel block.VoltageOverflows)
        |> takeUntilFinished acquisition

    let private prepareDevice picoScope acquisition =
        asyncChoice {
            acquisition.StatusChanged.Trigger PreparingStream
            do! PicoScope.ChannelSettings.setAcquisitionInputChannels picoScope acquisition.Parameters.Inputs
            do! PicoScope.Triggering.setTriggerSettings picoScope acquisition.Parameters.TriggerSettings
            do! PicoScope.Sampling.setResolution picoScope acquisition.Parameters.Resolution

            for inputSampling in acquisition.Parameters.Inputs.InputSampling do
                do! PicoScope.DataBuffers.setDataBuffer picoScope inputSampling.InputChannel inputSampling.DownsamplingMode acquisition.Parameters.MemorySegment
                    <| Acquisition.Buffers.findByInputSampling inputSampling acquisition.DataBuffers }
    
    let private startStreaming picoScope acquisition = asyncChoice {
        let! sampleInterval = PicoScope.Acquisition.startStreaming picoScope acquisition.Parameters
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
                    (fun () -> acquisition.SampleBlockObserved.Trigger sampleBlock),
                    ignore, ignore)

    let rec private pollUntilFinished picoScope acquisition = asyncChoice {
        if not acquisition.StopCapability.IsCancellationRequested then
            let! availability = 
                PicoScope.Acquisition.pollStreamingLatestValues picoScope
                <| handleStreamingValuesReady acquisition
            
            do! Async.Sleep 100 |> AsyncChoice.liftAsync
            do! pollUntilFinished picoScope acquisition }

    let private finishAcquisition picoScope acquisition acquisitionResult = async {
        let! stopResult = PicoScope.Acquisition.stop picoScope
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
        
    let start picoScope acquisition =
        if acquisition.StopCapability.IsCancellationRequested then
            failwith "Cannot start an acquisition which has already been stopped."
        
        let acquisitionWorkflow = asyncChoice {
            use __ = Acquisition.Buffers.pinningHandle acquisition.DataBuffers
            do! prepareDevice picoScope acquisition
            do! startStreaming picoScope acquisition
            do! pollUntilFinished picoScope acquisition }

        asyncChoice {
            let! waitToFinish = acquisitionWorkflow |> Async.StartChild |> AsyncChoice.liftAsync 
            acquisition.WaitToFinish <- Some waitToFinish }
                      
    let stop picoScope acquisition =
        match acquisition.WaitToFinish with
        | Some waitToFinish ->
            if not acquisition.StopCapability.IsCancellationRequested then
                acquisition.StopCapability.Cancel (DidAutoStop false)

            async {
                let! acquisitionResult = waitToFinish
                return! finishAcquisition picoScope acquisition acquisitionResult }
        
        | None -> failwith "Cannot stop an acquisition which has not been started."