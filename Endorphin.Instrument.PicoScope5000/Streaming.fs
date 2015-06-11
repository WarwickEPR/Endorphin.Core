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
        private { Parameters             : StreamingParameters
                  DataBuffers            : AcquisitionBuffers
                  StopCapability         : CancellationCapability<StreamStopOptions>
                  StatusChanged          : Event<StreamStatus>
                  SampleBlockObserved    : Event<SampleBlock> }

    let create streamingParameters =
        { Parameters  = streamingParameters 
          DataBuffers = 
            Acquisition.Buffers.allocateAcquisitionBuffers 
            <| streamingParameters.MemorySegment
            <| streamingParameters.BufferLength
            <| streamingParameters.Inputs
          StopCapability         = new CancellationCapability<StreamStopOptions>()
          StatusChanged          = new Event<StreamStatus>()
          SampleBlockObserved    = new Event<SampleBlock>() }
          
    let stop acquisition =
        if not acquisition.StopCapability.IsCancellationRequested then
            acquisition.StopCapability.Cancel (DidAutoStop false)

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

    let takeUntilFinished acquisition (obs : IObservable<'T>) =
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
        choice {
            acquisition.StatusChanged.Trigger PreparingStream
            do! PicoScope.ChannelSettings.setAcquisitionInputChannels picoScope acquisition.Parameters.Inputs
            do! PicoScope.Triggering.setTriggerSettings picoScope acquisition.Parameters.TriggerSettings

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
            
            do! Async.Sleep 100
            do! pollUntilFinished picoScope acquisition }

    let finishAcquisition picoScope acquisition =
        match PicoScope.Acquisition.stop picoScope with
        | Success ()      -> acquisition.StatusChanged.Trigger (FinishedStream acquisition.StopCapability.Options)
        | Failure message -> acquisition.StatusChanged.Trigger (FailedStream message)
        
    let start picoScope acquisition = asyncChoice {
        try
            use __ = Acquisition.Buffers.pinningHandle acquisition.DataBuffers
            do! prepareDevice picoScope acquisition 
            do! startStreaming picoScope acquisition
            do! pollUntilFinished picoScope acquisition
        finally
            finishAcquisition picoScope acquisition }