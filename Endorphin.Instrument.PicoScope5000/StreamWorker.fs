namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Utils
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices
open System.Threading
open System

type StreamingParmaeters =
    { sampleInterval : float<s> 
      downsamplingRatio : uint32
      streamStop : StreamStop
      triggerSettings : TriggerSettings
      activeChannels : (Channel * InputSettings * BandwidthLimit) list
      channelStreams : ChannelData list
      channelAggregateStreams : ChannelAggregateData list
      memorySegment : uint32 }

type StreamStatus =
    | Preparing
    | Started of hardwareSampleInterval : float<s>
    | Finished of didAutoStop : bool
    | Stopping
    
type StreamStopCapability() =
    let stopCapability = new CancellationTokenSource()
    let started = ref false

    member this.IsCancellationRequested = stopCapability.IsCancellationRequested

    member this.Stop() =
        if not !started then
            failwith "Cannot stop stream before it has been started."
        stopCapability.Cancel()

    member this.StartUsingToken() =
        started := true 
        stopCapability.Token

type StreamWorker(stream, pico : PicoScope5000) =
    let statusChanged = new Event<StreamStatus>()
    let completed = new Event<unit>()
    let canceled = new Event<OperationCanceledException>()
    let error = new Event<Exception>()
   
    let readyToStart = new ManualResetEvent(false)
    let stopCapbility = new StreamStopCapability()

    let bufferLength =
        let minLength = 1e2 / (float (stream.sampleInterval * float stream.downsamplingRatio))
        let rec computeBufferLength length = 
            if (float) length > minLength
            then length
            else computeBufferLength (2 * length)

        computeBufferLength 1

    let downsampling = 
        let withAggregate = 
            if List.isEmpty stream.channelAggregateStreams then Downsampling.None
            else Downsampling.Aggregate

        stream.channelStreams
        |> List.map (fun channelStream -> channelStream.Downsampling)
        |> List.fold (|||) withAggregate

    let streamChannelSet =
        stream.channelStreams
        |> List.map (fun channelStream -> channelStream.Channel)
        |> Set.ofList

    let streamAggregateChannelSet =
        stream.channelAggregateStreams
        |> List.map (fun channelAggregateStream -> channelAggregateStream.Channel)
        |> Set.ofList

    let activeChannelsSet =
        stream.activeChannels
        |> List.map (fun (channel, _, _) -> channel)
        |> Set.ofList

    // do initialisation checks : channels etc.
    do
        if (Set.intersect streamChannelSet streamAggregateChannelSet) <> activeChannelsSet then
            invalidArg "stream.activeChannels"
                "Set of active channels does not match the set of streamed channels." stream.activeChannels

        if stream.activeChannels.Length <> activeChannelsSet.Count then
            invalidArg "stream.activeChannels"
                "Set of active channels contains multiple entires for some channel(s)." stream.activeChannels

    [<CLIEvent>]
    member this.StatusChanged = statusChanged.Publish

    [<CLIEvent>]
    member this.Error = error.Publish

    [<CLIEvent>]
    member this.Canceled = canceled.Publish

    [<CLIEvent>]
    member this.Completed = completed.Publish

    member this.Stop() =
        stopCapbility.Stop()
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting
        let syncContext = SynchronizationContext.CaptureCurrent()
        syncContext.RaiseEvent statusChanged Stopping

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        readyToStart.Set() |> ignore

    member this.Prepare() =
        let syncContext = SynchronizationContext.CaptureCurrent()

        let workflow = 
            let setupChannels = async {
                pico.SetTrigger(stream.triggerSettings)

                for (channel, inputSettings, bandwidthLimit) in stream.activeChannels do
                    pico.SetChannelSettings(channel, Enabled(inputSettings))
                    pico.SetChannelBandwidth(channel, bandwidthLimit)
            
                let inactiveChannels = 
                    stream.activeChannels
                    |> List.map (fun (channel, _, _) -> channel)
                    |> Set.ofList
                    |> Set.difference (Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ])

                for channel in inactiveChannels do
                    pico.DisableChannel(channel) }

            let createBuffers = async {
                return 
                    stream.channelStreams 
                    |> List.map (fun channelStream ->
                        let dataBuffer = Array.zeroCreate(bufferLength)
                        let gcHandle = GCHandle.Alloc(dataBuffer, GCHandleType.Pinned)
                        pico.SetDataBuffer(channelStream.Channel, dataBuffer, stream.memorySegment, downsampling)

                        { dataBuffer = dataBuffer
                          gcHandle = gcHandle
                          channelStream = channelStream }) }
            
            let createAggregateBuffers = async {
                return 
                    stream.channelAggregateStreams 
                    |> List.map (fun channelAggregateStream ->
                        let dataBufferMax = Array.zeroCreate(bufferLength)
                        let dataBufferMin = Array.zeroCreate(bufferLength)
                        let gcHandleMax = GCHandle.Alloc(dataBufferMax, GCHandleType.Pinned)
                        let gcHandleMin = GCHandle.Alloc(dataBufferMin, GCHandleType.Pinned)
                        pico.SetAggregateDataBuffers(
                            channelAggregateStream.Channel, dataBufferMax, dataBufferMin, stream.memorySegment)

                        { dataBufferMax = dataBufferMax
                          dataBufferMin = dataBufferMin
                          gcHandleMax = gcHandleMax
                          gcHandleMin = gcHandleMin
                          channelAggregateStream = channelAggregateStream }) } 

            let runStreaming = async {
                let! hardwareSampleInterval =
                    pico.RunStreamingAsync(stream.sampleInterval, stream.streamStop, stream.downsamplingRatio, downsampling, uint32 bufferLength)
            
                Started(hardwareSampleInterval) |> statusChanged.Trigger }

            let finishStream buffers aggregateBuffers didAutoStop = async {
                do! pico.StopAcquisitionAsync()

                for buffer in buffers do
                    buffer.gcHandle.Free()
                    buffer.channelStream.TriggerCompleted()

                for buffer in aggregateBuffers do
                    buffer.gcHandleMax.Free()
                    buffer.gcHandleMin.Free()
                    buffer.channelAggregateStream.TriggerCompleted()

                Finished(didAutoStop) |> statusChanged.Trigger
                completed.Trigger() } 

            let rec pollLatestValues buffers aggregateBuffers = 
                let dataCallback streamingValues =
                    if streamingValues.numberOfSamples > 0 then
                        let channelDidOverflow channel =
                            streamingValues.voltageOverflows
                            |> List.exists (fun overflowChannel -> channel = overflowChannel)

                        for buffer in buffers do
                            buffer.channelStream.ProcessDataReadyAsync(
                                buffer.dataBuffer, streamingValues.startIndex, streamingValues.numberOfSamples, 
                                channelDidOverflow buffer.channelStream.Channel, streamingValues.triggerPosition)
                            |> Async.Start

                        for buffer in aggregateBuffers do
                            buffer.channelAggregateStream.ProcessDataReadyAsync(
                                buffer.dataBufferMax, buffer.dataBufferMin, streamingValues.startIndex, streamingValues.numberOfSamples, 
                                channelDidOverflow buffer.channelAggregateStream.Channel, streamingValues.triggerPosition)
                            |> Async.Start

                        if streamingValues.didAutoStop then
                            (finishStream buffers aggregateBuffers true) 
                            |> Async.Start

                let rec pollLoop() = async {
                    pico.GetStreamingLatestValues(dataCallback)
                    do! Async.Sleep(100)
                    do! pollLoop() }

                async {
                    use! cancelHandler = Async.OnCancel(fun () -> 
                        (finishStream buffers aggregateBuffers false) |> Async.Start)
                
                    do! pollLoop() }

            async {
                Preparing |> statusChanged.Trigger

                do! setupChannels
                let! buffers = createBuffers
                let! aggregateBuffers = createAggregateBuffers
                do! runStreaming

                let! stopToken = Async.CancellationToken
                Async.Start(pollLatestValues buffers aggregateBuffers, stopToken) }

        Async.StartWithContinuations
            (workflow,
                (ignore), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> syncContext.RaiseEvent canceled exn),
                stopCapbility.StartUsingToken())