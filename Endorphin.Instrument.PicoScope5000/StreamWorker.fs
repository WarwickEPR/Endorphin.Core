namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Utils
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices
open System.Threading
open System
open log4net

type StreamingParmaeters =
    { sampleInterval : float<s> 
      downsamplingRatio : uint32
      streamStop : StreamStop
      triggerSettings : TriggerSettings
      activeChannels : (Channel * InputSettings) list
      channelStreams : ChannelData list
      channelAggregateStreams : ChannelAggregateData list
      memorySegment : uint32 }

type StreamStatus =
    | Preparing
    | ReadyToStream
    | Started of hardwareSampleInterval : float<s>
    | Finished of didAutoStop : bool
    | Stopping
    
type StreamStopCapability() =
    static let log = LogManager.GetLogger typeof<StreamStopCapability> 

    let stopCapability = new CancellationTokenSource()
    let started = ref false

    member this.IsCancellationRequested = stopCapability.IsCancellationRequested

    member this.Stop() =
        if not !started then
            failwith "Cannot stop stream before it has been started."
        "Cancelling stream token." |> log.Info
        stopCapability.Cancel()

    member this.StartUsingToken() =
        "Started using stream token." |> log.Info
        started := true 
        stopCapability.Token

type StreamWorker(pico : PicoScope5000, stream) =
    static let log = LogManager.GetLogger typeof<StreamWorker>

    let statusChanged = new Event<StreamStatus>()
    let completed = new Event<unit>()
    let canceled = new Event<OperationCanceledException>()
    let error = new Event<Exception>()
   
    let readyToStart = new ManualResetEvent(false)
    let stopCapbility = new StreamStopCapability()

    let bufferLength =
        let minLength = 256.0 / (float (stream.sampleInterval * float stream.downsamplingRatio))
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
        |> List.map (fun (channel, _) -> channel)
        |> Set.ofList

    do
        if (Set.union streamChannelSet streamAggregateChannelSet) <> activeChannelsSet then
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
        "Stream worker stoppping." |> log.Info
        stopCapbility.Stop()
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting
        let syncContext = SynchronizationContext.CaptureCurrent()
        syncContext.RaiseEvent statusChanged Stopping

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting stream worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        (sprintf "Active channels: %A." streamChannelSet) |> log.Info
        (sprintf "Sample interval: %A." stream.sampleInterval) |> log.Info
        (sprintf "Trigger settings: %A." stream.triggerSettings) |> log.Info
        (sprintf "Stream stop: %A." stream.streamStop) |> log.Info

        let syncContext = SynchronizationContext.CaptureCurrent()

        let workflow = 
            let setupChannels = async {
                "Setting up stream channels." |> log.Info
                pico.SetTrigger(stream.triggerSettings)
                let! availableChannels = pico.GetAvailableChannelsAsync()
                if not (activeChannelsSet.IsSubsetOf(availableChannels)) then
                    failwith "Some input channels required by the stream are not available on the PicoScope."
                
                for channel in availableChannels do
                    match channel with
                    | channel when activeChannelsSet.Contains(channel) ->
                        stream.activeChannels
                        |> List.map (fun (channel, inputSettings) -> (channel, Enabled(inputSettings)))
                        |> List.find (fun (activeChannel, _) -> activeChannel = channel)
                        |> pico.SetChannelSettings
                    | channel ->
                        pico.DisableChannel(channel) }

            let createBuffers = async {
                "Creating stream buffers." |> log.Info
                return 
                    stream.channelStreams 
                    |> List.map (fun channelStream ->
                        let dataBuffer = Array.zeroCreate(bufferLength)
                        pico.SetDataBuffer(channelStream.Channel, dataBuffer, stream.memorySegment, downsampling)

                        { dataBuffer = dataBuffer
                          channelStream = channelStream }) }
            
            let createAggregateBuffers = async {
                "Creating stream aggregate buffers." |> log.Info
                return 
                    stream.channelAggregateStreams 
                    |> List.map (fun channelAggregateStream ->
                        let dataBufferMax = Array.zeroCreate(bufferLength)
                        let dataBufferMin = Array.zeroCreate(bufferLength)
                        pico.SetAggregateDataBuffers(
                            channelAggregateStream.Channel, dataBufferMax, dataBufferMin, stream.memorySegment)

                        { dataBufferMax = dataBufferMax
                          dataBufferMin = dataBufferMin
                          channelAggregateStream = channelAggregateStream }) } 

            let awaitReadyForStream = async {
                if not stopCapbility.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged ReadyToStream
                let! ready = Async.AwaitWaitHandle(readyToStart)
                ready |> ignore }

            let runStreaming = async {
                "Initiating streaming..." |> log.Info
                let! (hardwareSampleInterval, cancellationCallback) =
                    pico.RunStreamingAsync(stream.sampleInterval, stream.streamStop, stream.downsamplingRatio, downsampling, uint32 bufferLength)
            
                (sprintf "Started streaming with sammple interval: %A." hardwareSampleInterval) |> log.Info
                Started(hardwareSampleInterval) |> statusChanged.Trigger
                return cancellationCallback }

            let rec pollLatestValues buffers aggregateBuffers stopCallback = 
                let finishStream buffers aggregateBuffers didAutoStop = async {
                    "Stream worker finishing stream." |> log.Info
                    stopCallback()

                    for buffer in buffers do
                        (sprintf "Triggering completed event on buffer %A." buffer) |> log.Info
                        buffer.channelStream.TriggerCompleted()

                    for buffer in aggregateBuffers do
                        (sprintf "Triggering completed event on buffer %A." buffer) |> log.Info
                        buffer.channelAggregateStream.TriggerCompleted()

                    Finished(didAutoStop) |> statusChanged.Trigger
                    completed.Trigger() } 

                let dataCallback streamingValues =
                    (sprintf "Received stream data block of %d samples" streamingValues.numberOfSamples) |> log.Info
                    (sprintf "Stream did %s." (if streamingValues.didAutoStop then "auto-stop" else "not auto-stop")) |> log.Info
                    if streamingValues.didAutoStop then
                        stopCapbility.Stop()

                    if streamingValues.numberOfSamples > 0 then
                        let channelDidOverflow channel =
                            streamingValues.voltageOverflows
                            |> Set.contains channel

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

                let rec pollLoop() = async {
                    "Polling PicoScope for stream values..." |> log.Info
                    pico.GetStreamingLatestValues(dataCallback)
                    do! Async.Sleep(100)
                    do! pollLoop() }

                async {
                    use! cancelHandler = Async.OnCancel(fun () -> 
                        (finishStream buffers aggregateBuffers false) |> Async.Start)
                
                    do! pollLoop() }

            async {
                "Preparing stream worker for acquisition." |> log.Info
                Preparing |> statusChanged.Trigger

                do! setupChannels
                let! buffers = createBuffers
                let! aggregateBuffers = createAggregateBuffers
                do! awaitReadyForStream
                let! stopCallback = runStreaming

                let! stopToken = Async.CancellationToken
                Async.Start(pollLatestValues buffers aggregateBuffers stopCallback, stopToken) }

        Async.StartWithContinuations
            (workflow,
                (ignore), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> syncContext.RaiseEvent canceled exn),
                stopCapbility.StartUsingToken())