namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices
open System
open log4net

type StreamingParmaeters =
    { sampleInterval : int<ns> 
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
    | Streaming of hardwareSampleInterval : int<ns>
    | Finished of didAutoStop : bool

type StreamStopOptions = {
    didAutoStop : bool }

type StreamWorker(pico : PicoScope5000, stream) =
    static let log = LogManager.GetLogger typeof<StreamWorker>

    let statusChanged = new Event<StreamStatus>()
    let completed = new Event<unit>()
    let cancelling = new Event<OperationCanceledException>()
    let error = new Event<Exception>()
    let workerFinished = new Event<unit>()
   
    let readyToStart = new ManualResetHandle(false)
    let stopCapbility = new CancellationCapability<StreamStopOptions>()
    let cancellationCapability = new CancellationCapability()

    let bufferLength =
        let minLength = 256.0 * 1e9 / (float (int stream.sampleInterval * int stream.downsamplingRatio))
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

    member this.StatusChanged = statusChanged.Publish
    member this.Error = error.Publish
    member this.Cancelling = cancelling.Publish
    member this.Completed = completed.Publish

    member this.Stop() =
        "Stream worker stoppping." |> log.Info
        cancellationCapability.Cancel()
        stopCapbility.Cancel { didAutoStop = false }
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting stream worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare stream as cancellation was already requested."

        "Stream worker preparing." |> log.Info
        (sprintf "Active channels: %A." streamChannelSet) |> log.Info
        (sprintf "Sample interval: %dns." (int stream.sampleInterval)) |> log.Info
        (sprintf "Trigger settings: %A." stream.triggerSettings) |> log.Info
        (sprintf "Stream stop: %A." stream.streamStop) |> log.Info

        let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

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
                return stream.channelStreams 
                |> List.map (fun channelStream ->
                   let dataBuffer = Array.zeroCreate(bufferLength)
                   pico.SetDataBuffer(channelStream.Channel, dataBuffer, stream.memorySegment, downsampling)

                   { dataBuffer = dataBuffer
                     channelStream = channelStream }) }
            
            let createAggregateBuffers = async {
                "Creating stream aggregate buffers." |> log.Info
                return stream.channelAggregateStreams 
                |> List.map (fun channelAggregateStream ->
                    let dataBufferMax = Array.zeroCreate(bufferLength)
                    let dataBufferMin = Array.zeroCreate(bufferLength)
                    pico.SetAggregateDataBuffers(
                        channelAggregateStream.Channel, dataBufferMax, dataBufferMin, stream.memorySegment)

                    { dataBufferMax = dataBufferMax
                      dataBufferMin = dataBufferMin
                      channelAggregateStream = channelAggregateStream }) } 

            let awaitReadyForStream = async {
                "Waiting for ready-to-stream signal..." |> log.Info
                syncContext.RaiseEvent statusChanged ReadyToStream
                do! Async.AwaitWaitHandle(readyToStart) |> Async.Ignore
                "Received ready-to-stream signal" |> log.Info }

            let rec pollLatestValues buffers aggregateBuffers (acquisition : IDisposable) = 
                let finishStream() =
                    "Stream worker finishing stream." |> log.Info
                    acquisition.Dispose()

                    for buffer in buffers do
                        (sprintf "Triggering completed event on buffer %A." buffer) |> log.Info
                        buffer.channelStream.TriggerCompleted()

                    for buffer in aggregateBuffers do
                        (sprintf "Triggering completed event on buffer %A." buffer) |> log.Info
                        buffer.channelAggregateStream.TriggerCompleted()

                    (sprintf "Stream worker finished stream %s." 
                        (if stopCapbility.Options.didAutoStop then "automatically" else "manually")) |> log.Info

                    syncContext.RaiseEvent statusChanged (Finished stopCapbility.Options.didAutoStop)
                    workerFinished.Trigger()

                let handleError (exn : Exception) =
                    (sprintf "Stopping streaming acquisition due to error %A.\nStack trace:%s\n" exn exn.StackTrace) |> log.Error
                    acquisition.Dispose()
                    
                    for buffer in buffers do
                        (sprintf "Triggering error event on buffer %A." buffer) |> log.Info
                        buffer.channelStream.TriggerError(exn)

                    for buffer in aggregateBuffers do
                        (sprintf "Triggering error event on buffer %A." buffer) |> log.Info
                        buffer.channelAggregateStream.TriggerError(exn)

                    syncContext.RaiseEvent error exn
                    workerFinished.Trigger()

                let dataCallback streamingValues =
                    (sprintf "Received stream data block of %d samples" streamingValues.numberOfSamples) |> log.Info
                    (sprintf "Stream did %s." (if streamingValues.didAutoStop then "auto-stop" else "not auto-stop")) |> log.Info
                    if streamingValues.didAutoStop then
                        stopCapbility.Cancel { didAutoStop = true }

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
                    use! cancelHandler = 
                        Async.OnCancel(finishStream)  
                                    
                    try 
                        do! pollLoop() 
                    with
                        | :? OperationCanceledException as exn -> raise exn
                        | exn -> handleError exn }

            let runStreaming buffers aggregateBuffers = async {
                "Initiating streaming..." |> log.Info
                let! (hardwareSampleInterval, acquisition) =
                    pico.RunStreamingAsync(stream.sampleInterval, stream.streamStop, stream.downsamplingRatio, downsampling, uint32 bufferLength)
                
                (sprintf "Started streaming with sammple interval: %Ans." hardwareSampleInterval) |> log.Info
                syncContext.RaiseEvent statusChanged (Streaming hardwareSampleInterval)

                "Starting poll loop." |> log.Info
                Async.Start(
                    pollLatestValues buffers aggregateBuffers acquisition,
                    stopCapbility.Token)  }

            async {
                use! cancelHandler = Async.OnCancel(pico.DiscardDataBuffers)
                
                "Preparing for stream acquisition." |> log.Info
                syncContext.RaiseEvent statusChanged Preparing

                do! setupChannels
                let! buffers = createBuffers
                let! aggregateBuffers = createAggregateBuffers
                do! awaitReadyForStream
                do! runStreaming buffers aggregateBuffers }

        async {
            "Listening for worker finished event." |> log.Info
            let! waitToFinish =
                workerFinished.Publish
                |> Async.AwaitEvent
                |> Async.StartChild

            "Starting stream workflow." |> log.Info 
            Async.StartWithContinuations(
                workflow,
                (ignore), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> 
                    syncContext.RaiseEvent cancelling exn
                    syncContext.RaiseEvent workerFinished ()), // canceled event won't be raised unless the stream is stopped before it starts running
                cancellationCapability.Token)
            
            "Waiting for worker to finish." |> log.Info
            do! waitToFinish 
            "Stream completed." |> log.Info
            syncContext.RaiseEvent completed () }
        |> Async.Start