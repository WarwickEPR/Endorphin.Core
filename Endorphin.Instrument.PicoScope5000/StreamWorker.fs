namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Runtime.InteropServices
open System.Reactive.Linq
open System
open log4net
open FSharp.Charting
open System.Threading

type ChannelStream = 
    { inputSettings : InputSettings
      downsamplingModes : Set<DownsamplingMode> }

type PicoScope5000Stream =
    { sampleInterval : int<ns> 
      downsampling : uint32 option
      streamStop : StreamStop
      triggerSettings : TriggerSettings
      activeChannels : Map<Channel, ChannelStream>
      memorySegment : uint32 }

type PicoScope5000Stream with
    static member NoDownsampling : uint32 option = None
    static member DownsamplingRatio (ratio : uint32) = Some ratio

type StreamStatus =
    | PreparingStream
    | ReadyToStream
    | Streaming of hardwareSampleInterval : int<ns>
    | FinishedStream of didAutoStop : bool
    | CanceledStream
    | FailedStream

type StreamStopOptions =
    { didAutoStop : bool }

type SampleBlock =
    { samples : Map<Channel * BufferDownsampling, Sample array>
      length : int
      voltageOverflows : Set<Channel> }

type StreamWorker(pico : PicoScope5000, stream) =
    static let log = LogManager.GetLogger typeof<StreamWorker>

    let statusChanged = new Event<StreamStatus>()
    let success = new Event<unit>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let sampleBlock = new Event<SampleBlock>()

    let readyToStart = new ManualResetHandle(false)
    let cancellationCapability = new CancellationCapability()
    let stopCapability = new CancellationCapability<StreamStopOptions>()

    let downsamplingRatio =
        match stream.downsampling with
        | Some value -> value
        | None -> 1u

    let bufferLength =
        let minLength = 256.0 * 1e9 / (float (int stream.sampleInterval * int downsamplingRatio))
        let rec computeBufferLength length = 
            if (float) length > minLength
            then length
            else computeBufferLength (2 * length)

        computeBufferLength 1

    let activeChannelSet =
        seq { for channel in stream.activeChannels -> channel.Key }
        |> Set.ofSeq

    let downsamplingModes =
        seq { for channel in stream.activeChannels -> channel.Value }
        |> Seq.collect (fun channelStream -> Set.toSeq channelStream.downsamplingModes)
        |> Seq.fold (|||) DownsamplingMode.None // combine the required types of downsampling using logical OR
    
    let streamingParameters = 
        { sampleInterval = stream.sampleInterval
          streamStop = stream.streamStop
          downsamplingRatio = downsamplingRatio
          downsamplingModes = downsamplingModes
          bufferLength = uint32 bufferLength }

    do
        if stream.activeChannels.Count = 0 then
            invalidArg "stream.activeChannels"
                "Set of active channels must be non-empty." stream.activeChannels

        if stream.downsampling.IsNone && downsamplingModes <> DownsamplingMode.None then
            invalidArg "stream.downsampling"
                "A downsampling ratio must be specified for a stream where an active channel uses downsampling." stream.downsampling

        if stream.downsampling.IsSome && downsamplingModes = DownsamplingMode.None then
            invalidArg "stream.downsampling"
                "A downsampling ratio was specified but none of the active channels use downsampling." stream.downsampling

        stream.activeChannels
        |> Map.iter (fun channel channelStream ->
            if channelStream.downsamplingModes.Count = 0 then
                invalidArg "stream.activeChannels"
                    "List downsampling modes for active channel must be non-empty." (channel, channelStream)
            
            if downsamplingModes <> DownsamplingMode.None && channelStream.downsamplingModes.Contains DownsamplingMode.None then
                invalidArg "stream.activeChannels"
                    "DownsamplingMode.None cannot be used in a stream which uses another form of downsampling." (channel, channelStream))
    
    member __.StatusChanged = statusChanged.Publish
    member __.Success = success.Publish
    member __.Error = error.Publish
    member __.Canceled = canceled.Publish
    member __.SampleSlice =
        sampleBlock.Publish
        |> Event.collectSeq (fun block -> seq { 
            for index in 0 .. block.length - 1 ->
                block.samples
                |> Map.map (fun _ buffer -> buffer.[index]) })

    member __.Sample(channel, buffer) =
        sampleBlock.Publish
        |> Event.collectSeq (fun block ->
            block.samples.[channel, buffer]
            |> Array.toSeq)
    
    member __.VoltageOverflow channel =
        sampleBlock.Publish
        |> Event.choose (fun block ->
            if block.voltageOverflows.Contains channel then Some () else None)

    member this.Chart(channel, buffer, refreshInterval : TimeSpan) =
        (this.Sample(channel, buffer)
         |> Event.scan (fun chartData x ->
             match chartData with
             | [] -> [(0, x)]
             | (last, _) :: rest -> (last + 1, x) :: chartData) List.empty)
         .Sample(refreshInterval)
         .ObserveOn(SynchronizationContext.CaptureCurrent())
        |> LiveChart.FastLine

    member this.ChartXY(channelX, bufferX, channelY, bufferY, refreshInterval : TimeSpan) =
        (sampleBlock.Publish
         |> Event.scan (fun chartData block ->
             let samples = seq {
                 for index in 0 .. block.length - 1 ->
                     (block.samples.[channelX, bufferX].[index],
                      block.samples.[channelY, bufferY].[index]) }
             Seq.fold (fun data sample -> sample :: data) chartData samples) List.empty)
         .Sample(refreshInterval)
         .ObserveOn(SynchronizationContext.CaptureCurrent())
        |> LiveChart.FastLine
    
    member this.ChartAll (refreshInterval : TimeSpan) =
        let sampler = Observable.Interval refreshInterval
        let channelBuffers = seq {
            for (channel, channelStream) in (Map.toSeq stream.activeChannels) do
                for downsamplingMode in channelStream.downsamplingModes do
                    match downsamplingMode with
                    | DownsamplingMode.None -> yield (channel, BufferDownsampling.AllSamples)
                    | DownsamplingMode.Averaged -> yield (channel, BufferDownsampling.Averaged)
                    | DownsamplingMode.Decimated -> yield (channel, BufferDownsampling.Decimated)
                    | DownsamplingMode.Aggregate -> 
                        yield (channel, BufferDownsampling.AggregateMin)
                        yield (channel, BufferDownsampling.AggregateMax)
                    | _ -> invalidArg "downsamplingMode" "Invalid DownsamplingMode" downsamplingMode }
        seq { 
            for (channel, buffer) in channelBuffers ->
                ((this.Sample(channel, buffer)
                  |> Event.scan (fun chartData x ->
                     match chartData with
                     | [] -> [(0, x)]
                     | (last, _) :: rest -> (last + 1, x) :: chartData) List.empty)
                  .Sample(refreshInterval)
                  .ObserveOn(SynchronizationContext.CaptureCurrent()))
                |> LiveChart.FastLine }
        |> Chart.Combine

    member __.Stop() =
        // avoid stopping the stream manually if it is already stopping automatically
        if not stopCapability.IsCancellationRequested then
            "Stream worker stopping manually." |> log.Info
            cancellationCapability.Cancel()
            stopCapability.Cancel { didAutoStop = false }
            readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member __.SetReadyToStart() =
        "Setting stream worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member __.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare stream as cancellation was already requested."

        "Stream worker preparing." |> log.Info
        sprintf "Active channels: %A." stream.activeChannels |> log.Info
        sprintf "Sample interval: %d ns." (int stream.sampleInterval) |> log.Info
        sprintf "Trigger settings: %A." stream.triggerSettings |> log.Info
        sprintf "Stream stop: %A." stream.streamStop |> log.Info

        let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()
        
        let streamWorkflow buffers = async {
            // this callback will be sent to the PicoScope and called when the driver has written new data to the buffer
            let dataCallback (streamingValues : StreamingValuesReady) =
                if not stopCapability.IsCancellationRequested then
                    if streamingValues.didAutoStop then
                        stopCapability.Cancel { didAutoStop = true }

                    if streamingValues.numberOfSamples > 0 then
                        let processSamples = 
                            buffers
                            |> Map.toSeq
                            |> Seq.map (fun ((channel, bufferDescription), buffer) -> async {
                                let samples = Array.zeroCreate streamingValues.numberOfSamples
                                Array.Copy(buffer, int streamingValues.startIndex, samples, 0, streamingValues.numberOfSamples)
                                return ((channel, bufferDescription), samples) })
                            |> Async.Parallel
                        async {
                            sprintf "Received stream data block of %d samples" streamingValues.numberOfSamples |> log.Info
                            sprintf "Stream did %s." (if streamingValues.didAutoStop then "auto-stop" else "not auto-stop") |> log.Info
                            let! samples = processSamples
                            syncContext.RaiseEvent sampleBlock
                                { samples = samples |> Map.ofSeq
                                  length = streamingValues.numberOfSamples
                                  voltageOverflows = streamingValues.voltageOverflows }}
                        |> Async.Start
  
            // this loop polls the PicoScope for the latest stream values
            let rec pollLoop (acquisition : IStreamingAcquisition) = async {
                "Polling PicoScope for stream values..." |> log.Info
                do! acquisition.GetLatestValues dataCallback
                do! Async.Sleep 100
                if not stopCapability.IsCancellationRequested then
                    do! pollLoop acquisition }

            "Initiating streaming..." |> log.Info
            use! acquisition = pico.RunStreamingAsync streamingParameters
                
            sprintf "Started streaming with sammple interval: %A ns." acquisition.SampleInterval |> log.Info
            syncContext.RaiseEvent statusChanged (Streaming acquisition.SampleInterval)

            "Starting poll loop." |> log.Info
            do! pollLoop acquisition
            
            let didAutoStop = stopCapability.Options.didAutoStop
            sprintf "Stream finished successfully %s auto-stop." (if didAutoStop then "with" else "without") |> log.Info
            syncContext.RaiseEvent statusChanged (FinishedStream didAutoStop) }

        let startupWorkflow = async {
            let setupChannels = async {
                "Setting up stream channels." |> log.Info
                pico.SetTrigger stream.triggerSettings
                let! availableChannels = pico.GetAvailableChannelsAsync()
                if not (activeChannelSet.IsSubsetOf availableChannels) then
                    failwith "Some input channels required by the stream are not available on the PicoScope."
                
                for channel in availableChannels do
                    match channel with
                    | channel when stream.activeChannels.ContainsKey channel ->
                        let inputSettings = stream.activeChannels.[channel].inputSettings
                        pico.SetChannelSettings(channel, Enabled inputSettings)
                    | channel ->
                        pico.SetChannelSettings(channel, Disabled) }
            
            let createBuffers = async {
                "Creating stream buffers." |> log.Info
                return stream.activeChannels
                |> Map.toSeq
                |> Seq.collect (fun (channel, channelStream) -> seq {
                    for downsampling in channelStream.downsamplingModes do
                        match downsampling.Buffer() with
                        | Single bufferDescription -> 
                            let buffer = Array.zeroCreate bufferLength
                            pico.SetDataBuffer(channel, buffer, stream.memorySegment, downsampling)
                            yield ((channel, bufferDescription), buffer) 
                        | Pair (bufferMaxDescription, bufferMinDescription) ->
                            let bufferMax = Array.zeroCreate bufferLength
                            let bufferMin = Array.zeroCreate bufferLength
                            pico.SetAggregateDataBuffers(channel, bufferMax, bufferMin, stream.memorySegment)
                            yield ((channel, bufferMaxDescription), bufferMax)
                            yield ((channel, bufferMinDescription), bufferMin) })
                |> Map.ofSeq }

            let awaitReadyForStream = async {
                "Waiting for ready-to-stream signal..." |> log.Info
                syncContext.RaiseEvent statusChanged ReadyToStream
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore }
                
            "Preparing for stream acquisition." |> log.Info
            syncContext.RaiseEvent statusChanged PreparingStream
            
            use! __ = Async.OnCancel (fun () -> 
                pico.DiscardDataBuffers()
                
                "Stream canceled before starting." |> log.Info
                syncContext.RaiseEvent statusChanged CanceledStream)

            do! setupChannels
            let! buffers = createBuffers
            do! awaitReadyForStream
            
            Async.StartWithContinuations(
                streamWorkflow buffers,
                (fun () -> syncContext.RaiseEvent success ()),
                (fun exn ->
                    stopCapability.Cancel { didAutoStop = false }
                    log.Error (sprintf "Stream failed during acquisition acquisition due to error %A." exn, exn)
                    syncContext.RaiseEvent statusChanged FailedStream
                    syncContext.RaiseEvent error exn),
                ignore) } // no cancellation token so no cancellation continuation
        
        "Starting stream workflow." |> log.Info 
        Async.StartWithContinuations(
            startupWorkflow,
            (ignore), // no continuation unless there is an error or cancellation
            (fun exn ->
                log.Error ((sprintf "Stream failed before starting due to error %A." exn), exn)
                syncContext.RaiseEvent statusChanged FailedStream
                syncContext.RaiseEvent error exn),
            (fun exn -> syncContext.RaiseEvent canceled exn), // canceled event won't be raised unless the stream is stopped before it starts running
            cancellationCapability.Token)