﻿namespace Endorphin.Instrument.PicoScope5000

open System
open Errors
open System.Reactive
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Runtime.InteropServices
open System.Threading

type StreamingInterval =
    | Seconds of uint32
    | Milliseconds of uint32
    | Microseconds of uint32
    | Nanoseconds of uint32
    | Picoseconds of uint32
    | Femtoseconds of uint32

type StreamingParmaeters =
    { streamingInterval : StreamingInterval 
      downsamplingRatio : uint32
      maximumPreTriggerSamples : uint32
      maximumPostTriggerSamples : uint32
      autoStop : bool }

type StreamAgentStatus =
    | Active
    | Discarded

type StreamStatus =
    | Preparing
    | Started of streamingInterval : StreamingInterval
    | Finished of didAutoStop : bool

type BufferDataReady =
    { startIndex : uint32
      numberOfSamples : int 
      overflows : Channel seq
      triggered : Triggered }

type internal StreamCommand =
    | Observe of Channel * Downsampling * AsyncReplyChannel<IObservable<int16>>
    | ObserveAggregate of Channel * AsyncReplyChannel<IObservable<int16> * IObservable<int16>>
    | ObserveAgentStatus of AsyncReplyChannel<IObservable<StreamAgentStatus>>
    | RunStream of StreamingParmaeters * AsyncReplyChannel<IObservable<StreamStatus>>
    | StopStream
    | ViewData of AsyncReplyChannel<int16 seq>
    | Discard of AsyncReplyChannel<unit>

type StreamAgent(handle) = 
    static let streamingIntervalToParameters =
        function
        | Seconds(interval) -> (interval, TimeUnit.Seconds)
        | Milliseconds(interval) -> (interval, TimeUnit.Milliseconds)
        | Microseconds(interval) -> (interval, TimeUnit.Microseconds)
        | Nanoseconds(interval) -> (interval, TimeUnit.Nanoseconds)
        | Picoseconds(interval) -> (interval, TimeUnit.Picoseconds)
        | Femtoseconds(interval) -> (interval, TimeUnit.Femtoseconds)

    static let streamingIntervalFromParameters interval timeUnit =
        match timeUnit with
        | TimeUnit.Seconds -> Seconds(interval)
        | TimeUnit.Milliseconds -> Milliseconds(interval)
        | TimeUnit.Microseconds -> Microseconds(interval)
        | TimeUnit.Nanoseconds -> Nanoseconds(interval)
        | TimeUnit.Picoseconds -> Picoseconds(interval)
        | TimeUnit.Femtoseconds -> Femtoseconds(interval)
        | _ -> failwith "Unexpected time unit."
    
    let streamingBufferLength streamingInterval (downsamplingRatio : uint32) =
        let minSampleCount = 
            streamingInterval 
            |> function
            | Seconds(timeInterval) -> 1e2 / (float (timeInterval * downsamplingRatio))
            | Milliseconds(timeInterval) -> 1e5 / (float (timeInterval * downsamplingRatio))
            | Microseconds(timeInterval) -> 1e8 / (float (timeInterval * downsamplingRatio))
            | Nanoseconds(timeInterval) -> 1e11 / (float (timeInterval * downsamplingRatio))
            | Picoseconds(timeInterval) -> 1e14 / (float (timeInterval * downsamplingRatio))
            | Femtoseconds(timeInterval) -> 1e17 / (float (timeInterval * downsamplingRatio))
    
        let rec computeBufferLength length = 
            if (float) length > minSampleCount
            then length
            else computeBufferLength (2 * length)

        computeBufferLength 1

    let streamAgentMailbox =
        let agentStatusSubject = new BehaviorSubject<_>(Active)

        let bufferAgentMailbox (buffer : int16 array) (observer : IObserver<int16>) =
            fun (mailbox : MailboxProcessor<BufferDataReady>) ->
                let rec loop() = async {
                    let! message = mailbox.Receive()
                    let data = Array.zeroCreate(message.numberOfSamples)
                    Array.Copy(buffer, int message.startIndex, data, 0, message.numberOfSamples)
                    for sample in data do
                        observer.OnNext(sample)
                    return! loop() }
            
                loop()

        let acquisition observers aggregateObservers streamingParameters (statusSubject : ISubject<StreamStatus>) cancellationToken =    
            let bufferLength = streamingBufferLength streamingParameters.streamingInterval streamingParameters.downsamplingRatio
        
            let createBuffers = async {
                GC.Collect()
                return Seq.toList <| seq {
                    for (observer, channel, downsampling) in observers do
                        let buffer = Array.zeroCreate(bufferLength)
                        let bufferHandle = GCHandle.Alloc(buffer, GCHandleType.Pinned)
                        Api.SetDataBuffer(handle, channel, buffer, bufferLength, 0u, downsampling) |> checkStatusIsOk
                        let bufferProcessor = MailboxProcessor.Start(bufferAgentMailbox buffer observer)
                        yield (bufferProcessor, buffer, bufferHandle) 
                    for ((maxObserver, minObserver), channel) in aggregateObservers do 
                        let maxBuffer = Array.zeroCreate(bufferLength)
                        let minBuffer = Array.zeroCreate(bufferLength)
                        let maxBufferHandle = GCHandle.Alloc(maxBuffer, GCHandleType.Pinned)
                        let minBufferHandle = GCHandle.Alloc(minBuffer, GCHandleType.Pinned)
                        Api.SetDataBuffers(handle, channel, maxBuffer, minBuffer, bufferLength, 0u, Downsampling.Aggregate) |> checkStatusIsOk
                        let maxBufferProcessor = MailboxProcessor.Start(bufferAgentMailbox maxBuffer maxObserver)
                        let minBufferProcessor = MailboxProcessor.Start(bufferAgentMailbox minBuffer minObserver)
                        yield (maxBufferProcessor, maxBuffer, maxBufferHandle)
                        yield (minBufferProcessor, minBuffer, minBufferHandle) } }

            let runStreaming = async {
                let downsampling = 
                    observers
                    |> List.map(fun (_, _, mode) -> mode)
                    |> List.fold(|||) 
                        (if List.isEmpty aggregateObservers 
                         then Downsampling.None
                         else Downsampling.Aggregate) 

                let (interval, timeUnit) = streamingIntervalToParameters streamingParameters.streamingInterval
                let mutable hardwareInterval = interval
                let autoStop = if streamingParameters.autoStop then 1s else 0s
                Api.RunStreaming(handle, &hardwareInterval, timeUnit, streamingParameters.maximumPreTriggerSamples, 
                    streamingParameters.maximumPostTriggerSamples, autoStop, streamingParameters.downsamplingRatio, downsampling,
                    uint32 bufferLength) |> checkStatusIsOk

                Started(streamingIntervalFromParameters hardwareInterval timeUnit) |> statusSubject.OnNext }

            let finishStream buffers didAutoStop =
                Api.Stop(handle) |> checkStatusIsOk

                for (observer, _, _) in observers do
                    observer.OnCompleted()
                for ((maxObserver, minObserver), _) in aggregateObservers do 
                    maxObserver.OnCompleted()
                    minObserver.OnCompleted()
            
                for (_, _, bufferHandle : GCHandle) in buffers do
                    bufferHandle.Free()

                Finished(didAutoStop) |> statusSubject.OnNext
                statusSubject.OnCompleted()
                        
            let dataCallback buffers = 
                PicoScopeStreamingReady(
                    fun _ numberOfSamples startIndex overflows triggeredAt triggered autoStop _ ->
                        if numberOfSamples > 0
                        then let overflows = 
                                seq { for i in (int Channel.A) .. (int Channel.D) do
                                          if ((1 <<< i) ||| (int overflows)) <> 0 then
                                              yield enum<Channel>(i) }
                             for (bufferProcessor : MailboxProcessor<BufferDataReady>, _, _) in buffers do
                                 { startIndex = startIndex
                                   numberOfSamples = numberOfSamples 
                                   overflows = overflows
                                   triggered = (if triggered = 0s then NotTriggered else Triggered(triggeredAt)) }
                                 |> bufferProcessor.Post
                        if autoStop <> 0s
                        then finishStream buffers true)

            let rec pollLatestValues buffers = 
                let rec pollLoop() = async {
                    Api.GetStreamingLatestValues(handle, dataCallback buffers, IntPtr.Zero) |> checkStatusIsOk
                    do! Async.Sleep(100)
                    do! pollLoop() }

                async {
                    use! cancelHandler = Async.OnCancel(fun () -> finishStream buffers false)
                    do! pollLoop() }

            async {
                let! buffers = createBuffers
                do! runStreaming
                Async.Start(pollLatestValues buffers, cancellationToken) }

        fun (mailbox : MailboxProcessor<StreamCommand>) ->
            let rec preparing observers aggregateObservers = async {
                let! message = mailbox.Receive()
                match message with
                | Observe(channel, downsampling, replyChannel) ->
                    if downsampling = Downsampling.Aggregate
                    then failwith "Attempted to create aggregate observable using Observe command. Use ObserveAggregate instead."
                
                    let subject = new Subject<int16>()
                    subject.AsObservable() |> replyChannel.Reply
                    return! preparing ((subject.AsObserver(), channel, downsampling) :: observers) aggregateObservers

                | ObserveAggregate(channel, replyChannel) ->
                    let maxSubject = new Subject<int16>()
                    let minSubject = new Subject<int16>()
                    (maxSubject.AsObservable(), minSubject.AsObservable()) |> replyChannel.Reply
                    return! preparing observers (((maxSubject.AsObserver(), minSubject.AsObserver()), channel) :: aggregateObservers)
                
                | ObserveAgentStatus(replyChannel) ->
                    agentStatusSubject.AsObservable() |> replyChannel.Reply
                    return! preparing observers aggregateObservers

                | RunStream(streamingParameters, replyChannel) ->
                    let statusSubject = new BehaviorSubject<StreamStatus>(Preparing)
                    statusSubject.AsObservable() |> replyChannel.Reply
                    return! running observers aggregateObservers streamingParameters statusSubject
            
                | StopStream -> failwith "Cannot stop a stream before it has been started."
                | ViewData(_) -> failwith "Cannot request a stream data view before the stream has finished."
                | Discard(replyChannel) -> 
                    agentStatusSubject.OnNext(Discarded)
                    agentStatusSubject.OnCompleted()
                    replyChannel.Reply() }

            and running observers aggregateObservers streamingParameters statusSubject = async {
                use acquisitionCts = new CancellationTokenSource()
                use mailboxCts = new CancellationTokenSource()

                do! acquisition observers aggregateObservers streamingParameters statusSubject acquisitionCts.Token

                use _ = statusSubject.AsObservable()
                        |> Observable.filter(
                            function
                            | Finished(_) -> true
                            | _ -> false)
                        |> Observable.subscribe(fun _ -> mailboxCts.Cancel())

                let rec checkForStopMessage() = async {
                    if mailboxCts.IsCancellationRequested
                    then return! finishedStream()
                    else if mailbox.CurrentQueueLength <> 0
                            then let! message = mailbox.Receive()
                                 match message with
                                 | Observe(_, _, _) | ObserveAggregate(_, _) | ObserveAgentStatus(_) -> 
                                     failwith "Cannot create new observables while the stream is running." 
                                 | RunStream(_, _) -> failwith "Cannot start a stream when it is already runnning."
                                 | StopStream -> acquisitionCts.Cancel()
                                 | ViewData(_) -> failwith "Cannot request a stream data view before the stream has finished."
                                 | Discard(_) -> failwith "Cannot discard a stream agent while a stream is running."
                            else do! Async.Sleep(100)
                                 do! checkForStopMessage() }

                do! checkForStopMessage()
                return! finishedStream() }

            and finishedStream() = async {
                let! message = mailbox.Receive()
                match message with
                | Observe(_, _, _)
                | ObserveAggregate(_, _) ->
                    failwith "Cannot create new observables once stream is finished. Discard this stream agent and create a new one."
                | ObserveAgentStatus(replyChannel) ->
                    agentStatusSubject.AsObservable() |> replyChannel.Reply
                    return! finishedStream()
                | RunStream(_, _ ) ->
                    failwith "Cannot run a new data stream on a finished stream agent. Discard this stream agent and create a new one."
                | StopStream -> return! finishedStream() // possible that the stop stream message arrives after the stream has auto-stopped
                | ViewData(_) -> failwith "Not yet implemented"
                | Discard(replyChannel) -> 
                    agentStatusSubject.OnNext(Discarded)
                    agentStatusSubject.OnCompleted()
                    replyChannel.Reply() }

            preparing [] []
    
    let streamMailboxProcessor = MailboxProcessor.Start(streamAgentMailbox)

    interface IDisposable with
        member this.Dispose() = 
            streamMailboxProcessor.PostAndReply(Discard)
    
    member this.Observe(channel, downsampling) =
        fun replyChannel -> Observe(channel, downsampling, replyChannel)
        |> streamMailboxProcessor.PostAndReply

    member this.ObserveAggregate(channel) =
        fun replyChannel -> ObserveAggregate(channel)
        |> streamMailboxProcessor.PostAndReply

    member this.ObserveAgentStatus() =
        ObserveAgentStatus
        |> streamMailboxProcessor.PostAndReply

    member this.RunStream(streamingParameters) =
        fun replyChannel -> RunStream(streamingParameters, replyChannel)
        |> streamMailboxProcessor.PostAndReply

    member this.RunStream(streamingInterval, downsamplingRatio, maxPreTriggerSamples, maxPostTriggerSamples, autoStop) =
        let streamingParameters =
            { streamingInterval = streamingInterval
              downsamplingRatio = downsamplingRatio
              maximumPreTriggerSamples = maxPreTriggerSamples
              maximumPostTriggerSamples = maxPostTriggerSamples
              autoStop = autoStop }
        fun replyChannel -> RunStream(streamingParameters, replyChannel)
        |> streamMailboxProcessor.PostAndReply

    member this.StopStream() =
        streamMailboxProcessor.Post(StopStream)

    member this.ViewData() =
        ViewData
        |> streamMailboxProcessor.PostAndReply

    member this.ViewDataAsync() =
        ViewData
        |> streamMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask