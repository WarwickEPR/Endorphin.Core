namespace Endorphin.Instrument.PicoScope5000

open System
open Api
open Errors
open System.Text
open System.Linq
open System.Reactive
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Runtime.InteropServices
open System.Threading

type ChannelSettings = 
    { enabled : bool
      coupling : Coupling
      range : Range
      analogueOffsetInVolts : float }

type SimpleTriggerSettings =
    { channel : Channel
      adcThreshold : int16
      thresholdDirection : ThresholdDirection
      delaySampleCount : uint32
      autoTriggerDelayInMillisec : int16 }
      
type StreamingInterval =
    | Seconds of uint32
    | Milliseconds of uint32
    | Microseconds of uint32
    | Nanoseconds of uint32
    | Picoseconds of uint32
    | Femtoseconds of uint32

type StreamingParmeters =
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

type Triggered =
    | NotTriggered
    | Triggered of uint32

type BufferDataReady =
    { startIndex : uint32
      numberOfSamples : int 
      overflows : Channel seq
      triggered : Triggered }

type StreamCommand =
    | Observe of Channel * Downsampling * AsyncReplyChannel<IObservable<int16>>
    | ObserveAggregate of Channel * AsyncReplyChannel<IObservable<int16> * IObservable<int16>>
    | RunStream of StreamingParmeters * AsyncReplyChannel<IObservable<StreamStatus>>
    | StopStream
    | ViewData of AsyncReplyChannel<int16 seq>
    | Discard

type Command =
    | CloseUnit of replyChannel : AsyncReplyChannel<unit>
    | GetUnitDriverVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitUsbVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitVariantInfo of replyChannel : AsyncReplyChannel<string>
    | GetUnitSerial of replyChannel : AsyncReplyChannel<string>
    | GetUnitCalibrationDate of replyChannel : AsyncReplyChannel<string>
    | GetUnitKernelVersion of replyChannel: AsyncReplyChannel<string>
    | GetUnitDigitalHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitAnalogueHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitFirmwareVersion1 of replyChannel : AsyncReplyChannel<string>
    | GetUnitFirmwareVersion2 of replyChannel : AsyncReplyChannel<string>
    | GetUnitInfo of replyChannel: AsyncReplyChannel<(PicoInfo * string) list>
    | GetUnitIsMainsPowered of replyChannel : AsyncReplyChannel<bool>
    | SetUnitIsMainsPowered of mainsPowered : bool
    | FlashLedIndefinitely
    | StopFlashingLed
    | FlashLed of flashCount : Int16
    | Ping of replyChannel : AsyncReplyChannel<unit>
    | GetTimebaseIntervalInNanosec of timebase : uint32 * replyChannel : AsyncReplyChannel<float * int32>
    | GetTimebaseIntervalInNanosecForSegment of timebase : uint32 * memorySegment : uint32 * replyChannel : AsyncReplyChannel<float * int32>
    | GetFastestTimebaseForCurrentResolution of replyChannel : AsyncReplyChannel<uint32>
    | GetFastestTimebaseForResolution of resolution : Resolution * replyChannel : AsyncReplyChannel<uint32>
    | GetFastestStreamingIntervalInNanosecForCurrentResolution of channelCount : int32 *  replyChannel : AsyncReplyChannel<float>
    | GetFastestStreamingIntervalInNanosecForResolution of resolution : Resolution * channelCount : int32 * replyChannel : AsyncReplyChannel<float>
    | GetDeviceResolution of replyChannel : AsyncReplyChannel<Resolution>
    | SetDeviceResolution of resolution : Resolution
    | GetAnalogueOffsetLimitsInVolts of range : Range * coupling : Coupling * replyChannel : AsyncReplyChannel<float * float>
    | GetAvailableChannelRanges of channel : Channel *  replyChannel : AsyncReplyChannel<Range seq>
    | SetChannelSettings of channel : Channel * channelSettings : ChannelSettings
    | SetChannelBandwidth of channel : Channel * bandwidthLimit : BandwidthLimit
    | DisableChannel of channel : Channel
    | GetMaximumNumberOfChannelsForCurrentResolution of replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfChannelsForResolution of resolution : Resolution * replyChannel : AsyncReplyChannel<int32>
    | DiableTrigger
    | SetAutoTrigger of delayInMillisec : int16
    | SetSimpleTrigger of triggerSettings : SimpleTriggerSettings
    | SetTriggerDelay of unprocessedSampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    | GetCurrentMemorySegment of replyChannel : AsyncReplyChannel<uint32>
    | SetCurrentMemorySegment of memorySegment : uint32
    | SetNumberOfMemorySegments of memorySegment : uint32 * replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | GetAdcCountToVoltsConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<int16 -> float>
    | GetVoltsToAdcCountConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<float -> int16>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : Downsampling * memorySegment : uint32 * AsyncReplyChannel<uint32>
    | GetMaximumDownsamplingRatioForCurrentMemorySegment of unprocessedSampleCount : uint32 * downsampling : Downsampling * replyChannel : AsyncReplyChannel<uint32>
    | CreateStreamAgent of AsyncReplyChannel<MailboxProcessor<StreamCommand> * IObservable<StreamAgentStatus>>

type Agent(serial, initialResolution) =
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

    static let fastestTimebaseForResolution =
        function
        | Resolution._8bit -> 0u
        | Resolution._12bit -> 1u
        | Resolution._14bit
        | Resolution._15bit -> 3u
        | Resolution._16bit -> 4u
        | _ -> failwith "Unexpected resolution."

    static let rangeInVolts =
        function
        | Range._10mV -> 0.010
        | Range._20mV -> 0.020
        | Range._50mV -> 0.050
        | Range._100mV -> 0.100
        | Range._200mV -> 0.200
        | Range._500mV -> 0.500
        | Range._1V -> 1.0
        | Range._2V -> 2.0
        | Range._5V -> 5.0
        | Range._10V -> 10.0
        | Range._20V -> 20.0
        | Range._50V -> 50.0
        | _ -> failwith "Unexpected range."

    static let streamingBufferLength streamingInterval (downsamplingRatio : uint32) =
        let minSampleCount = 
            function
            | Seconds(timeInterval) -> 1e2 * (float timeInterval) / (float downsamplingRatio)
            | Milliseconds(timeInterval) -> 1e5 * (float timeInterval) / (float downsamplingRatio)
            | Microseconds(timeInterval) -> 1e8 * (float timeInterval) / (float downsamplingRatio)
            | Nanoseconds(timeInterval) -> 1e11 * (float timeInterval) / (float downsamplingRatio)
            | Picoseconds(timeInterval) -> 1e14 * (float timeInterval) / (float downsamplingRatio)
            | Femtoseconds(timeInterval) -> 1e17 * (float timeInterval) / (float downsamplingRatio)
    
        let rec computeBufferLength length = 
            if (float) length > minSampleCount streamingInterval
            then length
            else computeBufferLength (2 * length)

        computeBufferLength 1

    static let getUnitInfoValue handle info =
        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        Api.GetUnitInfo(handle, result, resultLength, &requiredLength, info) |> checkStatusIsOk
        result.ToString()

    static let getUnitInfo handle =
        seq { for value in (int PicoInfo.DriverVersion) .. (int PicoInfo.FirmwareVersion2) do
                let info = enum<PicoInfo>(value)
                let result = (info, getUnitInfoValue handle info) 
                yield result }
        |> Seq.toList

    static let maximumNumberOfChanelsForResolution =
        function
        | Resolution._8bit | Resolution._12bit | Resolution._14bit -> 4
        | Resolution._15bit -> 2
        | Resolution._16bit -> 1
        | _ -> failwith "Unexpected resolution."

    static let getFastestStreamingIntervalInNanosec = 
        function
        | (Resolution._12bit, 4)
        | (Resolution._12bit, 3)
        | (Resolution._14bit, 4)
        | (Resolution._14bit, 3) -> 256.0
        | (Resolution._8bit, 4) 
        | (Resolution._8bit, 3) 
        | (Resolution._12bit, 2) 
        | (Resolution._14bit, 2) 
        | (Resolution._15bit, 2) -> 128.0
        | (Resolution._8bit, 3)
        | (Resolution._12bit, 1)
        | (Resolution._14bit, 1)
        | (Resolution._15bit, 1)
        | (Resolution._16bit, 2) -> 64.0
        | (Resolution._8bit, 1) -> 32.0
        | (resolution, channelCount) when channelCount > maximumNumberOfChanelsForResolution resolution ->
            failwith "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
        | parameters -> failwith "Unexpected number of channels or resolution: %A." parameters

    let streamAgentMailbox handle (agentStatusSubject : ISubject<StreamAgentStatus>) =
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

        let acquisition observers aggregateObservers streamingParameters (statusSubject : ISubject<StreamStatus>) =    
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
                    |> List.fold(|||) (if List.isEmpty aggregateObservers 
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
                use pollCts = new CancellationTokenSource()
                Async.Start(pollLatestValues buffers, pollCts.Token)
                return fun () -> pollCts.Cancel() }

        fun (mailbox : MailboxProcessor<StreamCommand>) ->
            let rec preparing observers aggregateObservers = async {
                let! message = mailbox.Receive()
                match message with
                | Observe(channel, downsampling, replyChannel) ->
                    if downsampling = Downsampling.Aggregate
                    then failwith "Attempted to create aggregate observable using Observe command. Use ObserveAggregate instead."
                
                    let subject = new Subject<int16>()
                    replyChannel.Reply(subject.AsObservable())
                    return! preparing ((subject.AsObserver(), channel, downsampling) :: observers) aggregateObservers

                | ObserveAggregate(channel, replyChannel) ->
                    let maxSubject = new Subject<int16>()
                    let minSubject = new Subject<int16>()
                    replyChannel.Reply(maxSubject.AsObservable(), minSubject.AsObservable())
                    return! preparing observers (((maxSubject.AsObserver(), minSubject.AsObserver()), channel) :: aggregateObservers)
            
                | RunStream(streamingParameters, replyChannel) ->
                    let statusSubject = new BehaviorSubject<StreamStatus>(Preparing)
                    statusSubject.AsObservable() |> replyChannel.Reply
                    return! running observers aggregateObservers streamingParameters statusSubject
            
                | StopStream -> failwith "Cannot stop a stream before it has been started."
                | ViewData(_) -> failwith "Cannot request a stream data view before the stream has finished."
                | Discard -> 
                    agentStatusSubject.OnNext(Discarded)
                    agentStatusSubject.OnCompleted() }

            and running observers aggregateObservers streamingParameters statusSubject = async {
                let! doStop = acquisition observers aggregateObservers streamingParameters statusSubject
                use mailboxCts = new CancellationTokenSource()

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
                                 | Observe(_, _, _) | ObserveAggregate(_, _) -> failwith "Cannot create new observables while the stream is running." 
                                 | RunStream(_, _) -> failwith "Cannot start a stream when it is already runnning."
                                 | StopStream -> doStop()
                                 | ViewData(_) -> failwith "Cannot request a stream data view before the stream has finished."
                                 | Discard -> failwith "Cannot discard a stream agent while a stream is running."
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
                | RunStream(_, _ ) ->
                    failwith "Cannot run a new data stream on a finished stream agent. Discard this stream agent and create a new one."
                | StopStream -> return! finishedStream() // possible that the stop stream message arrives after the stream has auto-stopped
                | ViewData(_) -> failwith "Not yet implemented"
                | Discard -> 
                    agentStatusSubject.OnNext(Discarded)
                    agentStatusSubject.OnCompleted() }

            preparing [] []

    let picoScopeAgentMailbox handle (mailbox : MailboxProcessor<Command>) =
        let rec loop currentSegment currentResolution = async {
            let! message = mailbox.Receive() 
            match message with

            | CloseUnit(replyChannel) ->
                Api.CloseUnit(handle) |> checkStatusIsOk
                replyChannel.Reply()

            // Requests
            
            | GetUnitDriverVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.DriverVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitUsbVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.UsbVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitHardwareVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.HardwareVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitVariantInfo(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.VariantInfo |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitSerial(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.BatchAndSerial |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitCalibrationDate(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.CalibrationDate |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitKernelVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.KernelVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitDigitalHardwareVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.DigitalHardwareVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitAnalogueHardwareVersion(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.AnalogueHardwareVersion |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitFirmwareVersion1(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.FirmwareVersion1 |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitFirmwareVersion2(replyChannel) -> 
                getUnitInfoValue handle PicoInfo.FirmwareVersion2 |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitInfo(replyChannel) -> 
                let infos = getUnitInfo(handle)
                infos |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetUnitIsMainsPowered(replyChannel) -> 
                let status = Api.CurrentPowerSource(handle)
                match status with
                | PicoStatus.PowerSupplyConnected -> true |> replyChannel.Reply
                | PicoStatus.PowerSupplyNotConnected -> false |> replyChannel.Reply
                | _ -> raise (PicoException(messageForStatus(status), status))
                return! loop currentSegment currentResolution

            | Ping(replyChannel) -> 
                Api.PingUnit(handle) |> checkStatusIsOk
                replyChannel.Reply()
                return! loop currentSegment currentResolution

            | GetTimebaseIntervalInNanosec(timebase, replyChannel) -> 
                let mutable interval = float32 0.0
                let mutable maxSamples = 0
                Api.GetTimebase(handle, timebase, 0, &interval, &maxSamples, currentSegment) |> checkStatusIsOk
                (float interval, maxSamples) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetTimebaseIntervalInNanosecForSegment(timebase, segment, replyChannel) ->
                let mutable interval = float32 0.0
                let mutable maxSamples = 0
                Api.GetTimebase(handle, timebase, 0, &interval, &maxSamples, segment) |> checkStatusIsOk
                (float interval, maxSamples) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetFastestTimebaseForCurrentResolution(replyChannel) -> 
                fastestTimebaseForResolution currentResolution |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetFastestTimebaseForResolution(resolution, replyChannel) -> 
                fastestTimebaseForResolution resolution |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetFastestStreamingIntervalInNanosecForCurrentResolution(channelCount, replyChannel) -> 
                (getFastestStreamingIntervalInNanosec(currentResolution, channelCount)) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetFastestStreamingIntervalInNanosecForResolution(resolution, channelCount, replyChannel) ->
                (getFastestStreamingIntervalInNanosec(resolution, channelCount)) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetDeviceResolution(replyChannel) -> 
                currentResolution |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetAnalogueOffsetLimitsInVolts(range, coupling, replyChannel) ->
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                Api.GetAnalogueOffset(handle, range, coupling, &maxOffset, &minOffset) |> checkStatusIsOk
                (float maxOffset, float minOffset) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetAvailableChannelRanges(channel, replyChannel) ->
                let mutable rangesLength = 12
                let ranges = Array.zeroCreate(rangesLength)
                Api.GetChannelInformation(handle, ChannelInfo.VoltageOffsetRanges, 0, ranges, &rangesLength, channel) |> checkStatusIsOk
                Array.toSeq(ranges).Take(rangesLength) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetMaximumNumberOfChannelsForCurrentResolution(replyChannel) ->
                let channelCount = int ((getUnitInfoValue handle PicoInfo.VariantInfo).Chars(1))
                let maxChannels = maximumNumberOfChanelsForResolution currentResolution
                min channelCount maxChannels |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetMaximumNumberOfChannelsForResolution(resolution, replyChannel) ->
                let channelCount = int ((getUnitInfoValue handle PicoInfo.VariantInfo).Chars(1))
                let maxChannels = maximumNumberOfChanelsForResolution resolution
                min channelCount maxChannels |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | IsTriggerEnabled(replyChannel) ->
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                Api.IsTriggerOrPulseWidthQualifierEnabled(handle, &triggerEnabled, &pwqEnabled) |> checkStatusIsOk
                (triggerEnabled <> 0s) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetCurrentMemorySegment(replyChannel) ->
                currentSegment |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetMaximumNumberOfSegments(replyChannel) ->
                let mutable maxSegments = 0u
                Api.GetMaximumNumberOfSegments(handle, &maxSegments) |> checkStatusIsOk 
                maxSegments |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetAdcCountToVoltsConversion(range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s 
                Api.MaximumValue(handle, &maxAdcCounts) |> checkStatusIsOk
                let maxAdcCountsValue = maxAdcCounts       
                let range = rangeInVolts(range)
                (fun adcCounts -> (range * float(adcCounts) / float(maxAdcCountsValue)) + analogueOffset) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetVoltsToAdcCountConversion(range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s
                Api.MaximumValue(handle, &maxAdcCounts) |> checkStatusIsOk    
                let maxAdcCountsValue = maxAdcCounts            
                let range = rangeInVolts(range)
                (fun voltage -> int16 (((voltage - analogueOffset) / range) * float(maxAdcCountsValue))) |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetMaximumDownsamplingRatio(sampleCount, downsampling, memorySegment, replyChannel) ->
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(handle, sampleCount, &maxDownsamplingRatio, downsampling, memorySegment) |> checkStatusIsOk
                maxDownsamplingRatio |> replyChannel.Reply
                return! loop currentSegment currentResolution

            | GetMaximumDownsamplingRatioForCurrentMemorySegment(sampleCount, downsampling, replyChannel) ->
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(handle, sampleCount, &maxDownsamplingRatio, downsampling, currentSegment) |> checkStatusIsOk
                maxDownsamplingRatio |> replyChannel.Reply
                return! loop currentSegment currentResolution

            // Instructions

            | SetUnitIsMainsPowered(mainsPowered) ->
                let powerStatus = if mainsPowered then PicoStatus.PowerSupplyConnected else PicoStatus.PowerSupplyNotConnected
                Api.ChangePowerSource(handle, powerStatus) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | FlashLedIndefinitely ->
                Api.FlashLed(handle, -1s) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | StopFlashingLed ->
                Api.FlashLed(handle, 0s) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | FlashLed(counts) ->
                if counts <= 0s then failwith "The device LED can only be flashed a positive, non-zero number of times."
                Api.FlashLed(handle, counts) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | SetDeviceResolution(resolution) ->
                Api.SetDeviceResolution(handle, resolution) |> checkStatusIsOk
                return! loop currentSegment resolution

            | SetChannelSettings(channel, channelSettings) ->
                let enabled = if channelSettings.enabled then 1s else 0s
                Api.SetChannel(handle, channel, enabled, channelSettings.coupling, channelSettings.range, 
                    float32 channelSettings.analogueOffsetInVolts) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | SetChannelBandwidth(channel, bandwidthLimit) ->
                Api.SetBandwidthFilter(handle, channel, bandwidthLimit) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | DisableChannel(channel) ->
                Api.SetChannel(handle, channel, 0s, Coupling.DC, Range._5V, 0.0f) |> checkStatusIsOk
                return! loop currentSegment currentResolution
            
            | DiableTrigger ->
                Api.SetSimpleTrigger(handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, 0s) |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | SetAutoTrigger(delayInMillisec) ->
                Api.SetSimpleTrigger(handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, int16 delayInMillisec)
                |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | SetSimpleTrigger(triggerSettings) ->
                Api.SetSimpleTrigger(handle, 1s, triggerSettings.channel, triggerSettings.adcThreshold, 
                    triggerSettings.thresholdDirection,  triggerSettings.delaySampleCount,
                    int16 triggerSettings.autoTriggerDelayInMillisec)
                |> checkStatusIsOk
                return! loop currentSegment currentResolution

            | SetTriggerDelay(sampleCount) ->
                Api.SetTriggerDelay(handle, sampleCount) |> checkStatusIsOk
                return! loop currentSegment currentResolution
                
            | SetNumberOfMemorySegments(memorySegmentCount, replyChannel) ->
                let mutable samplesPerSegement = 0
                Api.MemorySegments(handle, memorySegmentCount, &samplesPerSegement) |> checkStatusIsOk
                samplesPerSegement |> replyChannel.Reply
                return! loop 0u currentResolution

            | SetCurrentMemorySegment(memorySegment) ->
                return! loop memorySegment currentResolution 
            
            // Create acquisition agents

            | CreateStreamAgent(replyChannel) ->
                let streamAgentStatusSubject = new BehaviorSubject<_>(Active)
                (MailboxProcessor.Start(streamAgentMailbox handle streamAgentStatusSubject), streamAgentStatusSubject.AsObservable()) |> replyChannel.Reply
                
                use mailboxCts = new CancellationTokenSource()
                use _ = streamAgentStatusSubject.AsObservable()
                        |> Observable.filter (fun status -> status = Discarded)
                        |> Observable.subscribe (fun _ -> mailboxCts.Cancel())
                
                let rec checkForMessages() = async {
                    if (not mailboxCts.Token.IsCancellationRequested) && mailbox.CurrentQueueLength <> 0
                    then failwith "PicoScope agent received message while a stream agent is active. Discard this agent first."
                    
                    if (not mailboxCts.Token.IsCancellationRequested)
                    then do! Async.Sleep(100)
                         do! checkForMessages() }

                do! checkForMessages()
                return! loop currentSegment currentResolution }
            
        loop 0u initialResolution

    let picoMailboxProcessor =
        let mutable handle = 0s
        let status = OpenUnit(&handle, serial, initialResolution)
        match status with
        | PicoStatus.Ok | PicoStatus.PowerSupplyNotConnected ->
            MailboxProcessor.Start(picoScopeAgentMailbox handle)
        | _ -> raise (PicoException(messageForStatus(status), status))

    new() = Agent(null, Resolution._8bit)
    new(initialResolution) = Agent(null, initialResolution)
    new(serial) = Agent(serial, Resolution._8bit)

    member this.CloseUnit() =
        CloseUnit
        |> picoMailboxProcessor.PostAndReply
    
    member this.CloseUnitAsync() =
        CloseUnit
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitDriverVersion() =
        GetUnitDriverVersion
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitDriverVersionAsync() =
        GetUnitDriverVersion
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitUsbVersion() =
        GetUnitUsbVersion
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitUsbVersionAsync() =
        GetUnitUsbVersion
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitHardwareVersion() =
        GetUnitHardwareVersion
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitHardwareVersionAsync() =
        GetUnitHardwareVersion
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitVariantInfo() =
        GetUnitVariantInfo
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitVariantInfoAsync() =
        GetUnitVariantInfo
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitSerial() =
        GetUnitSerial
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitSerialAsync() =
        GetUnitSerial
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitCalibrationDate() =
        GetUnitCalibrationDate
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitCalibrationDateAsync() =
        GetUnitCalibrationDate
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitKernelVersion() =
        GetUnitKernelVersion
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitKernelVersionAsync() =
        GetUnitKernelVersion
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitDigitalHardwareVersion() =
        GetUnitDigitalHardwareVersion
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitDigitalHardwareVersionAsync() =
        GetUnitDigitalHardwareVersion
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitFirmwareVersion1() =
        GetUnitFirmwareVersion1
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitFirmwareVersion1Async() =
        GetUnitFirmwareVersion1
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitFirmwareVersion2() =
        GetUnitFirmwareVersion2
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitFirmwareVersion2Async() =
        GetUnitFirmwareVersion2
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetUnitInfo() =
        picoMailboxProcessor.PostAndReply(GetUnitInfo)
        |> List.toSeq

    member this.GetUnitInfoAsync() =
        async {
            let! info = GetUnitInfo |> picoMailboxProcessor.PostAndAsyncReply
            return info |> List.toSeq }
        |> Async.StartAsTask

    member this.GetUnitIsMainsPowered() =
        GetUnitIsMainsPowered
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitIsMainsPoweredAsync() =
        GetUnitIsMainsPowered
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.SetUnitIsMainsPowered(mainsPowered) =
        SetUnitIsMainsPowered(mainsPowered)
        |> picoMailboxProcessor.Post

    member this.FlashLedIndefinitely() =
        FlashLedIndefinitely
        |> picoMailboxProcessor.Post

    member this.FlashLed(flashCount) =
        FlashLed(flashCount)
        |> picoMailboxProcessor.Post

    member this.StopFlashingLed() =
        StopFlashingLed
        |> picoMailboxProcessor.Post

    member this.Ping() =
        Ping
        |> picoMailboxProcessor.PostAndReply

    member this.PingAsync() =
        Ping
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetTimebaseIntervalInNanosec(timebase) =
        fun replyChannel -> GetTimebaseIntervalInNanosec(timebase, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetTimebaseIntervalInNanosecAsync(timebase) =
        fun replyChannel -> GetTimebaseIntervalInNanosec(timebase, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetTimebaseIntervalInNanosecForSegment(timebase, memorySegment) =
        fun replyChannel -> GetTimebaseIntervalInNanosecForSegment(timebase, memorySegment, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetTimebaseIntervalInNanosecForSegmentAsync(timebase, memorySegment) =
        fun replyChannel -> GetTimebaseIntervalInNanosecForSegment(timebase, memorySegment, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetTimebaseForCurrentResolution() =
        picoMailboxProcessor.PostAndReply(GetFastestTimebaseForCurrentResolution)

    member this.GetTimebaseIntervalInNanosecAsync() =
        picoMailboxProcessor.PostAndAsyncReply(GetFastestTimebaseForCurrentResolution)
        |> Async.StartAsTask

    member this.GetTimebaseForResolution(resolution) =
        fun replyChannel -> GetFastestTimebaseForResolution(resolution, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetTimebaseIntervalForResolutionAsync(resolution) =
        fun replyChannel -> GetFastestTimebaseForResolution(resolution, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask
    
    member this.GetFastestStreamingIntervalInNanosecForCurrentResolution(channelCount) =
        fun replyChannel -> GetFastestStreamingIntervalInNanosecForCurrentResolution(channelCount, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetFastestStreamingIntervalInNanosecForCurrentResolutionAsync(channelCount) =
        fun replyChannel -> GetFastestStreamingIntervalInNanosecForCurrentResolution(channelCount, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetFastestStreamingIntervalInNanosecForResolution(resolution, channelCount) =
        fun replyChannel -> GetFastestStreamingIntervalInNanosecForResolution(resolution, channelCount, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetFastestStreamingIntervalInNanosecForResolutionAsync(resolution, channelCount) =
        fun replyChannel -> GetFastestStreamingIntervalInNanosecForResolution(resolution, channelCount, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetDeviceResolution() =
        GetDeviceResolution
        |> picoMailboxProcessor.PostAndReply

    member this.GetDeviceResolutionAsync() =
        GetDeviceResolution
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.SetDeviceResolution(resolution) =
        SetDeviceResolution(resolution)
        |> picoMailboxProcessor.Post

    member this.GetAnalogueOffsetLimitsInVolts(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimitsInVolts(range, coupling, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetAnalogueOffsetLimitsInVoltsAsync(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimitsInVolts(range, coupling, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetAvailableChannelRanges(channel) =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetAvailableChannelRangesAsync(channel) =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.SetChannelSettings(channel, channelSettings) =
        SetChannelSettings(channel, channelSettings)
        |> picoMailboxProcessor.Post

    member this.SetChannelSettings(channel, enabled, range, coupling, analogueOffsetInVolts) =
        let channelSettings =
            { enabled = enabled
              range = range
              coupling = coupling 
              analogueOffsetInVolts = analogueOffsetInVolts }
        SetChannelSettings(channel, channelSettings)
        |> picoMailboxProcessor.Post

    member this.SetChannelBandwidth(channel, bandwidthLimit) =
        SetChannelBandwidth(channel, bandwidthLimit)
        |> picoMailboxProcessor.Post

    member this.DisableChannel(channel) =
        DisableChannel(channel)
        |> picoMailboxProcessor.Post

    (*
    | GetMaximumNumberOfChannelsForCurrentResolution of replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfChannelsForResolution of resolution : Resolution * replyChannel : AsyncReplyChannel<int32>
    | DiableTrigger
    | SetAutoTrigger of delayInMillisec : int16
    | SetSimpleTrigger of triggerSettings : SimpleTriggerSettings
    | SetTriggerDelay of unprocessedSampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    | GetCurrentMemorySegment of replyChannel : AsyncReplyChannel<uint32>
    | SetCurrentMemorySegment of memorySegment : uint32
    | SetNumberOfMemorySegments of memorySegment : uint32 * replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | GetAdcCountToVoltsConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<int16 -> float>
    | GetVoltsToAdcCountConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<float -> int16>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : Downsampling * memorySegment : uint32 * AsyncReplyChannel<uint32>
    | GetMaximumDownsamplingRatioForCurrentMemorySegment of unprocessedSampleCount : uint32 * downsampling : Downsampling * replyChannel : AsyncReplyChannel<uint32>
    | CreateStreamAgent of AsyncReplyChannel<MailboxProcessor<StreamCommand> * IObservable<StreamAgentStatus>> *)


    static member GetConnectedDeviceSerials() =
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = EnumerateUnits(&count, serials, &stringLength) 
        match status with
        | PicoStatus.Ok -> serials.ToString().Split(",".ToCharArray())
        | PicoStatus.NotFound -> Array.empty
        | status -> raise (PicoException(messageForStatus status, status))    
    