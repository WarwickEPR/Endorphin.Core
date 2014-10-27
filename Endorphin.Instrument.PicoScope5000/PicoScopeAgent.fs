namespace Endorphin.Instrument.PicoScope5000

open System
open Errors
open System.Text
open System.Linq
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

type internal Command =
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
    | IsUnitMainsPowered of replyChannel : AsyncReplyChannel<bool>
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
    | DisableTrigger
    | SetAutoTrigger of delayInMillisec : int16
    | SetSimpleTrigger of triggerSettings : SimpleTriggerSettings
    | SetTriggerDelay of sampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    | GetCurrentMemorySegment of replyChannel : AsyncReplyChannel<uint32>
    | SetCurrentMemorySegment of memorySegment : uint32
    | SetNumberOfMemorySegments of memorySegments : uint32 * replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | GetAdcCountToVoltsConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<int16 -> float>
    | GetVoltsToAdcCountConversion of range : Range * analogueOffsetInVolts : float * replyChannel : AsyncReplyChannel<float -> int16>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : Downsampling * memorySegment : uint32 * AsyncReplyChannel<uint32>
    | GetMaximumDownsamplingRatioForCurrentMemorySegment of unprocessedSampleCount : uint32 * downsampling : Downsampling * replyChannel : AsyncReplyChannel<uint32>
    | CreateStreamAgent of AsyncReplyChannel<StreamAgent>

type PicoScope5000Agent(serial, initialResolution) =
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

            | IsUnitMainsPowered(replyChannel) -> 
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
            
            | DisableTrigger ->
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
                let streamAgent = new StreamAgent(handle)
                streamAgent |> replyChannel.Reply

                use mailboxCts = new CancellationTokenSource()
                use _ = streamAgent.ObserveAgentStatus()
                        |> Observable.filter (fun status -> status = Discarded)
                        |> Observable.subscribe (fun _ -> mailboxCts.Cancel())
                
                let rec checkForMessages() = async {
                    if (not mailboxCts.Token.IsCancellationRequested) && mailbox.CurrentQueueLength <> 0
                    then failwith "PicoScope agent received message while a stream agent is active. Discard this agent first."
                    
                    if (not mailboxCts.Token.IsCancellationRequested)
                    then do! Async.Sleep(100)
                         do! checkForMessages() }

                do! checkForMessages()
                mailboxCts.Dispose()
                return! loop currentSegment currentResolution }
            
        loop 0u initialResolution

    let picoMailboxProcessor =
        let mutable handle = 0s
        let status = Api.OpenUnit(&handle, serial, initialResolution)
        match status with
        | PicoStatus.Ok | PicoStatus.PowerSupplyNotConnected ->
            MailboxProcessor.Start(picoScopeAgentMailbox handle)
        | _ -> raise (PicoException(messageForStatus(status), status))

    new() = new PicoScope5000Agent(null, Resolution._8bit)
    new(initialResolution) = new PicoScope5000Agent(null, initialResolution)
    new(serial) = new PicoScope5000Agent(serial, Resolution._8bit)

    interface IDisposable with
        member this.Dispose() =
            CloseUnit
            |> picoMailboxProcessor.PostAndReply

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
        IsUnitMainsPowered
        |> picoMailboxProcessor.PostAndReply

    member this.GetUnitIsMainsPoweredAsync() =
        IsUnitMainsPowered
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

    member this.GetMaximumNumberOfChannelsForCurrentResolution() =
        GetMaximumNumberOfChannelsForCurrentResolution
        |> picoMailboxProcessor.PostAndReply

    member this.GetMaximumNumberOfChannelsForCurrentResolutionAsync() =
        GetMaximumNumberOfChannelsForCurrentResolution
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetMaximumNumberOfChannelsForResolution(resolution) =
        fun replyChannel -> GetMaximumNumberOfChannelsForResolution(resolution, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetMaximumNumberOfChannelsForResolutionAsync(resolution) =
        fun replyChannel -> GetMaximumNumberOfChannelsForResolution(resolution, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.DisableTrigger() = 
        DisableTrigger
        |> picoMailboxProcessor.Post

    member this.SetAutoTrigger(delayInMillisec) =
        SetAutoTrigger(delayInMillisec)
        |> picoMailboxProcessor.Post

    member this.SetSimpleTrigger(triggerSettings) =
        SetSimpleTrigger(triggerSettings)
        |> picoMailboxProcessor.Post

    member this.SetSimpleTrigger(channel, adcThreshold, thresholdDirection, delaySampleCount, autoTriggerDelayInMillisec) =
        let triggerSettings =
            { channel = channel
              adcThreshold = adcThreshold
              thresholdDirection = thresholdDirection
              delaySampleCount = delaySampleCount
              autoTriggerDelayInMillisec = autoTriggerDelayInMillisec }
        SetSimpleTrigger(triggerSettings)
        |> picoMailboxProcessor.Post

    member this.SetTriggerDelay(sampleCount) =
        SetTriggerDelay(sampleCount)
        |> picoMailboxProcessor.Post

    member this.IsTriggerEnabled() =
        IsTriggerEnabled
        |> picoMailboxProcessor.PostAndReply

    member this.IsTriggerEnabledAsync() =
        IsTriggerEnabled
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetCurrentMemorySegment() =
        GetCurrentMemorySegment
        |> picoMailboxProcessor.PostAndReply

    member this.GetCurrentMemorySegmentAsync() =
        GetCurrentMemorySegment
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.SetCurrentMemorySegment(memorySegment) =
        SetCurrentMemorySegment(memorySegment)
        |> picoMailboxProcessor.Post

    member this.SetNumberOfMemorySegments(memorySegments) =
        SetNumberOfMemorySegments(memorySegments)
        |> picoMailboxProcessor.Post

    member this.GetMaximumNumberOfSegments() =
        GetMaximumNumberOfSegments
        |> picoMailboxProcessor.PostAndReply

    member this.GetMaximumNumberOfSegmentsAsync() =
        GetMaximumNumberOfSegments
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetAdcCountToVoltsConversion(range, analogueOffsetInVolts) =
        fun replyChannel -> GetAdcCountToVoltsConversion(range, analogueOffsetInVolts, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetAdcCountToVoltsConversionAsync(range, analogueOffsetInVolts) =
        fun replyChannel -> GetAdcCountToVoltsConversion(range, analogueOffsetInVolts, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetVoltsToAdcCountConversion(range, analogueOffsetInVolts) =
        fun replyChannel -> GetVoltsToAdcCountConversion(range, analogueOffsetInVolts, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetVoltsToAdcCountConversionAsync(range, analogueOffsetInVolts) =
        fun replyChannel -> GetVoltsToAdcCountConversion(range, analogueOffsetInVolts, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment) =
        fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetMaximumDownsamplingRatioAsync(unprocessedSampleCount, downsampling, memorySegment) =
        fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetMaximumDownsamplingRatioForCurrentMemorySegment(unprocessedSampleCount, downsampling) =
        fun replyChannel -> GetMaximumDownsamplingRatioForCurrentMemorySegment(unprocessedSampleCount, downsampling, replyChannel)
        |> picoMailboxProcessor.PostAndReply

    member this.GetMaximumDownsamplingRatioForCurrentMemorySegmentAsync(unprocessedSampleCount, downsampling) =
        fun replyChannel -> GetMaximumDownsamplingRatioForCurrentMemorySegment(unprocessedSampleCount, downsampling, replyChannel)
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.CreateStreamAgent() =
        CreateStreamAgent
        |> picoMailboxProcessor.PostAndReply

    member this.CreateStreamAgentAsync() =
        CreateStreamAgent
        |> picoMailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    static member GetConnectedDeviceSerials() =
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = Api.EnumerateUnits(&count, serials, &stringLength) 
        match status with
        | PicoStatus.Ok -> serials.ToString().Split(",".ToCharArray())
        | PicoStatus.NotFound -> Array.empty
        | status -> raise (PicoException(messageForStatus status, status))    
    