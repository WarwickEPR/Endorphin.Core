namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Utils
open Errors
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Text
open System.Linq

type internal Command =
    | CloseUnit of replyChannel : AsyncReplyChannel<unit>
    | GetUnitDriverVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitUsbVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitModelNumber of replyChannel : AsyncReplyChannel<string>
    | GetUnitSerial of replyChannel : AsyncReplyChannel<string>
    | GetUnitCalibrationDate of replyChannel : AsyncReplyChannel<string>
    | GetUnitKernelVersion of replyChannel: AsyncReplyChannel<string>
    | GetUnitDigitalHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitAnalogueHardwareVersion of replyChannel : AsyncReplyChannel<string>
    | GetUnitFirmwareVersion1 of replyChannel : AsyncReplyChannel<string>
    | GetUnitFirmwareVersion2 of replyChannel : AsyncReplyChannel<string>
    | GetAllUnitInfo of replyChannel: AsyncReplyChannel<(PicoInfo * string) list>
    | IsUnitMainsPowered of replyChannel : AsyncReplyChannel<bool>
    | SetMainsPower of useMainsPower : bool
    | FlashLedIndefinitely
    | StopFlashingLed
    | FlashLed of flashCount : Int16
    | Ping of replyChannel : AsyncReplyChannel<unit>
    | GetTimebaseInterval of timebase : uint32 * memorySegment : uint32 * replyChannel : AsyncReplyChannel<float<s> * int32>
    | GetDeviceResolution of replyChannel : AsyncReplyChannel<Resolution>
    | SetDeviceResolution of resolution : Resolution
    | GetAnalogueOffsetLimits of range : Range * coupling : Coupling * replyChannel : AsyncReplyChannel<float<V> * float<V>>
    | GetAvailableChannels of replyChannel : AsyncReplyChannel<Set<Channel>>
    | GetAvailableChannelRanges of channel : Channel *  replyChannel : AsyncReplyChannel<Range seq>
    | SetChannelSettings of channel : Channel * channelSettings : ChannelSettings
    | DisableChannel of channel : Channel
    | DisableTrigger
    | SetAutoTrigger of delay : float<s>
    | SetSimpleTrigger of triggerSettings : SimpleTriggerSettings
    | SetTriggerDelay of sampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    | SetNumberOfMemorySegments of memorySegments : uint32 * replyChannel : AsyncReplyChannel<int32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | GetAdcCountToVoltageConversion of range : Range * analogueOffset : float<V> * replyChannel : AsyncReplyChannel<int16 -> float<V>>
    | GetVoltageToAdcCountConversion of range : Range * analogueOffset : float<V> * replyChannel : AsyncReplyChannel<float<V> -> int16>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : Downsampling * memorySegment : uint32 * replyChannel : AsyncReplyChannel<uint32>
    | SetDataBuffer of channel : Channel * buffer : int16 array * segmentIndex : uint32 * downsampling : Downsampling
    | SetAggregateDataBuffers of channel : Channel * bufferMax : int16 array * bifferMin : int16 array * segmentIndex : uint32
    | RunStreaming of sampleInterval : float<s> * streamStop : StreamStop * downsamplingRatio : uint32 * downsampling : Downsampling * bufferLength : uint32 * replyChannel : AsyncReplyChannel<float<s>>
    | GetStreamingLatestValues of callback : (StreamingValuesReady -> unit)
    | StopAcquisition of AsyncReplyChannel<unit>

type PicoScope5000(serial, initialResolution) =
    let picoScopeError = new Event<PicoException>()

    let (handle, wasMainsPoweredInitially) =
        let mutable localHandle = 0s
        let status = Api.OpenUnit(&localHandle, serial, initialResolution)
        match status with
        | PicoStatus.Ok -> (localHandle, true)
        | PicoStatus.PowerSupplyNotConnected -> (localHandle, false)
        | _ -> raise (PicoException(messageForStatus(status), status, "Open unit"))

    let inputChannels =
        let resultLength = 32s
        let modelNumber = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        Api.GetUnitInfo(handle, modelNumber, resultLength, &requiredLength, PicoInfo.ModelNumber) |> checkStatusIsOk "Get model number"
        let numberOfInputs = int (modelNumber.[1].ToString()) // number of input channels is the second number in the model number
        match numberOfInputs with
        | 2 -> Set.ofList [ Channel.A ; Channel.B ]
        | 4 -> Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ]
        | _ -> failwith "Unexpected PicoScope model number."

    let agent = Agent.Start(fun mailbox ->
        let rec loop currentResolution isMainsPowered = async {
            let checkStatusAndFailedDueToMainsPower message status =
                try
                    checkStatusIsOk message status
                    false
                with
                | PicoException(_, status, _) as exn when status = PicoStatus.PowerSupplyNotConnected ->
                    (exn :?> PicoException) |> picoScopeError.Trigger
                    true
                | PicoException(_, _, _) as exn ->
                    (exn :?> PicoException) |> picoScopeError.Trigger
                    false

            let checkStatusAndTriggerErrorEvent message status =
                try
                    checkStatusIsOk message status
                with
                | PicoException(_, _, _) as exn ->
                    (exn :?> PicoException) |> picoScopeError.Trigger
            
            let getUnitInfo info command =
                let resultLength = 32s
                let result = new StringBuilder(int resultLength)
                let mutable requiredLength = 0s
                Api.GetUnitInfo(handle, result, resultLength, &requiredLength, info) |> checkStatusAndTriggerErrorEvent command
                result.ToString()

            let getAllUnitInfos message =
                seq { 
                    for value in (int PicoInfo.DriverVersion) .. (int PicoInfo.FirmwareVersion2) do
                        let info = enum<PicoInfo>(value)
                        yield (info, getUnitInfo info message) }
                |> Seq.toList


            let! message = mailbox.Receive() 
            match message with

            | CloseUnit(replyChannel) ->
                Api.CloseUnit(handle) |> checkStatusAndTriggerErrorEvent message
                replyChannel.Reply()

            // Requests
            
            | GetUnitDriverVersion(replyChannel) -> 
                getUnitInfo PicoInfo.DriverVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitUsbVersion(replyChannel) -> 
                getUnitInfo PicoInfo.UsbVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitHardwareVersion(replyChannel) -> 
                getUnitInfo PicoInfo.HardwareVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitModelNumber(replyChannel) -> 
                getUnitInfo PicoInfo.ModelNumber message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitSerial(replyChannel) -> 
                getUnitInfo PicoInfo.SerialNumber message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitCalibrationDate(replyChannel) -> 
                getUnitInfo PicoInfo.CalibrationDate message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitKernelVersion(replyChannel) -> 
                getUnitInfo PicoInfo.KernelVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitDigitalHardwareVersion(replyChannel) -> 
                getUnitInfo PicoInfo.DigitalHardwareVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitAnalogueHardwareVersion(replyChannel) -> 
                getUnitInfo PicoInfo.AnalogueHardwareVersion message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitFirmwareVersion1(replyChannel) -> 
                getUnitInfo PicoInfo.FirmwareVersion1 message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetUnitFirmwareVersion2(replyChannel) -> 
                getUnitInfo PicoInfo.FirmwareVersion2 message |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetAllUnitInfo(replyChannel) -> 
                let infos = getAllUnitInfos message
                infos |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | IsUnitMainsPowered(replyChannel) -> 
                let status = Api.CurrentPowerSource(handle)
                let mainsPowered = 
                    match status with
                    | PicoStatus.PowerSupplyConnected -> true
                    | _ -> status |> checkStatusAndFailedDueToMainsPower message |> not
                mainsPowered |> replyChannel.Reply
                return! loop currentResolution mainsPowered

            | Ping(replyChannel) -> 
                let failedDueToMainsPower = 
                    Api.PingUnit(handle) 
                    |> checkStatusAndFailedDueToMainsPower message
                replyChannel.Reply()
                return! loop currentResolution (isMainsPowered && not failedDueToMainsPower)

            | GetTimebaseInterval(timebase, segment, replyChannel) ->
                let mutable interval = float32 0.0
                let mutable maxSamples = 0
                Api.GetTimebase(handle, timebase, 0, &interval, &maxSamples, segment) |> checkStatusAndTriggerErrorEvent message
                ((float interval) * 1e-9<s>, maxSamples) |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetDeviceResolution(replyChannel) -> 
                currentResolution |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetAnalogueOffsetLimits(range, coupling, replyChannel) ->
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                Api.GetAnalogueOffset(handle, range, coupling, &maxOffset, &minOffset) |> checkStatusAndTriggerErrorEvent message
                (float maxOffset * 1.0<V>, float minOffset * 1.0<V>) |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetAvailableChannels(replyChannel) ->
                match isMainsPowered with
                | true -> inputChannels
                | false -> 
                    [ Channel.A ; Channel.B ]
                    |> Set.ofList
                    |> Set.intersect inputChannels 
                |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetAvailableChannelRanges(channel, replyChannel) ->
                if not (inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                let mutable rangesLength = 12
                let ranges = Array.zeroCreate(rangesLength)
                Api.GetChannelInformation(handle, ChannelInfo.VoltageOffsetRanges, 0, ranges, &rangesLength, channel) |> checkStatusAndTriggerErrorEvent message
                Array.toSeq(ranges).Take(rangesLength) |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | IsTriggerEnabled(replyChannel) ->
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                Api.IsTriggerOrPulseWidthQualifierEnabled(handle, &triggerEnabled, &pwqEnabled) |> checkStatusAndTriggerErrorEvent message
                (triggerEnabled <> 0s) |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetMaximumNumberOfSegments(replyChannel) ->
                let mutable maxSegments = 0u
                Api.GetMaximumNumberOfSegments(handle, &maxSegments) |> checkStatusAndTriggerErrorEvent message
                maxSegments |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetAdcCountToVoltageConversion(range : Range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s 
                Api.MaximumValue(handle, &maxAdcCounts) |> checkStatusAndTriggerErrorEvent message
                let maxAdcCountsValue = maxAdcCounts       
                let voltageRange = range.ToVolts()
                
                (fun adcCounts -> (voltageRange * float(adcCounts &&& currentResolution.BitMask()) / float(maxAdcCountsValue)) + analogueOffset) 
                |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetVoltageToAdcCountConversion(range : Range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s
                Api.MaximumValue(handle, &maxAdcCounts) |> checkStatusAndTriggerErrorEvent message
                let maxAdcCountsValue = maxAdcCounts            
                let voltageRange = range.ToVolts() 
                
                (fun voltage -> currentResolution.BitMask() &&& int16 (((voltage - analogueOffset) / voltageRange) * float(maxAdcCountsValue)))
                                |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            | GetMaximumDownsamplingRatio(sampleCount, downsampling, memorySegment, replyChannel) ->
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(handle, sampleCount, &maxDownsamplingRatio, downsampling, memorySegment) 
                |> checkStatusAndTriggerErrorEvent message
                maxDownsamplingRatio |> replyChannel.Reply
                return! loop currentResolution isMainsPowered

            // Instructions

            | SetMainsPower(useMainsPower) ->
                let powerStatus = if useMainsPower then PicoStatus.PowerSupplyConnected else PicoStatus.PowerSupplyNotConnected
                let failedDueToMainsPower = 
                    Api.ChangePowerSource(handle, powerStatus) 
                    |> checkStatusAndFailedDueToMainsPower message
                return! loop currentResolution (not failedDueToMainsPower)

            | FlashLedIndefinitely ->
                Api.FlashLed(handle, -1s) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | StopFlashingLed ->
                Api.FlashLed(handle, 0s) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | FlashLed(counts) ->
                if counts <= 0s then 
                    invalidArg "conunts" 
                        "The device LED can only be flashed a positive, non-zero number of times." counts
                Api.FlashLed(handle, counts) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | SetDeviceResolution(newResolution) ->
                let failedDueToMainsPower = 
                    Api.SetDeviceResolution(handle, newResolution) 
                    |> checkStatusAndFailedDueToMainsPower message
                return! loop newResolution (isMainsPowered && not failedDueToMainsPower)

            | SetChannelSettings(channel, channelState) ->
                if not (inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                match channelState with
                | Disabled -> 
                    Api.SetChannel(handle, channel, 0s, Coupling.DC, Range._10V, 0.0f) 
                    |> checkStatusAndTriggerErrorEvent message

                | Enabled(inputSettings) ->
                    Api.SetChannel(
                        handle, channel, 1s, inputSettings.coupling, inputSettings.range, 
                        float32 inputSettings.analogueOffset) 
                    |> checkStatusAndTriggerErrorEvent message
                    
                    Api.SetBandwidthFilter(handle, channel, inputSettings.bandwidthLimit) 
                    |> checkStatusAndTriggerErrorEvent message

                return! loop currentResolution isMainsPowered

            | DisableChannel(channel) ->
                if not (inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetChannel(handle, channel, 0s, Coupling.DC, Range._10V, 0.0f) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered
            
            | DisableTrigger ->
                Api.SetSimpleTrigger(handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, 0s)
                |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | SetAutoTrigger(delay) ->
                Api.SetSimpleTrigger(handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, int16 (float delay * 1000.0))
                |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | SetSimpleTrigger(triggerSettings) ->
                Api.SetSimpleTrigger(handle, 1s, triggerSettings.channel, triggerSettings.adcThreshold, 
                    triggerSettings.thresholdDirection,  triggerSettings.delaySampleCount,
                    int16 (float triggerSettings.autoTriggerDelay * 1000.0))
                |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | SetTriggerDelay(sampleCount) ->
                Api.SetTriggerDelay(handle, sampleCount) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered
                
            | SetNumberOfMemorySegments(memorySegmentCount, replyChannel) ->
                let mutable samplesPerSegement = 0
                Api.MemorySegments(handle, memorySegmentCount, &samplesPerSegement) |> checkStatusAndTriggerErrorEvent message
                samplesPerSegement |> replyChannel.Reply
                return! loop currentResolution isMainsPowered 
                
            // Acquisition

            | SetDataBuffer(channel, buffer, segmentIndex, downsampling) ->
                if downsampling = Downsampling.Aggregate then
                    invalidArg "downsampling"
                        "Attempted to set data buffer with aggregate downsampling. Use SetAggregateDataBuffers instead." downsampling
                if not (inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffer(handle, channel, buffer, buffer.Length, segmentIndex, downsampling) |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex) ->
                if bufferMax.Length <> bufferMin.Length then
                    invalidArg "(bufferMax, bufferMin)"
                        "Attempted to set aggregate data buffers of different lengths" (bufferMax, bufferMin)
                if not (inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffers(handle, channel, bufferMax, bufferMin, bufferMax.Length, segmentIndex, Downsampling.Aggregate) 
                |> checkStatusAndTriggerErrorEvent message
                return! loop currentResolution isMainsPowered

            | RunStreaming(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength, replyChannel) ->
                let (interval, timeUnit) = sampleInterval.ToIntegerIntervalWithTimeUnit()
                let (autoStop, maxPreTriggerSamples, maxPostTriggerSamples) = streamStop.ToAutoStopAndMaxTriggerSamples()
                let mutable hardwareInterval = interval

                let failedDueToMainsPower =
                    Api.RunStreaming(handle, &hardwareInterval, timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop, 
                        downsamplingRatio, downsampling, bufferLength)
                    |> checkStatusAndFailedDueToMainsPower message 

                hardwareInterval.ToInvervalInSecondsFromTimeUnit(timeUnit)
                |> replyChannel.Reply
                return! loop currentResolution (isMainsPowered && not failedDueToMainsPower)

            | GetStreamingLatestValues(callback) ->
                let picoScopeCallback = 
                    PicoScopeStreamingReady(
                        fun _ numberOfSamples startIndex overflows triggeredAt triggered didAutoStop _ ->
                            { numberOfSamples = numberOfSamples
                              startIndex = startIndex
                              voltageOverflows = inputChannels
                                                 |> Set.filter (fun channel -> ((1 <<< int channel) &&& (int overflows)) <> 0)
                              triggerPosition = TriggerPosition.FromTriggeredAndPosition(triggered, startIndex + uint32 triggeredAt)
                              didAutoStop = didAutoStop <> 0s }
                            |> callback)

                let failedDueToMainsPower = 
                    Api.GetStreamingLatestValues(handle, picoScopeCallback, IntPtr.Zero) 
                    |> checkStatusAndFailedDueToMainsPower message
                return! loop currentResolution (isMainsPowered && not failedDueToMainsPower)

            | StopAcquisition(replyChannel) ->
                Api.Stop(handle) |> checkStatusAndTriggerErrorEvent message
                replyChannel.Reply()
                return! loop currentResolution isMainsPowered }
                            
        loop initialResolution wasMainsPoweredInitially)

    new() = new PicoScope5000(null, Resolution._8bit)
    new(initialResolution) = new PicoScope5000(null, initialResolution)
    new(serial) = new PicoScope5000(serial, Resolution._8bit)

    interface IDisposable with
        member IDisposable.Dispose() =
            CloseUnit
            |> agent.PostAndReply

    member this.GetAvailableChannelsAsync() =
        GetAvailableChannels
        |> agent.PostAndAsyncReply

    member this.GetUnitDriverVersionAsync() =
        GetUnitDriverVersion
        |> agent.PostAndAsyncReply

    member this.GetUnitUsbVersionAsync() =
        GetUnitUsbVersion
        |> agent.PostAndAsyncReply
        
    member this.GetUnitHardwareVersionAsync() =
        GetUnitHardwareVersion
        |> agent.PostAndAsyncReply
        
    member this.GetUnitModelNumberAsync() =
        GetUnitModelNumber
        |> agent.PostAndAsyncReply

    member this.GetUnitSerialAsync() =
        GetUnitSerial
        |> agent.PostAndAsyncReply

    member this.GetUnitCalibrationDateAsync() =
        GetUnitCalibrationDate
        |> agent.PostAndAsyncReply

    member this.GetUnitKernelVersionAsync() =
        GetUnitKernelVersion
        |> agent.PostAndAsyncReply
        
    member this.GetUnitDigitalHardwareVersionAsync() =
        GetUnitDigitalHardwareVersion
        |> agent.PostAndAsyncReply

    member this.GetUnitAnalogueHardwareVersionAsync() =
        GetUnitAnalogueHardwareVersion
        |> agent.PostAndAsyncReply

    member this.GetUnitFirmwareVersion1Async() =
        GetUnitFirmwareVersion1
        |> agent.PostAndAsyncReply

    member this.GetUnitFirmwareVersion2Async() =
        GetUnitFirmwareVersion2
        |> agent.PostAndAsyncReply

    member this.GetAllUnitInfoAsync() =
        GetAllUnitInfo 
        |> agent.PostAndAsyncReply

    member this.GetUnitIsMainsPoweredAsync() =
        IsUnitMainsPowered
        |> agent.PostAndAsyncReply

    member this.FlashLedIndefinitely() =
        FlashLedIndefinitely
        |> agent.Post

    member this.FlashLed(flashCount) =
        FlashLed(flashCount)
        |> agent.Post

    member this.StopFlashingLed() =
        StopFlashingLed
        |> agent.Post
    
    member this.PingAsync() =
        Ping
        |> agent.PostAndAsyncReply

    member this.GetTimebaseIntervalInNanosecForSegmentAsync(timebase, memorySegment) =
        fun replyChannel -> GetTimebaseInterval(timebase, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetDeviceResolutionAsync() =
        GetDeviceResolution
        |> agent.PostAndAsyncReply

    member this.GetAnalogueOffsetLimitsAsync(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimits(range, coupling, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetAvailableChannelRangesAsync(channel) =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> agent.PostAndAsyncReply

    member this.SetChannelSettings(channel, channelSettings) =
        SetChannelSettings(channel, channelSettings)
        |> agent.Post

    member this.DisableChannel(channel) =
        DisableChannel(channel)
        |> agent.Post

    member this.DisableTrigger() = 
        DisableTrigger
        |> agent.Post

    member this.SetAutoTrigger(delay) =
        SetAutoTrigger(delay)
        |> agent.Post

    member this.SetSimpleTrigger(triggerSettings) =
        SetSimpleTrigger(triggerSettings)
        |> agent.Post

    member this.SetTrigger(triggerSettings) =
        match triggerSettings with
        | Simple(settings) -> this.SetSimpleTrigger(settings)
        | Auto(delay) -> this.SetAutoTrigger(delay)

    member this.SetTriggerDelay(sampleCount) =
        SetTriggerDelay(sampleCount)
        |> agent.Post

    member this.IsTriggerEnabledAsync() =
        IsTriggerEnabled
        |> agent.PostAndAsyncReply

    member this.SetNumberOfMemorySegments(memorySegments) =
        SetNumberOfMemorySegments(memorySegments)
        |> agent.Post

    member this.GetMaximumNumberOfSegmentsAsync() =
        GetMaximumNumberOfSegments
        |> agent.PostAndAsyncReply

    member this.GetAdcCountToVoltageConversionAsync(range, analogueOffset) =
        fun replyChannel -> GetAdcCountToVoltageConversion(range, analogueOffset, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetVoltageToAdcCountConversionAsync(range, analogueOffset) =
        fun replyChannel -> GetVoltageToAdcCountConversion(range, analogueOffset, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetMaximumDownsamplingRatioAsync(unprocessedSampleCount, downsampling, memorySegment) =
        fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    member this.SetDataBuffer(channel, buffer, segmentIndex, downsampling) =
        SetDataBuffer(channel, buffer, segmentIndex, downsampling)
        |> agent.Post

    member this.SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex) =
        SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex)
        |> agent.Post
        
    member this.RunStreamingAsync(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength) =
        fun replyChannel -> RunStreaming(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetStreamingLatestValues(callback) =
        GetStreamingLatestValues(callback)
        |> agent.Post

    member this.StopAcquisitionAsync() =
        StopAcquisition
        |> agent.PostAndAsyncReply

    static member GetConnectedDeviceSerials() =
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = Api.EnumerateUnits(&count, serials, &stringLength) 
        match status with
        | PicoStatus.Ok -> serials.ToString().Split(",".ToCharArray())
        | PicoStatus.NotFound -> Array.empty
        | status -> raise (PicoException(messageForStatus status, status, "GetConnectedDeviceSerials"))    
