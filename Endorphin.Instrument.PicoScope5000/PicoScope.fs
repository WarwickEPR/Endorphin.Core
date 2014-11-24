namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Errors
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Linq
open System.Runtime.InteropServices
open System.Text
open log4net

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
    | FlashLed of flashCount : int16
    | Ping of replyChannel : AsyncReplyChannel<unit>
    | GetTimebaseInterval of timebase : uint32 * memorySegment : uint32 * replyChannel : AsyncReplyChannel<int<ns> * int32>
    | GetDeviceResolution of replyChannel : AsyncReplyChannel<Resolution>
    | SetDeviceResolution of resolution : Resolution
    | GetAnalogueOffsetLimits of range : Range * coupling : Coupling * replyChannel : AsyncReplyChannel<float<V> * float<V>>
    | GetAvailableChannels of replyChannel : AsyncReplyChannel<Set<Channel>>
    | GetAvailableChannelRanges of channel : Channel *  replyChannel : AsyncReplyChannel<Range seq>
    | SetChannelSettings of channel : Channel * channelSettings : ChannelSettings
    | SetAutoTrigger of autoTriggerDelay : int16<ms>
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
    | DiscardDataBuffers
    | RunStreaming of sampleInterval : int<ns> * streamStop : StreamStop * downsamplingRatio : uint32 * downsampling : Downsampling * bufferLength : uint32 * replyChannel : AsyncReplyChannel<int<ns> * IDisposable>
    | GetStreamingLatestValues of callback : (StreamingValuesReady -> unit)
    | StopAcquisition

type internal State = {
    handle : int16
    serial : string
    currentResolution : Resolution
    inputChannels : Set<Channel>
    isMainsPowered : bool
    readyBuffers : (int16 array) list
    buffersInUse : (int16 array * GCHandle) list
    stopCapability : CancellationCapability option }

type PicoScope5000(initialisationSerial, initialResolution) =
    static let log = LogManager.GetLogger typeof<PicoScope5000>

    let checkStatusAndFailedDueToMainsPower message serial status =
        try
            checkStatusIsOk message status
            false
        with
        | PicoException(_, status, _) as exn when status = PicoStatus.PowerSupplyNotConnected ->
            (sprintf "PicoScope %s command failed because the power supply is not connected." serial, exn)
            |> log.Error
            true

    let getUnitInfo info command handle serial =
        (sprintf "Getting unit info %A from PicoScope %s." info serial) |> log.Info

        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        Api.GetUnitInfo(handle, result, resultLength, &requiredLength, info) |> checkStatusIsOk command
        result.ToString()

    let getAllUnitInfos message handle serial =
        seq { 
            for value in (int PicoInfo.DriverVersion) .. (int PicoInfo.FirmwareVersion2) do
                let info = enum<PicoInfo>(value)
                yield (info, getUnitInfo info message handle serial) }
        |> Seq.toList

    let agent = Agent.Start(fun mailbox ->
        let rec loop state = async {           
            (sprintf "(Re)entering PicoScope %s agent loop." state.serial) |> log.Info
            
            let! message = mailbox.Receive() 
            (sprintf "PicoScope %s received messsage %A." state.serial message) |> log.Info

            match message with

            | CloseUnit(replyChannel) ->
                if mailbox.CurrentQueueLength <> 0 then
                    failwith (sprintf "PicoScope %s received CloseUnit message when message queue is non-empty." state.serial)
                (sprintf "Closing connection PicoScope %s." state.serial) |> log.Info
                Api.CloseUnit(state.handle) |> checkStatusIsOk message
                (sprintf "Successfully closed connection to PicoScope %s." state.serial) |> log.Info
                replyChannel.Reply()

            // Requests
            
            | GetUnitDriverVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.DriverVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitUsbVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.UsbVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitHardwareVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.HardwareVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitModelNumber(replyChannel) -> 
                let info = getUnitInfo PicoInfo.ModelNumber message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitSerial(replyChannel) -> 
                let info = getUnitInfo PicoInfo.SerialNumber message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitCalibrationDate(replyChannel) -> 
                let info = getUnitInfo PicoInfo.CalibrationDate message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitKernelVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.KernelVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitDigitalHardwareVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.DigitalHardwareVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitAnalogueHardwareVersion(replyChannel) -> 
                let info = getUnitInfo PicoInfo.AnalogueHardwareVersion message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitFirmwareVersion1(replyChannel) -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion1 message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetUnitFirmwareVersion2(replyChannel) -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion2 message state.handle state.serial
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message info) |> log.Info
                info |> replyChannel.Reply
                return! loop state

            | GetAllUnitInfo(replyChannel) -> 
                let infos = getAllUnitInfos message state.handle state.serial
                let infoStrings =
                    infos
                    |> Seq.map (fun (info, value) -> sprintf "(%A: %s)" info value)
                (sprintf "PicoScope %s repsponding to message %A with\n%A." state.serial message
                     (String.Join("\n", infoStrings))) |> log.Info
                infos |> replyChannel.Reply
                return! loop state

            | IsUnitMainsPowered(replyChannel) -> 
                let status = Api.CurrentPowerSource(state.handle)
                let mainsPowered = 
                    match status with
                    | PicoStatus.PowerSupplyConnected -> true
                    | PicoStatus.PowerSupplyNotConnected -> false
                    | _ -> 
                        status |> checkStatusIsOk message
                        false
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message mainsPowered) |> log.Info
                mainsPowered |> replyChannel.Reply
                return! loop { state with isMainsPowered = mainsPowered }

            | Ping(replyChannel) -> 
                let failedDueToMainsPower = 
                    Api.PingUnit(state.handle) 
                    |> checkStatusAndFailedDueToMainsPower message state.serial
                (sprintf "PicoScope %s responding to ping request." state.serial) |> log.Info
                replyChannel.Reply()
                return! loop { state with isMainsPowered = (state.isMainsPowered && not failedDueToMainsPower) }

            | GetTimebaseInterval(timebase, segment, replyChannel) ->
                let mutable interval = 0
                let mutable maxSamples = 0
                Api.GetTimebase(state.handle, timebase, 0, &interval, &maxSamples, segment) |> checkStatusIsOk message
                let timebase = (interval * 1<ns>, maxSamples)
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message timebase) |> log.Info
                timebase |> replyChannel.Reply
                return! loop state

            | GetDeviceResolution(replyChannel) -> 
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message
                    (state.currentResolution)) |> log.Info
                state.currentResolution |> replyChannel.Reply
                return! loop state

            | GetAnalogueOffsetLimits(range, coupling, replyChannel) ->
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                Api.GetAnalogueOffset(state.handle, range, coupling, &maxOffset, &minOffset) |> checkStatusIsOk message
                let offsetLimits = (float maxOffset * 1.0<V>, float minOffset * 1.0<V>)
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message offsetLimits) |> log.Info
                offsetLimits |> replyChannel.Reply
                return! loop state

            | GetAvailableChannels(replyChannel) ->
                let availableChannels =
                    match state.isMainsPowered with
                    | true -> state.inputChannels
                    | false -> 
                        [ Channel.A ; Channel.B ]
                        |> Set.ofList
                        |> Set.intersect state.inputChannels 
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message availableChannels) |> log.Info
                availableChannels |> replyChannel.Reply
                return! loop state

            | GetAvailableChannelRanges(channel, replyChannel) ->
                if not (state.inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                let mutable rangesLength = 12
                let ranges = Array.zeroCreate(rangesLength)
                Api.GetChannelInformation(state.handle, ChannelInfo.VoltageOffsetRanges, 0, ranges, &rangesLength, channel) |> checkStatusIsOk message
                let availableChannelRanges = Array.toSeq(ranges).Take(rangesLength)
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message availableChannelRanges) |> log.Info
                availableChannelRanges |> replyChannel.Reply
                return! loop state

            | IsTriggerEnabled(replyChannel) ->
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                Api.IsTriggerOrPulseWidthQualifierEnabled(state.handle, &triggerEnabled, &pwqEnabled) |> checkStatusIsOk message
                (sprintf "PicoScope %s responding to message %A with %A." state.serial message (triggerEnabled <> 0s)) |> log.Info
                (triggerEnabled <> 0s) |> replyChannel.Reply
                return! loop state

            | GetMaximumNumberOfSegments(replyChannel) ->
                let mutable maxSegments = 0u
                Api.GetMaximumNumberOfSegments(state.handle, &maxSegments) |> checkStatusIsOk message
                (sprintf "PicoScope %s responding to message %A with %d." state.serial message maxSegments) |> log.Info
                maxSegments |> replyChannel.Reply
                return! loop state

            | GetAdcCountToVoltageConversion(range : Range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s 
                Api.MaximumValue(state.handle, &maxAdcCounts) |> checkStatusIsOk message
                let maxAdcCountsValue = maxAdcCounts       
                let voltageRange = range.ToVolts()
                (sprintf "PicoScope %s responding to message %A with ADC -> voltage conversion function with values %A."
                    state.serial message (voltageRange, analogueOffset, maxAdcCounts, state.currentResolution)) |> log.Info

                (fun adcCounts -> 
                    (voltageRange * float(adcCounts) / float(maxAdcCountsValue) + analogueOffset))
                |> replyChannel.Reply
                return! loop state

            | GetVoltageToAdcCountConversion(range : Range, analogueOffset, replyChannel) ->
                let mutable maxAdcCounts = 0s
                Api.MaximumValue(state.handle, &maxAdcCounts) |> checkStatusIsOk message
                let maxAdcCountsValue = maxAdcCounts            
                let voltageRange = range.ToVolts() 
                (sprintf "PicoScope %s responding to message %A with voltage -> ADC conversion function with values %A."
                    state.serial message (voltageRange, analogueOffset, maxAdcCounts, state.currentResolution)) |> log.Info

                (fun voltage -> 
                    int16 (((voltage - analogueOffset) / voltageRange) * float(maxAdcCountsValue)))
                |> replyChannel.Reply
                return! loop state

            | GetMaximumDownsamplingRatio(sampleCount, downsampling, memorySegment, replyChannel) ->
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(state.handle, sampleCount, &maxDownsamplingRatio, downsampling, memorySegment) 
                |> checkStatusIsOk message
                (sprintf "PicoScope %s responding to message %A with %d." state.serial message maxDownsamplingRatio) |> log.Info
                maxDownsamplingRatio |> replyChannel.Reply
                return! loop state

            // Instructions

            | SetMainsPower(useMainsPower) ->
                let powerStatus = if useMainsPower then PicoStatus.PowerSupplyConnected else PicoStatus.PowerSupplyNotConnected
                Api.ChangePowerSource(state.handle, powerStatus) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully changed power source to %s." state.serial
                    (if useMainsPower then "mains power" else "USB power")) |> log.Info
                return! loop { state with isMainsPowered = useMainsPower }

            | FlashLedIndefinitely ->
                Api.FlashLed(state.handle, -1s) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set LED to flash indefinitely." state.serial) |> log.Info
                return! loop state

            | StopFlashingLed ->
                Api.FlashLed(state.handle, 0s) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully stopped LED flashing.") |> log.Info
                return! loop state

            | FlashLed(counts) ->
                if counts <= 0s then
                    invalidArg "conunts" 
                        "The device LED can only be flashed a positive, non-zero number of times." counts
                Api.FlashLed(state.handle, counts) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set LED to flash %d times." state.serial counts) |> log.Info
                return! loop state

            | SetDeviceResolution(newResolution) ->
                let status = 
                    Api.SetDeviceResolution(state.handle, newResolution) 
                let failedDueToMainsPower =
                    status |> checkStatusAndFailedDueToMainsPower message state.serial
                if failedDueToMainsPower then
                    (sprintf "PicoScope %s failed to change resolution to %A because it does not have mains power." 
                        state.serial newResolution) |> log.Warn
                else
                    (sprintf "PicoScope %s successfully changed resolution to %A." state.serial newResolution) |> log.Info

                return! loop 
                    { state with
                        currentResolution = 
                            if failedDueToMainsPower then state.currentResolution 
                            else newResolution 
                        isMainsPowered =
                            (state.isMainsPowered && not failedDueToMainsPower) }
                
            | SetChannelSettings(channel, channelState) ->
                if not (state.inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                match channelState with
                | Enabled(inputSettings) ->
                    Api.SetChannel(
                        state.handle, channel, 1s, inputSettings.coupling, inputSettings.range, 
                        float32 inputSettings.analogueOffset) 
                    |> checkStatusIsOk message
                    (sprintf "PicoScope %s successfully set channel settings on %A." state.serial channel) |> log.Info
                    
                    Api.SetBandwidthFilter(state.handle, channel, inputSettings.bandwidthLimit) 
                    |> checkStatusIsOk message
                    (sprintf "PicoScope %s successfully set channel bandwidth on %A." state.serial channel) |> log.Info

                | Disabled -> 
                    Api.SetChannel(state.handle, channel, 0s, Coupling.DC, Range._10V, 0.0f) 
                    |> checkStatusIsOk message
                    (sprintf "PicoScope %s successfully disabled %A." state.serial channel) |> log.Info

                return! loop state

            | SetAutoTrigger(delay) ->
                Api.SetSimpleTrigger(state.handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, int16 delay)
                |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set auto triggering with delay %dms." state.serial (int16 delay)) |> log.Info
                
                return! loop state

            | SetSimpleTrigger(triggerSettings) ->
                let delay =
                    match triggerSettings.autoTrigger with
                    | None -> int16 0
                    | Some(delayInMillisec) ->
                        if delayInMillisec = 0s<ms> then
                            invalidArg "delay" "AutoTrigger delay must be non-zero. Use 'None' instead." 
                                triggerSettings.autoTrigger
                        int16 delayInMillisec

                Api.SetSimpleTrigger(state.handle, 1s, triggerSettings.channel, triggerSettings.adcThreshold, 
                    triggerSettings.thresholdDirection,  triggerSettings.delaySamplesAfterTrigger, delay)
                |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set simple trigger settings." state.serial) |> log.Info

                return! loop state

            | SetTriggerDelay(sampleCount) ->
                Api.SetTriggerDelay(state.handle, sampleCount) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set trigger delay." state.serial) |> log.Info

                return! loop state
                
            | SetNumberOfMemorySegments(memorySegmentCount, replyChannel) ->
                let mutable samplesPerSegement = 0
                Api.MemorySegments(state.handle, memorySegmentCount, &samplesPerSegement) |> checkStatusIsOk message
                (sprintf "PicoScope %s succesfully segmented device memory." state.serial) |> log.Info

                (sprintf "PicoScope %s replying to message %A with %d." state.serial message samplesPerSegement) |> log.Info
                samplesPerSegement |> replyChannel.Reply

                return! loop state 
                
            // Acquisition

            | SetDataBuffer(channel, buffer, segmentIndex, downsampling) ->
                if downsampling = Downsampling.Aggregate then
                    invalidArg "downsampling"
                        "Attempted to set data buffer with aggregate downsampling. Use SetAggregateDataBuffers instead." downsampling
                if not (state.inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffer(state.handle, channel, buffer, buffer.Length, segmentIndex, downsampling) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set data buffer for %A with buffer length %d." state.serial channel
                    (buffer.Length)) |> log.Info

                return! loop { state with readyBuffers = buffer :: state.readyBuffers }

            | SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex) ->
                if bufferMax.Length <> bufferMin.Length then
                    invalidArg "(bufferMax, bufferMin)"
                        "Attempted to set aggregate data buffers of different lengths" (bufferMax, bufferMin)
                if not (state.inputChannels.Contains(channel)) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffers(state.handle, channel, bufferMax, bufferMin, bufferMax.Length, segmentIndex, Downsampling.Aggregate) 
                |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully set aggregate data buffers for %A with buffer length %d." 
                    state.serial channel (bufferMax.Length)) |> log.Info

                return! loop { state with readyBuffers = bufferMax :: bufferMin :: state.readyBuffers }

            | DiscardDataBuffers ->
                if state.stopCapability.IsSome then
                    invalidArg "message" 
                        "Invalid message 'DiscardDataBuffers': cannot discard data buffers while an acquisition is in progress" message

                Api.Stop(state.handle) |> checkStatusIsOk message
                (sprintf "PicoScope %s successfully discarded data buffers." state.serial) |> log.Info

                return! loop 
                    { state with 
                        readyBuffers = [] }

            | RunStreaming(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength, replyChannel) ->             
                let (interval, timeUnit) = sampleInterval.ToIntegerIntervalWithTimeUnit()
                let (autoStop, maxPreTriggerSamples, maxPostTriggerSamples) = streamStop.ToAutoStopAndMaxTriggerSamples()
                let mutable hardwareInterval = interval

                Api.RunStreaming(state.handle, &hardwareInterval, timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop, 
                    downsamplingRatio, downsampling, bufferLength)
                |> checkStatusIsOk message 
                let intervalWithDimension = hardwareInterval.ToInvervalInNanoecondsFromTimeUnit(timeUnit)
                (sprintf "PicoScope %s successfully initiated streaming acquisition with sample interval %Ans." 
                    state.serial intervalWithDimension) |> log.Info
                
                let stopCapability = new CancellationCapability()
                let acquisition = { new IDisposable with
                    member this.Dispose() = 
                        (sprintf "PicoScope %s stopping acquisition." state.serial) |> log.Info
                        if not stopCapability.IsCancellationRequested then
                            (sprintf "PicoScope %s cancelling acquisition stop token." state.serial) |> log.Info
                            stopCapability.Cancel()
                            mailbox.Post StopAcquisition }

                (sprintf "PicoScope %s replying to message %A with sample initerval %Ans and stop acquisition callback."
                    state.serial message intervalWithDimension) |> log.Info
                (intervalWithDimension, acquisition) |> replyChannel.Reply

                let pinnedBuffers =
                    state.readyBuffers 
                    |> List.map (fun buffer -> 
                        (sprintf "PicoScope %s pinned buffer." state.serial) |> log.Info
                        (buffer, GCHandle.Alloc(buffer, GCHandleType.Pinned)))

                return! loop 
                    { state with 
                        readyBuffers = []
                        buffersInUse = pinnedBuffers
                        stopCapability = Some(stopCapability)}

            | GetStreamingLatestValues(callback) ->
                try
                    if state.stopCapability.IsNone then
                        invalidArg "message" 
                            "Invalid message 'GetStreamingLatestValues': no acquisition in progress." message 

                    (sprintf "PicoScope %s creating streaming callback." state.serial) |> log.Debug
                    let picoScopeCallback = 
                        PicoScopeStreamingReady(
                            fun _ numberOfSamples startIndex overflows triggeredAt triggered didAutoStop _ ->
                                if not state.stopCapability.Value.IsCancellationRequested then
                                    // no log.Info in this performance-critical section.
                                    { numberOfSamples = numberOfSamples
                                      startIndex = startIndex
                                      voltageOverflows = 
                                        state.inputChannels
                                        |> Set.filter (fun channel -> ((1 <<< int channel) &&& (int overflows)) <> 0)
                                      triggerPosition = TriggerPosition.FromTriggeredAndPosition(triggered, startIndex + uint32 triggeredAt)
                                      didAutoStop = didAutoStop <> 0s }
                                    |> callback
                            
                                if didAutoStop <> 0s && not state.stopCapability.Value.IsCancellationRequested then
                                    (sprintf "PicoScope %s streaming acquisition stopped automatically.") |> log.Info )

                    let status = Api.GetStreamingLatestValues(state.handle, picoScopeCallback, IntPtr.Zero)
                    (sprintf "PicoScope %s GetStreamingLatestValues API call status: %A." state.serial status) |> log.Debug
                    if status <> PicoStatus.Busy then
                        status |> checkStatusIsOk message
                        (sprintf "PicoScope %s requested latest streaming values." state.serial) |> log.Info
                    else
                        (sprintf "PicoScope %s did not request latest streaming values because it is busy." state.serial) |> log.Info
                with
                | exn -> 
                    (sprintf "PicoScope %s acquisition due to error %A.\nStack trace:\n%s" state.serial exn exn.StackTrace)
                        |> log.Error
                    Api.Stop(state.handle) |> ignore
                    state.buffersInUse
                    |> List.iter (fun (_, gcHandle) -> 
                        (sprintf "PicoScope %s released GC handle for buffer." state.serial) |> log.Info
                        gcHandle.Free())
                    raise exn

                return! loop state

            | StopAcquisition ->
                if state.stopCapability.IsNone then
                    invalidArg "message" 
                        "Invalid messsage 'StopAcquisition': no acquisition in progress." message
                
                state.buffersInUse
                |> List.iter (fun (_, gcHandle) -> 
                    (sprintf "PicoScope %s released GC handle for buffer." state.serial) |> log.Info
                    gcHandle.Free())

                Api.Stop(state.handle) |> checkStatusIsOk message
                (sprintf "PicoScope %s stopped data acquisition." state.serial) |> log.Info

                state.stopCapability.Value.Dispose()
                return! loop
                    { state with 
                        stopCapability = None } }
        
        and start() = async {
            if initialisationSerial = null then
                ("Starting PicoScope agent for first available device.") |> log.Info
            else
                (sprintf "Starting PicoScope agent for device with serial number %s." initialisationSerial) |> log.Info

            let mutable handle = 0s
            let mainsPowered = 
                let status = Api.OpenUnit(&handle, initialisationSerial, initialResolution)
                match status with
                | PicoStatus.Ok ->
                    (sprintf "Successfully started agent for PicoScope %s with mains power." initialisationSerial) |> log.Info
                    true
                | PicoStatus.PowerSupplyNotConnected ->
                    (sprintf "Successfully started agent for PicoScope %s with USB power." initialisationSerial) |> log.Info
                    false
                | _ -> 
                    (sprintf "Failed to start agent for PicoScope due to error: %s" 
                        (messageForStatus status)) |> log.Error
                    raise (PicoException(messageForStatus(status), status, "Open unit"))
            
            let serial =
                if initialisationSerial = null then
                    let resultLength = 32s
                    let localSerial = new StringBuilder(int resultLength)
                    let mutable requiredLength = 0s
                    Api.GetUnitInfo(handle, localSerial, resultLength, &requiredLength, PicoInfo.SerialNumber) 
                    |> checkStatusIsOk "Get serial number"
                    (sprintf "Successfully retreived serial number %s for PicoScope." (localSerial.ToString())) |> log.Info
                    localSerial.ToString()
                else initialisationSerial

            let modelNumber =
                let resultLength = 32s
                let localModelNumber = new StringBuilder(int resultLength)
                let mutable requiredLength = 0s
                Api.GetUnitInfo(handle, localModelNumber, resultLength, &requiredLength, PicoInfo.ModelNumber) 
                |> checkStatusIsOk "Get model number"
                (sprintf "Successfully retreived model number %s for PicoScope %s." (localModelNumber.ToString()) serial) |> log.Info
                localModelNumber.ToString()
            
            let numberOfInputs = int (modelNumber.[1].ToString()) // number of input channels is the second digit in the model number
            let inputChannels =
                match numberOfInputs with
                | 2 -> Set.ofList [ Channel.A ; Channel.B ]
                | 4 -> Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ]
                | _ -> failwith "Unexpected PicoScope model number."
            (sprintf "PicoScope %s has input channels: %A." serial inputChannels) |> log.Info

            let handleValue = handle
            mailbox.Error.Add(fun exn ->
                match exn with
                | :? PicoException as picoExn ->
                    let (message, status, command) = picoExn.Data0
                    sprintf "Stopping PicoScope %s agent due to error: (message: %A, status: %A, command: %A).\nStack trace:\n%A" 
                        serial message status command (exn.StackTrace)
                    |> log.Error
                | :? ArgumentException as argExn ->
                    sprintf "Stopping PicoScope %s agent due to invalid message arguments.\nStack trace:\n%A" serial (argExn.StackTrace)
                    |> log.Error
                | _ -> 
                    sprintf "Stopping PicoScope %s agent due to error: %A.\nStack tracke:\n%A" serial exn (exn.StackTrace)
                    |> log.Error

                let stopStatus = Api.Stop(handleValue)
                if stopStatus <> PicoStatus.Ok then
                    sprintf "Failed to stop acquisition on PicoScope %s: %s" serial (messageForStatus stopStatus)
                    |> log.Error
                else
                    sprintf "Successfully stopped acquisition on PicoScope %s." initialisationSerial
                    |> log.Error

                let closeStatus = Api.CloseUnit(handleValue)
                if closeStatus <> PicoStatus.Ok then
                    sprintf "Failed to close connection to PicoScope %s: %s" serial (messageForStatus closeStatus)
                    |> log.Error
                else
                    sprintf "Succesfully closed connection to PicoScope %s." serial
                    |> log.Error)

            return! loop {
                handle = handleValue
                serial = serial
                currentResolution = initialResolution
                inputChannels = inputChannels
                isMainsPowered = mainsPowered
                readyBuffers = []
                buffersInUse = []
                stopCapability = None } }

        start())

    member this.Error = agent.Error

    new() = new PicoScope5000(null, Resolution._8bit)
    new(initialResolution) = new PicoScope5000(null, initialResolution)
    new(serial) = new PicoScope5000(serial, Resolution._8bit)

    interface IDisposable with
        member this.Dispose() =
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

    (* not clear how this part of the API should be used.
    member this.SetMainsPower useMainsPower =
        SetMainsPower useMainsPower
        |> agent.Post *) 

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

    (* this seems to cause some problems for the hardware... leaving for later 
    member this.SetDeviceResolution resolution =
        SetDeviceResolution resolution
        |> agent.Post *)

    member this.GetAnalogueOffsetLimitsAsync(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimits(range, coupling, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetAvailableChannelRangesAsync(channel) =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> agent.PostAndAsyncReply

    member this.SetChannelSettings(channel, channelSettings) =
        SetChannelSettings(channel, channelSettings)
        |> agent.Post

    member this.EnableChannel(channel, inputSettings) =
        SetChannelSettings(channel, Enabled(inputSettings))
        |> agent.Post

    member this.DisableChannel(channel) =
        SetChannelSettings(channel, Disabled)
        |> agent.Post

    member this.SetAutoTrigger(delay) =
        SetAutoTrigger(delay)
        |> agent.Post

    member this.SetSimpleTrigger(triggerSettings) =
        SetSimpleTrigger(triggerSettings)
        |> agent.Post

    member this.SetTrigger(triggerSettings) =
        match triggerSettings with
        | SimpleTrigger(settings) -> this.SetSimpleTrigger(settings)
        | AutoTrigger(delay) -> this.SetAutoTrigger(delay)

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

    member this.DiscardDataBuffers() =
        DiscardDataBuffers
        |> agent.Post
        
    member this.RunStreamingAsync(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength) =
        fun replyChannel -> RunStreaming(sampleInterval, streamStop, downsamplingRatio, downsampling, bufferLength, replyChannel)
        |> agent.PostAndAsyncReply

    member this.GetStreamingLatestValues(callback) =
        GetStreamingLatestValues(callback)
        |> agent.Post

    static member GetConnectedDeviceSerials() =
        ("Getting list of connected PicoScope devices.") |> log.Info
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = Api.EnumerateUnits(&count, serials, &stringLength) 
        match status with
        | PicoStatus.Ok ->
            let deviceSerials = serials.ToString().Split(",".ToCharArray())
            (sprintf "Found connected PicoScopes with serials: %A." deviceSerials) |> log.Info
            deviceSerials
        | PicoStatus.NotFound -> 
            ("No devices detected.") |> log.Info
            Array.empty
        | status -> 
            (sprintf "Error while getting list of connected devices: %s." (messageForStatus status)) |> log.Error
            raise (PicoException(messageForStatus status, status, "GetConnectedDeviceSerials"))    
