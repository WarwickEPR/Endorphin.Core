namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Errors
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Linq
open System.Text
open log4net
open System.Runtime.InteropServices

type LedFlash =
    | LedOff
    | LedRepeat of counts : int16
    | LedIndefiniteRepeat

type internal Command =
    | ReleaseSession

    // Device info requests
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
    | GetAllUnitInfo of replyChannel : AsyncReplyChannel<(PicoInfo * string) list>

    // Mains power settings - not implemented: currently requiring the device to be mains powered at all times
    // | IsUnitMainsPowered of replyChannel : AsyncReplyChannel<bool> 
    // | SetMainsPower of useMainsPower : bool

    // Device resolution settings - SetDeviceResolution not yet implemented: device resolution is fixed at initialisation
    | GetDeviceResolution of replyChannel : AsyncReplyChannel<Resolution>
    // | SetDeviceResolution of resolution : Resolution

    // Channel setup
    | GetAvailableChannels of replyChannel : AsyncReplyChannel<Set<Channel>>
    | GetAvailableChannelRanges of channel : Channel *  replyChannel : AsyncReplyChannel<Range seq>
    | GetAnalogueOffsetLimits of range : Range * coupling : Coupling * replyChannel : AsyncReplyChannel<float<V> * float<V>>
    | SetChannelSettings of channel : Channel * channelSettings : ChannelSettings
    // | GetAdcCountToVoltageConversion of range : Range * analogueOffset : float<V> * replyChannel : AsyncReplyChannel<Sample -> float<V>>
    // | GetVoltageToAdcCountConversion of range : Range * analogueOffset : float<V> * replyChannel : AsyncReplyChannel<float<V> -> Sample>

    // Trigger setup
    | SetTrigger of triggerSettings : TriggerSettings
    | SetTriggerDelay of sampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    
    // Buffer setup and memory segmentation
    | GetTimebaseInterval of timebase : uint32 * memorySegment : uint32 * replyChannel : AsyncReplyChannel<int<ns> * int32>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : DownsamplingMode * memorySegment : uint32 * replyChannel : AsyncReplyChannel<uint32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | SetNumberOfMemorySegments of memorySegments : uint32 * replyChannel : AsyncReplyChannel<int32>
    | SetDataBuffer of channel : Channel * buffer : Sample array * segmentIndex : uint32 * downsampling : DownsamplingMode
    | SetAggregateDataBuffers of channel : Channel * bufferMax : Sample array * bifferMin : Sample array * segmentIndex : uint32
    | DiscardDataBuffers
    
    // Miscelaneous
    | Ping of replyChannel : AsyncReplyChannel<unit>
    | SetLedFlash of ledFlash : LedFlash

    // Acqusition
    | RunStreaming of streamingParameters : StreamingParameters * replyChannel : AsyncReplyChannel<IStreamingAcquisition>
    | GetStreamingLatestValues of callback : (StreamingValuesReady -> unit) * replyChannel : AsyncReplyChannel<unit>
    | StopAcquisition

type SessionParameters = {
    handle : int16
    serial : string
    resolution : Resolution
    inputChannels : Set<Channel> }

type PicoScope5000(session) =
    static let log = LogManager.GetLogger typeof<PicoScope5000>

    let sessionReleased = new Event<unit>()

    let checkStatus message status =
        match errorMessage status with
        | None -> ()
        | Some error ->
            let exn = PicoException(error, status, message.ToString())
            let logMessage = sprintf "PicoScope %s command %s failed: %s." (session.serial) (message.ToString()) error
            log.Error (logMessage, exn)
            raise exn

    let getUnitInfo info message =
        sprintf "Getting unit info %A from PicoScope %s." info (session.serial) |> log.Info

        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        Api.GetUnitInfo(session.handle, result, resultLength, &requiredLength, info) |> checkStatus message
        result.ToString()

    let getAllUnitInfos message =
        seq { 
            for value in (int PicoInfo.DriverVersion) .. (int PicoInfo.FirmwareVersion2) do
                let info = enum<PicoInfo>(value)
                yield (info, getUnitInfo info message) }
        |> Seq.toList

    let agent = Agent.Start(fun mailbox ->
        let rec preparing dataBuffers = async {           
            sprintf "(Re)entering PicoScope %s agent preparation loop." session.serial |> log.Info
            
            let! message = mailbox.Receive() 
            sprintf "PicoScope %s received messsage %A." session.serial message |> log.Info

            match message with
                            
            | ReleaseSession ->
                sprintf "PicoScope %s releasing session." session.serial |> log.Info
                if mailbox.CurrentQueueLength <> 0 then
                    failwithf "PicoScope %s received ReleaseSession message when message queue is non-empty." session.serial
                sessionReleased.Trigger()

            // Device info requests
            
            | GetUnitDriverVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.DriverVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitUsbVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.UsbVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.HardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitModelNumber replyChannel -> 
                let info = getUnitInfo PicoInfo.ModelNumber message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitSerial replyChannel -> 
                let info = getUnitInfo PicoInfo.SerialNumber message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitCalibrationDate replyChannel -> 
                let info = getUnitInfo PicoInfo.CalibrationDate message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitKernelVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.KernelVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitDigitalHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.DigitalHardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitAnalogueHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.AnalogueHardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitFirmwareVersion1 replyChannel -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion1 message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetUnitFirmwareVersion2 replyChannel -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion2 message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message info |> log.Info
                info |> replyChannel.Reply
                return! preparing dataBuffers

            | GetAllUnitInfo replyChannel -> 
                let infos = getAllUnitInfos message
                let infoStrings =
                    infos
                    |> Seq.map (fun (info, value) -> sprintf "(%A: %s)" info value)
                sprintf "PicoScope %s repsponding to message %A with\n    %A." session.serial message
                    (String.Join("\n   ", infoStrings)) |> log.Info
                infos |> replyChannel.Reply
                return! preparing dataBuffers

            // Device resolution settings

            | GetDeviceResolution replyChannel -> 
                sprintf "PicoScope %s responding to message %A with %A." session.serial message (session.resolution) |> log.Info
                session.resolution |> replyChannel.Reply
                return! preparing dataBuffers

            // Channel setup
            
            | GetAvailableChannels replyChannel ->
                let availableChannels = session.inputChannels
                sprintf "PicoScope %s responding to message %A with %A." session.serial message availableChannels |> log.Info
                availableChannels |> replyChannel.Reply
                return! preparing dataBuffers

            | GetAvailableChannelRanges (channel, replyChannel) ->
                if not (session.inputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                let mutable rangesLength = 12
                let ranges = Array.zeroCreate(rangesLength)
                Api.GetChannelInformation(session.handle, ChannelInfo.VoltageOffsetRanges, 0, ranges, &rangesLength, channel) |> checkStatus message
                let availableChannelRanges = Array.toSeq(ranges).Take(rangesLength)
                sprintf "PicoScope %s responding to message %A with %A." session.serial message availableChannelRanges |> log.Info
                availableChannelRanges |> replyChannel.Reply
                return! preparing dataBuffers

            | GetAnalogueOffsetLimits (range, coupling, replyChannel) ->
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                Api.GetAnalogueOffset(session.handle, range, coupling, &maxOffset, &minOffset) |> checkStatus message
                let offsetLimits = (float maxOffset * 1.0<V>, float minOffset * 1.0<V>)
                sprintf "PicoScope %s responding to message %A with %A." session.serial message offsetLimits |> log.Info
                offsetLimits |> replyChannel.Reply
                return! preparing dataBuffers

            | SetChannelSettings (channel, channelSettings) ->
                if not (session.inputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                match channelSettings with
                | Enabled inputSettings ->
                    Api.SetChannel(
                        session.handle, channel, 1s, inputSettings.coupling, inputSettings.range, 
                        float32 inputSettings.analogueOffset) 
                    |> checkStatus message
                    sprintf "PicoScope %s successfully set channel settings on %A." session.serial channel |> log.Info
                    
                    Api.SetBandwidthFilter(session.handle, channel, inputSettings.bandwidthLimit) 
                    |> checkStatus message
                    sprintf "PicoScope %s successfully set channel bandwidth on %A." session.serial channel |> log.Info

                | Disabled -> 
                    Api.SetChannel(session.handle, channel, 0s, Coupling.DC, Range._10V, 0.0f) 
                    |> checkStatus message
                    sprintf "PicoScope %s successfully disabled %A." session.serial channel |> log.Info

                return! preparing dataBuffers

            // Trigger setup
            
            | IsTriggerEnabled replyChannel ->
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                Api.IsTriggerOrPulseWidthQualifierEnabled(session.handle, &triggerEnabled, &pwqEnabled) |> checkStatus message
                sprintf "PicoScope %s responding to message %A with %A." session.serial message (triggerEnabled <> 0s) |> log.Info
                (triggerEnabled <> 0s) |> replyChannel.Reply
                return! preparing dataBuffers

            | SetTrigger trigger ->
                match trigger with
                | AutoTrigger delay ->
                    Api.SetSimpleTrigger(session.handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, int16 delay)
                    |> checkStatus message
                    sprintf "PicoScope %s successfully set auto triggering with delay %dms." session.serial (int16 delay) |> log.Info
                | SimpleTrigger settings ->
                    let delay =
                        match settings.autoTrigger with
                        | None -> int16 0
                        | Some delayMillisec ->
                            if delayMillisec = 0s<ms> then
                                invalidArg "delay" "AutoTrigger delay must be non-zero. Use 'None' instead." settings.autoTrigger
                            int16 delayMillisec

                    Api.SetSimpleTrigger(session.handle, 1s, settings.channel, settings.adcThreshold, settings.thresholdDirection,
                        settings.delaySamplesAfterTrigger, delay)
                    |> checkStatus message
                    sprintf "PicoScope %s successfully set simple trigger settings." session.serial |> log.Info

                return! preparing dataBuffers

            | SetTriggerDelay sampleCount ->
                Api.SetTriggerDelay(session.handle, sampleCount) |> checkStatus message
                sprintf "PicoScope %s successfully set trigger delay." session.serial |> log.Info
                return! preparing dataBuffers
                
            // Buffer setup and memory segmentation

            | GetTimebaseInterval (timebase, segment, replyChannel) ->
                let mutable interval = 0
                let mutable maxSamples = 0
                Api.GetTimebase(session.handle, timebase, 0, &interval, &maxSamples, segment) |> checkStatus message
                let timebase = (interval * 1<ns>, maxSamples)
                sprintf "PicoScope %s responding to message %A with %A." session.serial message timebase |> log.Info
                timebase |> replyChannel.Reply
                return! preparing dataBuffers

            | GetMaximumDownsamplingRatio (sampleCount, downsampling, memorySegment, replyChannel) ->
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(session.handle, sampleCount, &maxDownsamplingRatio, downsampling, memorySegment) 
                |> checkStatus message
                sprintf "PicoScope %s responding to message %A with %d." session.serial message maxDownsamplingRatio |> log.Info
                maxDownsamplingRatio |> replyChannel.Reply
                return! preparing dataBuffers

            | GetMaximumNumberOfSegments replyChannel ->
                let mutable maxSegments = 0u
                Api.GetMaximumNumberOfSegments(session.handle, &maxSegments) |> checkStatus message
                sprintf "PicoScope %s responding to message %A with %d." session.serial message maxSegments |> log.Info
                maxSegments |> replyChannel.Reply
                return! preparing dataBuffers

            | SetNumberOfMemorySegments (memorySegmentCount, replyChannel) ->
                let mutable samplesPerSegement = 0
                Api.MemorySegments(session.handle, memorySegmentCount, &samplesPerSegement) |> checkStatus message
                sprintf "PicoScope %s succesfully segmented device memory." session.serial |> log.Info

                sprintf "PicoScope %s replying to message %A with %d." session.serial message samplesPerSegement |> log.Info
                samplesPerSegement |> replyChannel.Reply
                return! preparing dataBuffers
                
            | SetDataBuffer (channel, buffer, segmentIndex, downsampling) ->
                if downsampling = DownsamplingMode.Aggregate then
                    invalidArg "downsampling"
                        "Attempted to set data buffer with aggregate downsampling. Use SetAggregateDataBuffers instead." downsampling
                if not (session.inputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffer(session.handle, channel, buffer, buffer.Length, segmentIndex, downsampling) |> checkStatus message
                sprintf "PicoScope %s successfully set data buffer for %A with buffer length %d." session.serial channel (buffer.Length) |> log.Info

                return! preparing (buffer :: dataBuffers)

            | SetAggregateDataBuffers (channel, bufferMax, bufferMin, segmentIndex) ->
                if bufferMax.Length <> bufferMin.Length then
                    invalidArg "(bufferMax, bufferMin)"
                        "Attempted to set aggregate data buffers of different lengths" (bufferMax, bufferMin)
                if not (session.inputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                Api.SetDataBuffers(session.handle, channel, bufferMax, bufferMin, bufferMax.Length, segmentIndex, DownsamplingMode.Aggregate) 
                |> checkStatus message
                
                sprintf "PicoScope %s successfully set aggregate data buffers for %A with buffer length %d." session.serial channel (bufferMax.Length) |> log.Info

                return! preparing (bufferMax :: bufferMin :: dataBuffers)

            | DiscardDataBuffers ->
                Api.Stop(session.handle) |> checkStatus message
                sprintf "PicoScope %s successfully discarded data buffers." session.serial |> log.Info

                return! preparing []

            // Miscelaneous

            | Ping replyChannel -> 
                Api.PingUnit(session.handle) |> checkStatus message
                sprintf "PicoScope %s responding to ping request." session.serial |> log.Info
                replyChannel.Reply()
                return! preparing dataBuffers

            | SetLedFlash ledFlash ->
                match ledFlash with
                | LedOff ->
                    Api.FlashLed(session.handle, 0s) |> checkStatus message
                    sprintf "PicoScope %s successfully stopped LED flashing." |> log.Info
                | LedRepeat counts ->
                    if counts <= 0s then
                        invalidArg "conunts" "The device LED can only be flashed a positive, non-zero number of times." counts
                    Api.FlashLed(session.handle, counts) |> checkStatus message
                    sprintf "PicoScope %s successfully set LED to flash %d times." session.serial counts |> log.Info
                | LedIndefiniteRepeat ->
                    Api.FlashLed(session.handle, -1s) |> checkStatus message
                    sprintf "PicoScope %s successfully set LED to flash indefinitely." session.serial |> log.Info

                return! preparing dataBuffers

            // Acquisition

            | RunStreaming (streamingParameters, replyChannel) ->             
                let (interval, timeUnit) = streamingParameters.sampleInterval.ToIntegerIntervalWithTimeUnit()
                let (autoStop, maxPreTriggerSamples, maxPostTriggerSamples) = streamingParameters.streamStop.ToAutoStopAndMaxTriggerSamples()
                let mutable hardwareInterval = interval

                Api.RunStreaming(session.handle, &hardwareInterval, timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop, 
                    streamingParameters.downsamplingRatio, streamingParameters.downsamplingModes, streamingParameters.bufferLength)
                |> checkStatus message 
                let intervalWithDimension = hardwareInterval.ToInvervalInNanoecondsFromTimeUnit(timeUnit)
                sprintf "PicoScope %s successfully initiated streaming acquisition with sample interval %A ns." 
                    session.serial intervalWithDimension |> log.Info
                
                let acquisition = { new IStreamingAcquisition with
                    member __.SampleInterval = intervalWithDimension

                    member __.GetLatestValues callback =
                        sprintf "PicoScope %s polling for latest streaming values." session.serial |> log.Info
                        (fun replyChannel -> GetStreamingLatestValues(callback, replyChannel)
                        |> mailbox.PostAndAsyncReply)
                    
                    member __.Dispose() = 
                        sprintf "PicoScope %s stopping acquisition." session.serial |> log.Info
                        StopAcquisition |> mailbox.Post }
                    
                sprintf "PicoScope %s replying to message %A with sample initerval %Ans and stop acquisition callback."
                    session.serial message intervalWithDimension |> log.Info
                acquisition |> replyChannel.Reply

                let pinnedBuffers =
                    dataBuffers
                    |> List.map (fun buffer -> 
                        sprintf "PicoScope %s pinned buffer." session.serial |> log.Info
                        (buffer, GCHandle.Alloc(buffer, GCHandleType.Pinned)))

                return! streaming pinnedBuffers
                
            | _ ->
                invalidArg "message"
                    (sprintf "PicoScope %s received invalid message in preparing state." session.serial) message }

        and streaming dataBuffers = async {
            let stopAcquisition reason =
                Api.Stop(session.handle) |> ignore
                dataBuffers
                |> List.iter (fun (_, gcHandle) -> 
                    sprintf "PicoScope %s released GC handle for buffer after %s." session.serial reason |> log.Info
                    gcHandle.Free())

            (sprintf "(Re)entering PicoScope %s agent streaming loop." session.serial) |> log.Info
            
            let! message = mailbox.TryReceive 1000
            if message.IsNone then
                let error = sprintf "PicoScope %s streaming acquisition failed due to timeout." session.serial
                log.Error error
                stopAcquisition "timeout"
                failwith error
             
            (sprintf "PicoScope %s received messsage %A." session.serial message) |> log.Info

            match message.Value with

            | GetStreamingLatestValues(callback, replyChannel) ->
                try
                    sprintf "PicoScope %s creating streaming callback." session.serial |> log.Debug
                    let picoScopeCallback = 
                        PicoScopeStreamingReady(fun _ numberOfSamples startIndex overflows triggeredAt triggered didAutoStop _ ->
                            // wrap the values in a StreamingValuesReady record and send them to the callback
                            { numberOfSamples = numberOfSamples
                              startIndex = startIndex
                              voltageOverflows = 
                                  session.inputChannels
                                  |> Set.filter (fun channel -> ((1 <<< int channel) &&& (int overflows)) <> 0)
                              triggerPosition = TriggerPosition.FromTriggeredAndPosition(triggered, startIndex + uint32 triggeredAt)
                              didAutoStop = didAutoStop <> 0s } |> callback)

                    let status = Api.GetStreamingLatestValues(session.handle, picoScopeCallback, IntPtr.Zero)
                    sprintf "PicoScope %s GetStreamingLatestValues API call status: %A." session.serial status |> log.Debug
                    if status <> PicoStatus.Busy then
                        status |> checkStatus message
                        sprintf "PicoScope %s requested latest streaming values." session.serial |> log.Info
                    else
                        sprintf "PicoScope %s did not request latest streaming values because it is busy." session.serial |> log.Info
                    replyChannel.Reply()
                with
                | exn -> 
                    log.Error (sprintf "PicoScope %s acquisition due to error %A." session.serial exn, exn)
                    stopAcquisition "error while requesting latest values"
                    raise exn

                return! streaming dataBuffers

            | StopAcquisition ->                
                stopAcquisition "finishing stream"
                sprintf "PicoScope %s stopped data acquisition." session.serial |> log.Info
                return! preparing []
            
            | _ ->
                stopAcquisition "unexpected message"
                invalidArg "message"
                    (sprintf "PicoScope %s received invalid message in streaming acquisition state." session.serial) message }

        // initialise in preparing state with empty data buffer list
        preparing [])

    member __.Error = agent.Error
    member __.SessionReleased = sessionReleased.Publish

    interface IDisposable with
        member __.Dispose() =
            ReleaseSession
            |> agent.Post

    // Device info requests

    member __.GetUnitDriverVersionAsync() =
        GetUnitDriverVersion
        |> agent.PostAndAsyncReply

    member __.GetUnitUsbVersionAsync() =
        GetUnitUsbVersion
        |> agent.PostAndAsyncReply
        
    member __.GetUnitHardwareVersionAsync() =
        GetUnitHardwareVersion
        |> agent.PostAndAsyncReply
        
    member __.GetUnitModelNumberAsync() =
        GetUnitModelNumber
        |> agent.PostAndAsyncReply

    member __.GetUnitSerialAsync() =
        GetUnitSerial
        |> agent.PostAndAsyncReply

    member __.GetUnitCalibrationDateAsync() =
        GetUnitCalibrationDate
        |> agent.PostAndAsyncReply

    member __.GetUnitKernelVersionAsync() =
        GetUnitKernelVersion
        |> agent.PostAndAsyncReply
        
    member __.GetUnitDigitalHardwareVersionAsync() =
        GetUnitDigitalHardwareVersion
        |> agent.PostAndAsyncReply

    member __.GetUnitAnalogueHardwareVersionAsync() =
        GetUnitAnalogueHardwareVersion
        |> agent.PostAndAsyncReply

    member __.GetUnitFirmwareVersion1Async() =
        GetUnitFirmwareVersion1
        |> agent.PostAndAsyncReply

    member __.GetUnitFirmwareVersion2Async() =
        GetUnitFirmwareVersion2
        |> agent.PostAndAsyncReply

    member __.GetAllUnitInfoAsync() =
        GetAllUnitInfo 
        |> agent.PostAndAsyncReply

    // Device resolution settings
        
    member __.GetDeviceResolutionAsync() =
        GetDeviceResolution
        |> agent.PostAndAsyncReply

    // Channel settings

    member __.GetAvailableChannelsAsync() =
        GetAvailableChannels
        |> agent.PostAndAsyncReply
        
    member __.GetAvailableChannelRangesAsync channel =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> agent.PostAndAsyncReply
    
    member __.GetAnalogueOffsetLimitsAsync(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimits(range, coupling, replyChannel)
        |> agent.PostAndAsyncReply
        
    member __.SetChannelSettings(channel, channelSettings) =
        SetChannelSettings(channel, channelSettings)
        |> agent.Post

    member __.GetAdcCountToVoltageConversion(range : Range, analogueOffset) =
        let maxAdcCounts = 
            match session.resolution with
            | Resolution._8bit -> 0x8100s
            | _ -> 0x8001s

        let voltageRange = range.ToVolts()

        fun (adcCounts : Sample) -> 
            (voltageRange * (float adcCounts) / (float maxAdcCounts) + analogueOffset)

    member __.GetVoltageToAdcCountConversion(range : Range, analogueOffset) : (float<V> -> Sample) =
        let maxAdcCounts = 
            match session.resolution with
            | Resolution._8bit -> 0x8100s
            | _ -> 0x8001s        

        let voltageRange = range.ToVolts() 

        fun voltage -> 
            int16 (((voltage - analogueOffset) / voltageRange) * (float maxAdcCounts))

    // Trigger settings
    
    member __.IsTriggerEnabledAsync() =
        IsTriggerEnabled
        |> agent.PostAndAsyncReply
        
    member __.SetTrigger triggerSettings =
        SetTrigger triggerSettings
        |> agent.Post

    member __.SetTriggerDelay sampleCount =
        SetTriggerDelay sampleCount
        |> agent.Post

    // Buffer setup and memory segmentation
    
    member __.GetTimebaseInterval(timebase, memorySegment) =
        fun replyChannel -> GetTimebaseInterval(timebase, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    member __.GetMaximumDownsamplingRatioAsync(unprocessedSampleCount, downsampling, memorySegment) =
        fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    member __.GetMaximumNumberOfSegmentsAsync() =
        GetMaximumNumberOfSegments
        |> agent.PostAndAsyncReply

    member __.SetNumberOfMemorySegments memorySegments =
        SetNumberOfMemorySegments memorySegments
        |> agent.Post

    member __.SetDataBuffer(channel, buffer, segmentIndex, downsampling) =
        SetDataBuffer(channel, buffer, segmentIndex, downsampling)
        |> agent.Post

    member __.SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex) =
        SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex)
        |> agent.Post

    member __.DiscardDataBuffers() =
        DiscardDataBuffers
        |> agent.Post

    // Miscelaneous
    
    member __.PingAsync() =
        Ping
        |> agent.PostAndAsyncReply

    member __.SetLedFlash ledFlash =
        SetLedFlash ledFlash
        |> agent.Post
    
    // Acquisition
        
    member __.RunStreamingAsync streamingParameters =
        fun replyChannel -> RunStreaming(streamingParameters, replyChannel)
        |> agent.PostAndAsyncReply