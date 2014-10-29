module FSharp

open Endorphin.Instrument.PicoScope5000

let observe channel downsampling (streamAgent : StreamAgent) =
    fun replyChannel -> Observe(channel, downsampling, replyChannel)
    |> streamAgent.PostAndReply

let observeAggregate channel (streamAgent : StreamAgent) =
    fun replyChannel -> ObserveAggregate(channel, replyChannel)
    |> streamAgent.PostAndReply

let observeAgentStatus (streamAgent : StreamAgent) =
    ObserveAgentStatus
    |> streamAgent.PostAndReply

let runStream streamingParameters (streamAgent : StreamAgent) =
    fun replyChannel -> RunStream(streamingParameters, replyChannel)
    |> streamAgent.PostAndReply

let stopStream (streamAgent : StreamAgent) =
    streamAgent.Post(StopStream)

let viewData (streamAgent : StreamAgent) =
    ViewData
    |> streamAgent.PostAndReply

let viewDataAsync (streamAgent : StreamAgent) =
    ViewData
    |> streamAgent.PostAndAsyncReply

let unitDriverVersion (picoScopeAgent : PicoScope5000Agent) =
    GetUnitDriverVersion
    |> picoScopeAgent.PostAndReply

let unitDriverVersionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitDriverVersion
    |> picoScopeAgent.PostAndAsyncReply

let unitUsbVersion (picoScopeAgent : PicoScope5000Agent) =
    GetUnitUsbVersion
    |> picoScopeAgent.PostAndReply

let unitUsbVersionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitUsbVersion
    |> picoScopeAgent.PostAndAsyncReply

let unitHardwareVersion (picoScopeAgent : PicoScope5000Agent) =
    GetUnitHardwareVersion
    |> picoScopeAgent.PostAndReply

let unitHardwareVersionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitHardwareVersion
    |> picoScopeAgent.PostAndAsyncReply
    
let unitVariantInfo (picoScopeAgent : PicoScope5000Agent) =
    GetUnitVariantInfo
    |> picoScopeAgent.PostAndReply

let unitVariantInfoAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitVariantInfo
    |> picoScopeAgent.PostAndAsyncReply
    
let unitSerial (picoScopeAgent : PicoScope5000Agent) =
    GetUnitSerial
    |> picoScopeAgent.PostAndReply

let unitSerialAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitSerial
    |> picoScopeAgent.PostAndAsyncReply
    
let unitCalibrationDate (picoScopeAgent : PicoScope5000Agent) =
    GetUnitCalibrationDate
    |> picoScopeAgent.PostAndReply

let unitCalibrationDateAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitCalibrationDate
    |> picoScopeAgent.PostAndAsyncReply
    
let unitKernelVersion (picoScopeAgent : PicoScope5000Agent) =
    GetUnitKernelVersion
    |> picoScopeAgent.PostAndReply

let unitKernelVersionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitKernelVersion
    |> picoScopeAgent.PostAndAsyncReply
    
let unitDigitalHardwareVersion (picoScopeAgent : PicoScope5000Agent) =
    GetUnitDigitalHardwareVersion
    |> picoScopeAgent.PostAndReply

let unitDigitalHardwareVersionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitDigitalHardwareVersion
    |> picoScopeAgent.PostAndAsyncReply
    
let unitFirmwareVersion1 (picoScopeAgent : PicoScope5000Agent) =
    GetUnitFirmwareVersion1
    |> picoScopeAgent.PostAndReply

let unitFirmwareVersion1Async (picoScopeAgent : PicoScope5000Agent) =
    GetUnitFirmwareVersion1
    |> picoScopeAgent.PostAndAsyncReply

let unitFirmwareVersion2 (picoScopeAgent : PicoScope5000Agent) =
    GetUnitFirmwareVersion2
    |> picoScopeAgent.PostAndReply

let unitFirmwareVersion2Async (picoScopeAgent : PicoScope5000Agent) =
    GetUnitFirmwareVersion2
    |> picoScopeAgent.PostAndAsyncReply

let unitInfo (picoScopeAgent : PicoScope5000Agent) =
    picoScopeAgent.PostAndReply(GetUnitInfo)

let unitInfoAsync (picoScopeAgent : PicoScope5000Agent) =
    GetUnitInfo 
    |> picoScopeAgent.PostAndAsyncReply

let unitIsMainsPowered (picoScopeAgent : PicoScope5000Agent) =
    IsUnitMainsPowered
    |> picoScopeAgent.PostAndReply

let unitIsMainsPoweredAsync (picoScopeAgent : PicoScope5000Agent) =
    IsUnitMainsPowered
    |> picoScopeAgent.PostAndAsyncReply

let setUnitIsMainsPowered mainsPowered (picoScopeAgent : PicoScope5000Agent) =
    SetUnitIsMainsPowered(mainsPowered)
    |> picoScopeAgent.Post

let flashLedIndefinitely (picoScopeAgent : PicoScope5000Agent) =
    FlashLedIndefinitely
    |> picoScopeAgent.Post

let flashLed flashCount (picoScopeAgent : PicoScope5000Agent) =
    FlashLed(flashCount)
    |> picoScopeAgent.Post

let stopFlashingLed (picoScopeAgent : PicoScope5000Agent) =
    StopFlashingLed
    |> picoScopeAgent.Post

let ping (picoScopeAgent : PicoScope5000Agent) =
    Ping
    |> picoScopeAgent.PostAndReply

let pingAsync (picoScopeAgent : PicoScope5000Agent) =
    Ping
    |> picoScopeAgent.PostAndAsyncReply
    
let timebaseIntervalInNanosec timebase (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetTimebaseIntervalInNanosec(timebase, replyChannel)
    |> picoScopeAgent.PostAndReply

let timebaseIntervalInNanosecAsync timebase (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetTimebaseIntervalInNanosec(timebase, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply
    
let timebaseIntervalInNanosecForSegment timebase memorySegment (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetTimebaseIntervalInNanosecForSegment(timebase, memorySegment, replyChannel)
    |> picoScopeAgent.PostAndReply

let timebaseIntervalInNanosecForSegmentAsync timebase memorySegment (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetTimebaseIntervalInNanosecForSegment(timebase, memorySegment, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply

let deviceResolution (picoScopeAgent : PicoScope5000Agent) =
    GetDeviceResolution
    |> picoScopeAgent.PostAndReply

let deviceResolutionAsync (picoScopeAgent : PicoScope5000Agent) =
    GetDeviceResolution
    |> picoScopeAgent.PostAndAsyncReply

let setDeviceResolution resolution (picoScopeAgent : PicoScope5000Agent) =
    SetDeviceResolution(resolution)
    |> picoScopeAgent.Post

let analogueOffsetLimitsInVolts range coupling (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAnalogueOffsetLimitsInVolts(range, coupling, replyChannel)
    |> picoScopeAgent.PostAndReply

let analogueOffsetLimitsInVoltsAsync range coupling (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAnalogueOffsetLimitsInVolts(range, coupling, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply

let availableChannelRanges channel (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
    |> picoScopeAgent.PostAndReply

let availableChannelRangesAsync channel (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply

let setChannelSettings channelSettings channel (picoScopeAgent : PicoScope5000Agent) =
    SetChannelSettings(channel, channelSettings)
    |> picoScopeAgent.Post

let setChannelBandwidth channel bandwidthLimit (picoScopeAgent : PicoScope5000Agent) =
    SetChannelBandwidth(channel, bandwidthLimit)
    |> picoScopeAgent.Post

let disableChannel channel (picoScopeAgent : PicoScope5000Agent) =
    DisableChannel(channel)
    |> picoScopeAgent.Post

let disableTrigger (picoScopeAgent : PicoScope5000Agent) = 
    DisableTrigger
    |> picoScopeAgent.Post

let setAutoTrigger delayInMillisec (picoScopeAgent : PicoScope5000Agent) =
    SetAutoTrigger(delayInMillisec)
    |> picoScopeAgent.Post

let setSimpleTrigger triggerSettings (picoScopeAgent : PicoScope5000Agent) =
    SetSimpleTrigger(triggerSettings)
    |> picoScopeAgent.Post

let setTriggerDelay sampleCount (picoScopeAgent : PicoScope5000Agent) =
    SetTriggerDelay(sampleCount)
    |> picoScopeAgent.Post

let isTriggerEnabled (picoScopeAgent : PicoScope5000Agent) =
    IsTriggerEnabled
    |> picoScopeAgent.PostAndReply

let isTriggerEnabledAsync (picoScopeAgent : PicoScope5000Agent) =
    IsTriggerEnabled
    |> picoScopeAgent.PostAndAsyncReply

let currentMemorySegment (picoScopeAgent : PicoScope5000Agent) =
    GetCurrentMemorySegment
    |> picoScopeAgent.PostAndReply

let currentMemorySegmentAsync (picoScopeAgent : PicoScope5000Agent) =
    GetCurrentMemorySegment
    |> picoScopeAgent.PostAndAsyncReply

let surrentMemorySegment memorySegment (picoScopeAgent : PicoScope5000Agent) =
    SetCurrentMemorySegment(memorySegment)
    |> picoScopeAgent.Post

let numberOfMemorySegments memorySegments (picoScopeAgent : PicoScope5000Agent) =
    SetNumberOfMemorySegments(memorySegments)
    |> picoScopeAgent.Post
 
let maximumNumberOfSegments (picoScopeAgent : PicoScope5000Agent) =
    GetMaximumNumberOfSegments
    |> picoScopeAgent.PostAndReply

let maximumNumberOfSegmentsAsync (picoScopeAgent : PicoScope5000Agent) =
    GetMaximumNumberOfSegments
    |> picoScopeAgent.PostAndAsyncReply

let adcCountToVoltsConversion range analogueOffsetInVolts (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAdcCountToVoltsConversion(range, analogueOffsetInVolts, replyChannel)
    |> picoScopeAgent.PostAndReply

let adcCountToVoltsConversionAsync range analogueOffsetInVolts (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetAdcCountToVoltsConversion(range, analogueOffsetInVolts, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply
    
let voltsToAdcCountConversion range analogueOffsetInVolts (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetVoltsToAdcCountConversion(range, analogueOffsetInVolts, replyChannel)
    |> picoScopeAgent.PostAndReply

let voltsToAdcCountConversionAsync range analogueOffsetInVolts (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetVoltsToAdcCountConversion(range, analogueOffsetInVolts, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply

let maximumDownsamplingRatio unprocessedSampleCount downsampling memorySegment (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
    |> picoScopeAgent.PostAndReply

let maximumDownsamplingRatioAsync unprocessedSampleCount downsampling memorySegment (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply
    
let maximumDownsamplingRatioForCurrentMemorySegment unprocessedSampleCount downsampling (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetMaximumDownsamplingRatioForCurrentMemorySegment(unprocessedSampleCount, downsampling, replyChannel)
    |> picoScopeAgent.PostAndReply

let maximumDownsamplingRatioForCurrentMemorySegmentAsync unprocessedSampleCount downsampling (picoScopeAgent : PicoScope5000Agent) =
    fun replyChannel -> GetMaximumDownsamplingRatioForCurrentMemorySegment(unprocessedSampleCount, downsampling, replyChannel)
    |> picoScopeAgent.PostAndAsyncReply
    
let createStreamAgent (picoScopeAgent : PicoScope5000Agent) =
    CreateStreamAgent
    |> picoScopeAgent.PostAndReply

let createStreamAgentAsync (picoScopeAgent : PicoScope5000Agent) =
    CreateStreamAgent
    |> picoScopeAgent.PostAndAsyncReply
    
let connectedDeviceSerials =
    PicoScope5000Agent.GetConnectedDeviceSerials()