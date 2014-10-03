module PicoScope5000Agent

open System
open System.Collections.Generic
open PicoScopeDriver
open Units

type Timebase = uint32
type SampleCount = int32
type ChannelCount = int32
type AdcCount = int16
type MemorySegment = uint32
type DownsamplingRatio = uint32
type UnprocessedSampleCount = uint32

type ChannelSettings = 
    { enabled : bool
      coupling : Coupling
      range : Range
      analogueOffset : float<V> }

type SimpleTriggerSettings =
    { channel : Channel
      threshold : AdcCount
      thresholdDirection : ThresholdDirection
      delaySamples : UnprocessedSampleCount
      autoTriggerDelay : int16<ms> }

type Command =
    | GetUnitDriverVersion of AsyncReplyChannel<string>
    | GetUnitUsbVersion of AsyncReplyChannel<string>
    | GetUnitHardwareVersion of AsyncReplyChannel<string>
    | GetUnitVariantInfo of AsyncReplyChannel<string>
    | GetUnitSerial of AsyncReplyChannel<string>
    | GetUnitCalibrationDate of AsyncReplyChannel<string>
    | GetUnitKernelVersion of AsyncReplyChannel<string>
    | GetUnitDigitalHardwareVersion of AsyncReplyChannel<string>
    | GetUnitAnalogueHardwareVersion of AsyncReplyChannel<string>
    | GetUnitFirmwareVersion1 of AsyncReplyChannel<string>
    | GetUnitFirmwareVersion2 of AsyncReplyChannel<string>
    | GetUnitMacAddress of AsyncReplyChannel<string>
    | GetUnitInfo of AsyncReplyChannel<Dictionary<PicoInfo, string>>
    | GetUnitIsMainsPowered of AsyncReplyChannel<bool>
    | SetUnitIsMainsPowered of bool
    | FlashLedIndefinitely
    | StopFlashingLed
    | FlashLed of Int16
    | Ping of AsyncReplyChannel<unit>
    | GetTimebaseInterval of Timebase * AsyncReplyChannel<float<ns> * SampleCount>
    | GetTimebaseIntervalForSegment of Timebase * MemorySegment * AsyncReplyChannel<float<ns> * SampleCount>
    | GetFastestTimebaseForCurrentResolution of AsyncReplyChannel<Timebase>
    | GetFastestTimebaseForResolution of Resolution * AsyncReplyChannel<Timebase>
    | GetFastestStreamingIntervalForCurrentResolution of ChannelCount * AsyncReplyChannel<float<ns>>
    | GetFastestStreamingIntervalForResolution of Resolution * ChannelCount * AsyncReplyChannel<float<ns>>
    | GetDeviceResolution of AsyncReplyChannel<Resolution>
    | SetDeviceResolution of Resolution
    | GetAnalogueOffsetLimits of Range * Coupling * AsyncReplyChannel<float<V> * float<V>>
    | GetAvailableChannelRanges of Channel * AsyncReplyChannel<Range[]>
    | SetChannelSettings of Channel * ChannelSettings
    | SetChannelBandwidth of Channel * BandwidthLimit
    | DisableChannel of Channel
    | GetMaximumNumberOfChannelsForCurrentResolution of AsyncReplyChannel<ChannelCount>
    | GetMaximumNumberOfChannelsForResolution of Resolution * AsyncReplyChannel<ChannelCount>
    | DiableTrigger
    | SetAutoTrigger of int16<ms>
    | SetSimpleTrigger of SimpleTriggerSettings
    | SetTriggerDelay of UnprocessedSampleCount
    | IsTriggerEnabled of AsyncReplyChannel<bool>
    | GetCurrentMemorySegment of AsyncReplyChannel<MemorySegment>
    | SetCurrentMemorySegment of MemorySegment
    | SetNumberOfMemorySegments of MemorySegment * AsyncReplyChannel<SampleCount>
    | GetMaximumNumberOfSegments of AsyncReplyChannel<MemorySegment>
    | GetAdcCountToVoltageConversion of Range * AsyncReplyChannel<AdcCount -> float<V>>
    | GetVoltageToAdcCountConversion of Range * AsyncReplyChannel<float<V> -> AdcCount>
    | GetMaximumDownsamplingRatio of UnprocessedSampleCount * Downsampling * MemorySegment * AsyncReplyChannel<DownsamplingRatio>
    | GetMaximumDownsamplingRatioForCurrentMemorySegment of UnprocessedSampleCount * Downsampling * AsyncReplyChannel<DownsamplingRatio>
    /// | CreateStreamAgent of ...

let openFirstConnectedDevice() = new PicoScope5000()

let openFirstConnectedDeviceWithResolution (resolution : Resolution) = new PicoScope5000(resolution)

let openBySerialNumber (serial : string) = new PicoScope5000(serial)

let openBySerialNumberWithResolution (serial : string) (resolution : Resolution) = new PicoScope5000(serial, resolution)

let getConnectedDeviceSerials () = PicoScope5000.GetConnectedUnitSerials()

let picoScope5000Mailbox (picoScope : PicoScope5000) =
    fun (mailbox : MailboxProcessor<Command>) ->
        let rec loop() = async {
            let! message = mailbox.Receive() 
            match message with

            // Requests
            
            | GetUnitDriverVersion(replyChannel) -> 
                picoScope.GetUnitDriverVersion() |> replyChannel.Reply

            | GetUnitUsbVersion(replyChannel) -> 
                picoScope.GetUnitUsbVersion() |> replyChannel.Reply

            | GetUnitHardwareVersion(replyChannel) -> 
                picoScope.GetUnitHardwareVersion() |> replyChannel.Reply

            | GetUnitVariantInfo(replyChannel) -> 
                picoScope.GetUnitVariantInfo() |> replyChannel.Reply

            | GetUnitSerial(replyChannel) -> 
                picoScope.GetUnitSerial() |> replyChannel.Reply

            | GetUnitCalibrationDate(replyChannel) -> 
                picoScope.GetUnitCalibrationDate() |> replyChannel.Reply

            | GetUnitKernelVersion(replyChannel) -> 
                picoScope.GetUnitKernelVersion() |> replyChannel.Reply

            | GetUnitDigitalHardwareVersion(replyChannel) -> 
                picoScope.GetUnitDigitalHardwareVersion() |> replyChannel.Reply

            | GetUnitAnalogueHardwareVersion(replyChannel) -> 
                picoScope.GetUnitAnalogueHardwareVersion() |> replyChannel.Reply

            | GetUnitFirmwareVersion1(replyChannel) -> 
                picoScope.GetUnitFirmwareVersion1() |> replyChannel.Reply

            | GetUnitFirmwareVersion2(replyChannel) -> 
                picoScope.GetUnitFirmwareVersion2() |> replyChannel.Reply

            | GetUnitMacAddress(replyChannel) -> 
                picoScope.GetUnitMacAddress() |> replyChannel.Reply

            | GetUnitInfo(replyChannel) -> 
                picoScope.GetUnitInfo() |> replyChannel.Reply

            | GetUnitIsMainsPowered(replyChannel) -> 
                picoScope.UnitIsMainsPowered |> replyChannel.Reply

            | Ping(replyChannel) -> 
                picoScope.Ping()
                replyChannel.Reply()

            | GetTimebaseInterval(timebase, replyChannel) -> 
                let interval, maxSamples = picoScope.GetTimebaseIntervalInNanoseconds(timebase)
                (nanoseconds (float interval), maxSamples) |> replyChannel.Reply

            | GetTimebaseIntervalForSegment(timebase, segment, replyChannel) ->
                let interval, maxSamples = picoScope.GetTimebaseIntervalInNanoseconds(timebase, segment)
                (nanoseconds (float interval), maxSamples) |> replyChannel.Reply

            | GetFastestTimebaseForCurrentResolution(replyChannel) -> 
                picoScope.GetFastestTimebase() |> replyChannel.Reply

            | GetFastestTimebaseForResolution(resolution, replyChannel) -> 
                picoScope.GetFastestTimebase(resolution) |> replyChannel.Reply

            | GetFastestStreamingIntervalForCurrentResolution(channelCount, replyChannel) -> 
                (nanoseconds (float (picoScope.GetFastestStreamingIntervalInNanoseconds(channelCount)))) |> replyChannel.Reply

            | GetFastestStreamingIntervalForResolution(resolution, channelCount, replyChannel) ->
                (nanoseconds (float (picoScope.GetFastestStreamingIntervalInNanoseconds(resolution, channelCount)))) |> replyChannel.Reply

            | GetDeviceResolution(replyChannel) -> 
                picoScope.DeviceResolution |> replyChannel.Reply

            | GetAnalogueOffsetLimits(range, coupling, replyChannel) ->
                let maxOffset, minOffset = picoScope.GetAnalogueOffsetLimits(range, coupling)
                (volts (float maxOffset), volts (float(minOffset))) |> replyChannel.Reply

            | GetAvailableChannelRanges(channel, replyChannel) ->
                picoScope.GetAvailableChannelRanges(channel) |> replyChannel.Reply

            | GetMaximumNumberOfChannelsForCurrentResolution(replyChannel) ->
                picoScope.GetMaximumNumberOfChannels() |> replyChannel.Reply

            | GetMaximumNumberOfChannelsForResolution(resolution, replyChannel) ->
                picoScope.GetMaximumNumberOfChannels(resolution) |> replyChannel.Reply

            | IsTriggerEnabled(replyChannel) ->
                picoScope.IsTriggerEnabled() |> replyChannel.Reply

            | GetCurrentMemorySegment(replyChannel) ->
                picoScope.CurrentSegmentIndex |> replyChannel.Reply

            | SetNumberOfMemorySegments(memorySegmentCount, replyChannel) ->
                picoScope.SetNumberOfSegmentsAndGetSamplesPerSegment(memorySegmentCount) |> replyChannel.Reply

            | GetMaximumNumberOfSegments(replyChannel) ->
                picoScope.GetMaximumNumberOfSegments() |> replyChannel.Reply

            | GetAdcCountToVoltageConversion(range, replyChannel) ->
                let maxAdcCounts = picoScope.GetMaximumAdcCountsForCurrentResolution()
                let range = volts (float (PicoScope5000.RangeInVolts(range)))
                (fun adcCounts -> range * float(adcCounts) / float(maxAdcCounts)) |> replyChannel.Reply

            | GetVoltageToAdcCountConversion(range, replyChannel) ->
                let maxAdcCounts = picoScope.GetMaximumAdcCountsForCurrentResolution()
                let range = volts (float (PicoScope5000.RangeInVolts(range)))
                (fun (voltage : float<V>) -> int16 ((voltage / range) * float(maxAdcCounts))) |> replyChannel.Reply

            | GetMaximumDownsamplingRatio(sampleCount, downsampling, memorySegment, replyChannel) ->
                picoScope.GetMaximumDownsamplingRatio(sampleCount, downsampling, memorySegment) |> replyChannel.Reply

            | GetMaximumDownsamplingRatioForCurrentMemorySegment(sampleCount, downsampling, replyChannel) ->
                picoScope.GetMaximumDownsamplingRatio(sampleCount, downsampling) |> replyChannel.Reply

            // Instructions

            | SetUnitIsMainsPowered(mainsPowered) ->
                picoScope.UnitIsMainsPowered <- true

            | FlashLedIndefinitely ->
                picoScope.FlashLed(-1s)

            | StopFlashingLed ->
                picoScope.FlashLed(0s)

            | FlashLed(counts) ->
                if counts <= 0s then failwith "The device LED can only be flashed a positive, non-zero number of times."
                picoScope.FlashLed(counts)

            | SetDeviceResolution(resolution) ->
                picoScope.DeviceResolution <- resolution

            | SetChannelSettings(channel, channelSettings) ->
                picoScope.SetChannel(channel, channelSettings.enabled, channelSettings.coupling, channelSettings.range, float32 channelSettings.analogueOffset)

            | SetChannelBandwidth(channel, bandwidthLimit) ->
                picoScope.SetBandwidth(channel, bandwidthLimit)

            | DisableChannel(channel) ->
                picoScope.DisableChannel(channel)
            
            | DiableTrigger ->
                picoScope.DisableTrigger()

            | SetAutoTrigger(delay) ->
                picoScope.SetAutoTrigger(int16 delay)

            | SetSimpleTrigger(triggerSettings) ->
                picoScope.SetSimpleTrigger(triggerSettings.channel, triggerSettings.threshold, triggerSettings.thresholdDirection, 
                    triggerSettings.delaySamples, int16 triggerSettings.autoTriggerDelay)

            | SetTriggerDelay(sampleCount) ->
                picoScope.SetTriggerDelay(sampleCount)

            | SetCurrentMemorySegment(memorySegment) ->
                picoScope.CurrentSegmentIndex <- memorySegment

            return! loop() }
            
        loop() 