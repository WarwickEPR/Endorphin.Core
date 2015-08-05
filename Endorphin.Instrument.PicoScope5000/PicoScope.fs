namespace Endorphin.Instrument.PicoScope5000

open ExtCore.Control
open System.Text
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Parsing
open StatusCodes
open NativeModel
open Endorphin.Core.CommandRequestAgent

[<RequireQualifiedAccess>]
module PicoScope =
    let private serialNumber device = device.SerialNumber
    let private handle device = device.Handle 
    
    [<AutoOpen>]
    module private Logging =
        let checkStatus =
            function
            | Ok            -> succeed ()
            | Error message -> fail message

        let checkStatusAndReturn value status = choice {
            do! checkStatus status
            return value }

        let checkInitialisationStatus =
            function
            | Ok                            -> succeed MainsPower
            | PowerSourceStatus powerSource -> succeed powerSource
            | Error message                 -> fail message

        let private log = log4net.LogManager.GetLogger typeof<PicoScope5000>
        
        let logOp = log.Info

        let logQueryResult successMessageFunc failureMessageFunc input =
            match input with
            | Success value -> successMessageFunc value |> log.Debug
            | Failure error -> failureMessageFunc error |> log.Error
            input

    let openDevice serial =
        CommandRequestAgent.create (serialNumber >> sprintf "PicoScope %s") true (fun () -> asyncChoice {
            if serial <> null 
            then sprintf "Opening device %s with resolution: %A." serial Resolution_8bit |> logOp
            else sprintf "Opening first available device with resolution: %A." Resolution_8bit |> logOp
        
            let mutable handle = 0s 
            let! powerSource = 
                NativeApi.OpenUnit (&handle, serial, resolutionEnum Resolution_8bit)
                |> checkInitialisationStatus

            let resultLength = 32s
            let result = new StringBuilder(int resultLength)
            let mutable requiredLength = 0s
            let! serial =
                NativeApi.GetUnitInfo(handle, result, resultLength, &requiredLength, DeviceInfoEnum.SerialNumber)
                |> checkStatusAndReturn (result.ToString())
                |> logQueryResult
                    (sprintf "Successfully opened device with power source %A: %s." powerSource)
                    (sprintf "Failed to open device: %s.")

            return { SerialNumber = serial ; Handle = handle } })
        |> AsyncChoice.map PicoScope5000

    let openFirst () = openDevice null

    let pingDevice (PicoScope5000 picoScope) =
        picoScope
        |> CommandRequestAgent.performCommand "Ping" (fun device ->
            NativeApi.PingUnit (handle device) |> checkStatus)

    let close (PicoScope5000 picoScope) =
        picoScope
        |> CommandRequestAgent.close (fun device ->
            NativeApi.CloseUnit (handle device) |> checkStatus)

    let enumerateDevices () =
        sprintf "Enumerating connected devices." |> logOp
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = NativeApi.EnumerateUnits (&count, serials, &stringLength)
        
        match status with
        | Found false -> succeed <| Seq.empty
        | Found true  -> succeed <| (Seq.ofArray <| serials.ToString().Split [| ',' |])
        | error       -> fail    <| statusMessage error
        |> logQueryResult
            (Seq.length >> sprintf "Found %d connected devices.")
            (sprintf "Failed to enumerate devices: %s.")
        |> AsyncChoice.liftChoice

    let setLedFlash (PicoScope5000 picoScope) ledFlash =
        picoScope
        |> CommandRequestAgent.performCommand (sprintf "Set LED flash: %A" ledFlash) (fun device ->
            NativeApi.FlashLed (handle device, ledFlashCounts ledFlash) |> checkStatus)

    let setPowerSource (PicoScope5000 picoScope) powerSource =
        picoScope
        |> CommandRequestAgent.performCommand (sprintf "Set power source: %A" powerSource) (fun device -> 
            NativeApi.ChangePowerSource (handle device, powerSourceStatusCode powerSource) |> checkStatus)

    let queryPowerSource (PicoScope5000 picoScope) =
        picoScope
        |> CommandRequestAgent.performObjectRequest "Query power source" (fun device ->
            let response = NativeApi.CurrentPowerSource (handle device)
            match response with
            | PowerSourceStatus powerSource -> succeed <| powerSource
            | error                         -> fail    <| statusMessage error)
                        
    let queryDeviceInfo (PicoScope5000 picoScope) deviceInfo =
        picoScope
        |> CommandRequestAgent.performObjectRequest (sprintf "Query device information: %A" deviceInfo) (fun device ->
            let resultLength = 32s
            let result = new StringBuilder(int resultLength)
            let mutable requiredLength = 0s
            NativeApi.GetUnitInfo(handle device, result, resultLength, &requiredLength, deviceInfoEnum deviceInfo)
            |> checkStatusAndReturn (result.ToString()))

    module Sampling =
        let setResolution (PicoScope5000 picoScope) resolution =
            picoScope
            |> CommandRequestAgent.performCommand (sprintf "Set device resolution: %A" resolution) (fun device ->
                NativeApi.SetDeviceResolution(handle device, resolutionEnum resolution) |> checkStatus)

        let queryResolution (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performObjectRequest "Query device resolution" (fun device ->
                let mutable resolution = ResolutionEnum._8bit
                NativeApi.GetDeviceResolution(handle device, &resolution)
                |> checkStatusAndReturn (parseResolution resolution))

        let queryMinimumAdcCount (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performValueRequest "Query minimum ADC count value" (fun device ->
                let mutable adcCount : AdcCount = 0s
                NativeApi.MinimumValue(handle device, &adcCount)
                |> checkStatusAndReturn adcCount)

        let queryMaximumAdcCount (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performValueRequest "Query maximum ADC count value." (fun device ->
                let mutable adcCount : AdcCount = 0s
                NativeApi.MaximumValue(handle device, &adcCount)
                |> checkStatusAndReturn adcCount)

        let queryMaximumDownsamplingRatio (PicoScope5000 picoScope) (SampleIndex unaggregatedSamples) downsamplingMode (MemorySegment segment) =
            let description = sprintf "Query maximum downsampling ratio for %A mode, %d samples and %A"
                                downsamplingMode unaggregatedSamples segment
            picoScope
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let mutable downsamplingRatio = 0u
                NativeApi.GetMaximumDownsamplingRatio(handle device, unaggregatedSamples, &downsamplingRatio, downsamplingModeEnum downsamplingMode, segment)
                |> checkStatusAndReturn (DownsamplingRatio downsamplingRatio))

        let queryMaximumMemorySegments (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performObjectRequest "Query maximum number of memory segments" (fun device ->
                let mutable index = 0u 
                NativeApi.GetMaximumNumberOfSegments(handle device, &index)
                |> checkStatusAndReturn (MemorySegment index))

        let private queryIntervalAndMaxSamples (PicoScope5000 picoScope) (Timebase timebase) (MemorySegment index)  = 
            let description = sprintf "Query sample interval and maximum sample count for timebase %d and segment index %d"
                                timebase index
            picoScope
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let mutable interval = 0
                let mutable maxSamples = 0
                let nanosec = LanguagePrimitives.Int32WithMeasure<ns>
                NativeApi.GetTimebase(handle device, timebase, 0, &interval, &maxSamples, index)
                |> checkStatusAndReturn (IntervalInNanoseconds (nanosec interval), SampleCount maxSamples))

        let queryTimebaseParameters picoScope timebase segment =
            asyncChoice {
                let! resolution = queryResolution picoScope
                let! (interval, maxSamples) = queryIntervalAndMaxSamples picoScope timebase segment
                return
                    { Timebase       = timebase
                      Resolution     = resolution 
                      MaximumSamples = maxSamples
                      SampleInterval = interval } }

        let segmentMemory (PicoScope5000 picoScope) (MemorySegment numberOfSegments) =
            picoScope
            |> CommandRequestAgent.performObjectRequest (sprintf "Segmenting device memory into %d segments." numberOfSegments)
                (fun device ->
                    let mutable samplesPerSegment = 0
                    NativeApi.MemorySegments(handle device, numberOfSegments, &samplesPerSegment)
                    |> checkStatusAndReturn (SampleCount samplesPerSegment))

    module ChannelSettings =
        let queryAvailableChannels picoScope =
            asyncChoice {
                let! modelNumber = queryDeviceInfo picoScope ModelNumber
                let! resolution = Sampling.queryResolution picoScope
                let availableChannels = Resolution.availableChannels resolution
                match int <| modelNumber.[1].ToString() with
                | 2 -> return! succeed <| (Set.intersect availableChannels (Set.ofList [ ChannelA ; ChannelB ]))
                | 4 -> return! succeed <| (Set.intersect availableChannels (Set.ofList [ ChannelA ; ChannelB ; ChannelC ; ChannelD ]))
                | _ -> return! fail    <| sprintf "Unexpected model number: %s." modelNumber }

        let queryAvailableAnalogueOffsetRange (PicoScope5000 picoScope) range coupling =
            let description = sprintf "Query available analogue offset range for input range %A with %A coupling" range coupling
            picoScope
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                let volts = LanguagePrimitives.Float32WithMeasure<V>
                NativeApi.GetAnalogueOffset(handle device, rangeEnum range, couplingEnum coupling, &maxOffset, &minOffset)
                |> checkStatusAndReturn (VoltageInVolts (volts maxOffset), VoltageInVolts (volts minOffset)))

        let queryAvaiableChannelRanges (PicoScope5000 picoScope) inputChannel =
            let description = sprintf "Query available input ranges for channel %A" inputChannel
            picoScope 
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let mutable rangesLength = 12
                let ranges = Array.zeroCreate rangesLength
                NativeApi.GetChannelInformation(handle device, ChannelInfoEnum.VoltageOffsetRanges, 0, ranges, &rangesLength, inputChannelEnum inputChannel)
                |> checkStatusAndReturn (ranges |> Array.toSeq |> Seq.map parseRange |> Seq.take rangesLength |> Set.ofSeq))

        let private setBandwidthFilter (PicoScope5000 picoScope) inputChannel bandwidthLimit =
            let description = sprintf "Set bandwidth %A to channel %A" inputChannel bandwidthLimit
            picoScope
            |> CommandRequestAgent.performCommand description (fun device ->
                NativeApi.SetBandwidthFilter(handle device, inputChannelEnum inputChannel, bandwidthLimitEnum bandwidthLimit)
                |> checkStatus)
                    
        let private setChannelInputSettings (PicoScope5000 picoScope) inputChannel inputSettings =
            let description = sprintf "Enable channel %A and setting input settings: %A" inputChannel inputSettings
            picoScope
            |> CommandRequestAgent.performCommand description (fun device ->
                let coupling  = couplingEnum inputSettings.Coupling
                let range     = rangeEnum inputSettings.Range
                let offset    = voltageFloatInVolts inputSettings.AnalogueOffset
                NativeApi.SetChannel(handle device, inputChannelEnum inputChannel, 1s, coupling, range, offset) 
                |> checkStatus)

        let private setChannelEnabled picoScope inputChannel inputSettings =
            asyncChoice {
                do! setChannelInputSettings picoScope inputChannel inputSettings
                do! setBandwidthFilter picoScope inputChannel inputSettings.BandwidthLimit }
        
        let private setChannelDisabled (PicoScope5000 picoScope) inputChannel =
            picoScope
            |> CommandRequestAgent.performCommand (sprintf "Disable channel %A" inputChannel) (fun device ->
                NativeApi.SetChannel(handle device, inputChannelEnum inputChannel, 0s, CouplingEnum.DC, RangeEnum._10V, 0.0f) 
                |> checkStatus)

        let private setChannelSettings picoScope inputChannel channelSettings =
            match channelSettings with
            | EnabledChannel inputSettings -> setChannelEnabled  picoScope inputChannel inputSettings
            | DisabledChannel              -> setChannelDisabled picoScope inputChannel

        let setAcquisitionInputChannels picoScope acquisition =
            asyncChoice {
                let requiredChannels   = Map.keys acquisition.InputSettings
                let! availableChannels = queryAvailableChannels picoScope
                if not (Set.isSubset requiredChannels availableChannels) then
                    return! fail
                    <| "The specified acquisition inputs require input channels which are not available on the current device." 
                for channel in availableChannels do
                    do! Inputs.settingsForChannel channel acquisition
                        |> setChannelSettings picoScope channel }

    module Triggering = 
        let private setAutoTrigger (PicoScope5000 picoScope) (AutoTriggerDelayInMilliseconds delay) =
            let description = sprintf "Set auto-trigger with delay: %d ms" (int16 delay)
            picoScope
            |> CommandRequestAgent.performCommand description (fun device -> 
                NativeApi.SetSimpleTrigger(handle device, 0s, ChannelEnum.A, 0s, ThresholdDirectionEnum.None, 0u, int16 delay)
                |> checkStatus)

        let private setSimpleTrigger (PicoScope5000 picoScope) simpleTriggerSettings =
            let description = sprintf "Set simple trigger settings: %A" simpleTriggerSettings
            picoScope
            |> CommandRequestAgent.performCommand description (fun device ->
                let channel = triggerChannelEnum simpleTriggerSettings.TriggerChannel
                let threshold                  = simpleTriggerSettings.AdcThreshold
                let thresholdDirection = levelThresholdEnum simpleTriggerSettings.ThresholdDirection
                let (SampleIndex startSample)  = simpleTriggerSettings.StartSample
                let delay = autoTriggerDelayIntInMilliseconds simpleTriggerSettings.AutoTrigger
                NativeApi.SetSimpleTrigger(handle device, 1s, channel, threshold, thresholdDirection, startSample, delay)
                |> checkStatus)

        let setTriggerSettings picoScope triggerSettings =
            match triggerSettings with
            | SimpleTrigger simpleTriggerSettings -> setSimpleTrigger picoScope simpleTriggerSettings
            | AutoTrigger delay                   -> setAutoTrigger   picoScope delay

        let queryTriggerStatus (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performObjectRequest "Query trigger status" (fun device ->
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                NativeApi.IsTriggerOrPulseWidthQualifierEnabled(handle device, &triggerEnabled, &pwqEnabled)
                |> checkStatusAndReturn
                    { TriggerState = parseToggleState triggerEnabled ; PulseWidthQualifierState = parseToggleState pwqEnabled })

    module internal DataBuffers =
        let setDataBuffer (PicoScope5000 picoScope) inputChannel downsamplingMode (MemorySegment index) acquisitionBuffer =
            let description = sprintf "Set data buffer for channel %A with downsampling mode %A on memory segment %d"
                                inputChannel downsamplingMode index
            picoScope
            |> CommandRequestAgent.performCommand description (fun device ->
                match acquisitionBuffer with
                | SingleBuffer buffer ->
                    NativeApi.SetDataBuffer(handle device, inputChannelEnum inputChannel, buffer, buffer.Length, index, downsamplingModeEnum downsamplingMode)
                    |> checkStatus
                | BufferPair (bufferMax, bufferMin) ->
                    NativeApi.SetDataBuffers(handle device, inputChannelEnum inputChannel, bufferMax, bufferMin, bufferMax.Length, index,    
                        downsamplingModeEnum downsamplingMode)
                    |> checkStatus)

    module internal Acquisition =
        let stop (PicoScope5000 picoScope) =
            picoScope 
            |> CommandRequestAgent.performCommand "Stop acquisition" (fun device ->
                NativeApi.Stop (handle device) |> checkStatus)

        let queryNumberOfCaptures (PicoScope5000 picoScope) =
            picoScope 
            |> CommandRequestAgent.performObjectRequest "Query number of captures" (fun device ->
                let mutable index = 0u
                NativeApi.GetNumberOfCaptures(handle device, &index)
                |> checkStatusAndReturn (MemorySegment index))

        let queryNumberOfProcessedCaptures (PicoScope5000 picoScope) =
            picoScope
            |> CommandRequestAgent.performObjectRequest "Query number of processed captures" (fun device ->
                let mutable index = 0u
                NativeApi.GetNumberOfProcessedCaptures(handle device, &index)
                |> checkStatusAndReturn (MemorySegment index))

        let private voltageOverflowChannels overflowBits =
            // determine whether any of the corresponding voltage overflow flags is set using the appropriate 
            // bit shifts and integer bitwise AND
            [ ChannelEnum.A ; ChannelEnum.B ; ChannelEnum.C ; ChannelEnum.D ]
            |> List.filter (fun channel -> ((1 <<< int channel) &&& (int overflowBits)) <> 0) 
            |> List.map parseInputChannel
            |> Set.ofList

        let pollStreamingLatestValues (PicoScope5000 picoScope) callback =
            let picoScopeCallback = // define the callback as required by the PicoScope API
                PicoScopeStreamingReady(fun _ numberOfSamples startIndex overflowBits triggeredAt triggered didAutoStop _ ->
                    // wrap the values in a StreamingValuesReady record and send them to the user callback
                    { NumberOfSamples = SampleCount numberOfSamples
                      StartIndex = SampleIndex startIndex
                      VoltageOverflows = voltageOverflowChannels overflowBits
                      TriggerPosition = parseTriggerPosition (triggered <> 0s) (SampleIndex triggeredAt)
                      DidAutoStop = didAutoStop <> 0s } |> callback)
            
            picoScope
            |> CommandRequestAgent.performObjectRequest "Poll for latest streaming values" (fun device ->
                let response = NativeApi.GetStreamingLatestValues(handle device, picoScopeCallback, System.IntPtr.Zero)
                match response with
                | AvailabilityStatus status -> succeed <| status
                | error                     -> fail    <| statusMessage error)

        let queryAvailableStreamingValues (PicoScope5000 picoScope) =
            let description = "Query number of available streaming values after acquisition"
            picoScope
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let mutable sampleIndex = 0u
                NativeApi.NumberOfStreamingValues(handle device, &sampleIndex)
                |> checkStatusAndReturn (SampleIndex sampleIndex))

        let startStreaming (PicoScope5000 picoScope) streamingParameters =
            let description = sprintf "Start streaming acquisition: %A" streamingParameters
            picoScope
            |> CommandRequestAgent.performObjectRequest description (fun device ->
                let (timeUnit, requestedInterval)                     = timeUnitEnumAndInterval streamingParameters.SampleInterval
                let (autoStop, preTriggerSamples, postTriggerSamples) = streamStopParameters streamingParameters.StreamStop
                let (SampleIndex bufferLength)                        = streamingParameters.BufferLength
                
                let downsamplingMode =
                    streamingParameters.Inputs.InputSampling
                    |> Set.map (fun sampling -> sampling.DownsamplingMode)
                    |> downsamplingModeEnumForSet

                let downsamplingRatio = 
                    match streamingParameters.DownsamplingRatio with
                    | Some (DownsamplingRatio downsamplingRatio) -> downsamplingRatio
                    | None                                       -> 1u

                let mutable hardwareInterval = uint32 requestedInterval
                NativeApi.RunStreaming(handle device, &hardwareInterval, timeUnit, preTriggerSamples, postTriggerSamples,
                                        autoStop, downsamplingRatio, downsamplingMode, bufferLength)
                |> checkStatusAndReturn (parseTimeUnitWithInterval (timeUnit, int hardwareInterval)))