namespace Endorphin.Instrument.PicoScope5000

open ExtCore.Control
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
module PicoScope =
    
    [<AutoOpen>]
    module private Logging =
        let private log = log4net.LogManager.GetLogger "PicoScope 5000"
        let logOp = log.Info
        let logDeviceOp (picoScope : PicoScope5000) message =
            sprintf "[%A] %s" picoScope message |> logOp

        let logQueryResult successMessageFunc failureMessageFunc input =
            match input with
            | Success value -> successMessageFunc value |> log.Debug
            | Failure error -> failureMessageFunc error |> log.Error
            input

        let logDeviceQueryResult (picoScope : PicoScope5000) successMessageFunc failureMessageFunc =
            logQueryResult 
                (fun value -> sprintf "[%A] %s" picoScope (successMessageFunc value))
                (fun error -> sprintf "[%A] %s" picoScope (failureMessageFunc error))

        let logDeviceOpResult picoScope successMessage = logDeviceQueryResult picoScope (fun _ -> successMessage)

    let private handle (PicoScope5000 h) = h

    let openInstrument serial (Resolution resolution) =
        if serial <> null then sprintf "Opening instrument %s with resolution: %A." serial resolution |> logOp
        else sprintf "Opening first available instrument with resolution: %A." resolution |> logOp
        
        let mutable handle = 0s 
        NativeApi.OpenUnit (&handle, serial, resolution)
        |> checkStatusAndReturn (PicoScope5000 handle)
        |> logQueryResult
            (sprintf "Successfully opened instrument: %A.")
            (sprintf "Failed to open instrument: %s.")

    let openFirst = openInstrument null

    let closeInstrument picoScope =
        logDeviceOp picoScope "Closing instrument." 
        NativeApi.CloseUnit (handle picoScope)
        |> checkStatus
        |> logDeviceOpResult picoScope
            (sprintf "Successfully closed instrument.")
            (sprintf "Failed to close instrument: %s.")

    let enumerateInstruments () =
        sprintf "Enumerating connected instruments." |> logOp
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
            (sprintf "Failed to enumerate instruments: %s.")

    let setLedFlash picoScope ledFlash =
        sprintf "Setting LED flash: %A." ledFlash |> logDeviceOp picoScope
        let (LedFlash count) = ledFlash
        NativeApi.FlashLed (handle picoScope, count)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully set LED flash.")
            (sprintf "Failed to set LED flash: %s.")

    let setPowerSource picoScope powerSource =
        sprintf "Setting power source: %A." powerSource |> logDeviceOp picoScope
        let (PowerSource statusCode) = powerSource
        NativeApi.ChangePowerSource (handle picoScope, statusCode)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully changed power source.")
            (sprintf "Failed to change power source: %s.")
    
    let queryPowerSource picoScope =
        "Querying power source." |> logDeviceOp picoScope
        let response = NativeApi.CurrentPowerSource (handle picoScope)
        match response with
        | PowerSourceStatus powerSource -> succeed <| powerSource
        | error                         -> fail    <| statusMessage error
        |> logDeviceQueryResult picoScope
            (sprintf "Power source: %A.")
            (sprintf "Failed to query power source: %s.")
                        
    let queryDeviceInfo picoScope deviceInfo =
        sprintf "Querying device information: %A." deviceInfo |> logDeviceOp picoScope
        let (DeviceInfo infoEnum) = deviceInfo 
        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        NativeApi.GetUnitInfo(handle picoScope, result, resultLength, &requiredLength, infoEnum)
        |> checkStatusAndReturn (result.ToString())
        |> logDeviceQueryResult picoScope
            (sprintf "%A: %s." deviceInfo)
            (sprintf "Failed to query device info: %s.")

    module internal ChannelSettings =
        let queryAvailableAnalogueOffsetRange picoScope range coupling =
            sprintf "Querying available analogue offset range for input range %A with %A coupling." range coupling |> logDeviceOp picoScope
            let (Range rangeEnum) = range
            let (Coupling couplingEnum) = coupling
            let mutable maxOffset = 0.0f
            let mutable minOffset = 0.0f
            let volts = LanguagePrimitives.Float32WithMeasure<V>
            NativeApi.GetAnalogueOffset(handle picoScope, rangeEnum, couplingEnum, &maxOffset, &minOffset)
            |> checkStatusAndReturn (VoltageInVolts (volts maxOffset), VoltageInVolts (volts minOffset))
            |> logDeviceQueryResult picoScope
                (fun ((VoltageInVolts max), (VoltageInVolts min)) -> sprintf "Channel offset range: %f V to %f V." (float min) (float max))
                (sprintf "Failed to query channel offset range: %s.")

        let queryAvaiableChannelRanges picoScope channel =
            sprintf "Querying available input ranges for channel %A." channel |> logDeviceOp picoScope
            let (InputChannel channelEnum) = channel
            let mutable rangesLength = 12
            let ranges = Array.zeroCreate rangesLength
            NativeApi.GetChannelInformation(handle picoScope, ChannelInfoEnum.VoltageOffsetRanges, 0, ranges, &rangesLength, channelEnum)
            |> checkStatusAndReturn (ranges |> Array.toSeq |> Seq.map parseRange |> Seq.take rangesLength)
            |> logDeviceQueryResult picoScope 
                (Seq.length >> sprintf "Obtained %d input ranges.")
                (sprintf "Failed to query available input ranges: %s.")

        let private setBandwidthFilter picoScope channel bandwidthLimit =
            sprintf "Setting bandwidth %A to channel %A." channel bandwidthLimit |> logDeviceOp picoScope
            let (InputChannel channelEnum) = channel
            let (Bandwidth bandwidthEnum) = bandwidthLimit
            NativeApi.SetBandwidthFilter(handle picoScope, channelEnum, bandwidthEnum)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set bandwidth filter.")
                (sprintf "Failed to set bandwidth filter: %s.")
                    
        let private setChannelInputSettings picoScope channel inputSettings =
            sprintf "Enabling channel %A and setting input settings : %A." channel inputSettings |> logDeviceOp picoScope
            let (InputChannel channelEnum) = channel
            let (Coupling coupling)        = inputSettings.Coupling
            let (Range range)              = inputSettings.Range
            let (VoltageInVolts offset)    = inputSettings.AnalogueOffset
            NativeApi.SetChannel(handle picoScope, channelEnum, 1s, coupling, range, float32 offset) 
            |> checkStatus 
            |> logDeviceOpResult picoScope
                ("Successfully set input settings.")
                (sprintf "Failed to set input settings: %s.")

        let private setChannelEnabled picoScope inputChannel inputSettings =
            choice {
                do! setChannelInputSettings picoScope inputChannel inputSettings
                do! setBandwidthFilter picoScope inputChannel inputSettings.BandwidthLimit }
        
        let private setChannelDisabled picoScope channel =
            sprintf "Disabling channel %A." channel |> logDeviceOp picoScope
            let (InputChannel channelEnum) = channel
            NativeApi.SetChannel(handle picoScope, channelEnum, 0s, CouplingEnum.DC, RangeEnum._10V, 0.0f) 
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully disabled channel.")
                (sprintf "Failed to disable channel: %s.")

        let setChannelSettings picoScope inputChannel channelSettings =
            sprintf "Setting channel %A settings: %A." inputChannel channelSettings |> logDeviceOp picoScope
            match channelSettings with
            | EnabledChannel inputSettings -> setChannelEnabled  picoScope inputChannel inputSettings
            | DisabledChannel              -> setChannelDisabled picoScope inputChannel
            |> logDeviceOpResult picoScope
                ("Successfully set channel settings.")
                (sprintf "Failed to set channel settings: %s.")

    module internal Sampling =
        let setResolution picoScope resolution =
            sprintf "Setting device resolution: %A" resolution |> logDeviceOp picoScope
            let (Resolution resolutionEnum) = resolution
            NativeApi.SetDeviceResolution(handle picoScope, resolutionEnum) 
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set device resolution")
                (sprintf "Failed to set device resolution: %s.")

        let queryResolution picoScope =
            "Querying device resolution." |> logDeviceOp picoScope
            let mutable resolution = ResolutionEnum._8bit
            NativeApi.GetDeviceResolution(handle picoScope, &resolution)
            |> checkStatusAndReturn (parseResolution resolution)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained device resolution: %A.")
                (sprintf "Failed to query device resolution: %s.")

        let queryMinimumAdcCount picoScope =
            "Querying minimum ADC count value." |> logDeviceOp picoScope
            let mutable adcCount : AdcCount = 0s
            NativeApi.MinimumValue(handle picoScope, &adcCount)
            |> checkStatusAndReturn adcCount
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained minimum ADC count value: %d.")
                (sprintf "Failed to query minimum ADC count: %s.")

        let queryMaximumAdcCount picoScope =
            "Querying maximum ADC count value." |> logDeviceOp picoScope
            let mutable adcCount : AdcCount = 0s
            NativeApi.MaximumValue(handle picoScope, &adcCount)
            |> checkStatusAndReturn adcCount
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum ADC count value: %d.")
                (sprintf "Failed to query maximum ADC count: %s.")

        let queryMaximumDownsamplingRatio picoScope (SampleIndex unaggregatedSamples) downsamplingMode (SegmentIndex segment) =
            sprintf "Querying maximum downsampling ratio for %A mode, %d samples and %A."
                downsamplingMode unaggregatedSamples segment |> logDeviceOp picoScope
            let mutable downsamplingRatio = 0u
            NativeApi.GetMaximumDownsamplingRatio(handle picoScope, unaggregatedSamples, &downsamplingRatio, downsamplingMode, segment)
            |> checkStatusAndReturn (DownsamplingRatio downsamplingRatio)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum downsampling ratio: %A.")
                (sprintf "Failed to query maximum downsampling ratio: %s.")

        let queryMaximumMemorySegments picoScope =
            "Querying maximum number of memory segments." |> logDeviceOp picoScope
            let mutable index = 0u 
            NativeApi.GetMaximumNumberOfSegments(handle picoScope, &index)
            |> checkStatusAndReturn (SegmentIndex index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum number of memory segments: %A.")
                (sprintf "Failed to query maximum number of memory segments: %s.")
          
        let private queryIntervalAndMaxSamples picoScope (Timebase timebase) (SegmentIndex segment)  = 
            sprintf "Querying sample interval and maximum sample count for timebase %d and segment index %d."
                timebase segment |> logDeviceOp picoScope
            let mutable interval = 0
            let mutable maxSamples = 0
            let nanosec = LanguagePrimitives.Int32WithMeasure<ns>
            NativeApi.GetTimebase(handle picoScope, timebase, 0, &interval, &maxSamples, segment)
            |> checkStatusAndReturn (IntervalInNanoseconds (nanosec interval), SampleCount maxSamples)
            |> logDeviceQueryResult picoScope 
                (sprintf "Obtained sample interval and maximum number of samples for times: %A.")
                (sprintf "Failed to query sample interval and maximum number of samples: %s.")

        let queryTimebaseParameters picoScope timebase segment =
            sprintf "Querying parameters parameters for %A and %A." timebase segment |> logDeviceOp picoScope
            choice {
                let! resolution = queryResolution picoScope
                let! (interval, maxSamples) = queryIntervalAndMaxSamples picoScope timebase segment
                return
                    { Timebase       = timebase
                      Resolution     = resolution 
                      MaximumSamples = maxSamples
                      SampleInterval = interval } }
            |> logDeviceQueryResult picoScope
                (sprintf "Successfully obtained timebase parameters: %A.")
                (sprintf "Failed to obtain timebase parameters: %s.")

        let segmentMemory picoScope (SegmentIndex numberOfSegments) =
            sprintf "Segmenting device memory into %d segments." numberOfSegments |> logDeviceOp picoScope
            let mutable samplesPerSegment = 0
            NativeApi.MemorySegments(handle picoScope, numberOfSegments, &samplesPerSegment)
            |> checkStatusAndReturn (SampleCount samplesPerSegment)
            |> logDeviceQueryResult picoScope
                (sprintf "Successfully segmented device memory: maximum number of samples per segment: %A.")
                (sprintf "Failed to segment device memory: %s.")

    module internal Triggering = 
        let private setAutoTrigger picoScope (AutoTriggerDelayInMilliseconds delay) =
            sprintf "Setting auto-trigger with delay: %d ms." (int16 delay) |> logDeviceOp picoScope
            NativeApi.SetSimpleTrigger(handle picoScope, 0s, ChannelEnum.A, 0s, ThresholdDirectionEnum.None, 0u, int16 delay)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set auto-trigger.")
                (sprintf "Failed to set auto-trigger delay: %s.")

        let private setSimpleTrigger picoScope simpleTriggerSettings =
            sprintf "Setting simple trigger settings: %A." simpleTriggerSettings |> logDeviceOp picoScope
            let (InputChannel channel)     = simpleTriggerSettings.Channel
            let threshold                  = simpleTriggerSettings.AdcThreshold
            let (LevelThreshold direction) = simpleTriggerSettings.ThresholdDirection
            let (SampleIndex startSample)  = simpleTriggerSettings.StartSample
            let (AutoTriggerDelay delay)   = simpleTriggerSettings.AutoTrigger
            NativeApi.SetSimpleTrigger(handle picoScope, 1s, channel, threshold, direction, startSample, delay)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set simple trigger settings.")
                (sprintf "Failed to set simple trigger settings: %s.")

        let setTriggerSettings picoScope triggerSettings =
            match triggerSettings with
            | SimpleTrigger simpleTriggerSettings -> setSimpleTrigger picoScope simpleTriggerSettings
            | AutoTrigger delay                   -> setAutoTrigger   picoScope delay

        let queryTriggerStatus picoScope =
            "Querying trigger status." |> logDeviceOp picoScope
            let mutable triggerEnabled = 0s
            let mutable pwqEnabled = 0s
            NativeApi.IsTriggerOrPulseWidthQualifierEnabled(handle picoScope, &triggerEnabled, &pwqEnabled)
            |> checkStatusAndReturn { Trigger = parseToggleState triggerEnabled ; PulseWidthQualifier = parseToggleState pwqEnabled }
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained trigger status: %A.")
                (sprintf "Failed to query trigger status: %s.")

    module internal Acquisition =
        let stop picoScope =
            "Stopping acquisition." |> logDeviceOp picoScope 
            NativeApi.Stop (handle picoScope)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully stopped acquisition")
                (sprintf "Failed to stop acquisition: %s.")

        let setDataBuffer picoScope inputChannel (SegmentIndex segment) dataBuffer =
            sprintf "Setting data buffer for input channel %A and segment index %d: %A." inputChannel segment dataBuffer |> logDeviceOp picoScope
            let (InputChannel channelEnum) = inputChannel
            match dataBuffer with
            | SingleBuffer (downsamplingMode, buffer) -> 
                NativeApi.SetDataBuffer(handle picoScope, channelEnum, buffer, buffer.Length, segment, downsamplingMode)
                |> checkStatus
            | BufferPair (downsamplingMode, bufferMax, bufferMin) ->
                NativeApi.SetDataBuffers(handle picoScope, channelEnum, bufferMax, bufferMin, bufferMax.Length, segment, downsamplingMode)
                |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set data buffer.")
                (sprintf "Failed to set data buffer: %s.")

        let queryNumberOfCaptures picoScope =
            "Querying number of captures." |> logDeviceOp picoScope
            let mutable index = 0u
            NativeApi.GetNumberOfCaptures(handle picoScope, &index)
            |> checkStatusAndReturn (SegmentIndex index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of captures: %A.")
                (sprintf "Failed to query number of captures: %s.")

        let queryNumberOfProcessedCaptures picoScope =
            "Querying number of processed captures" |> logDeviceOp picoScope
            let mutable index = 0u
            NativeApi.GetNumberOfProcessedCaptures(handle picoScope, &index)
            |> checkStatusAndReturn (SegmentIndex index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of processed captures: %A.")
                (sprintf "Failed to query number of processed captures: %s.")

        let private voltageOverflowChannels overflowBits =
            // determine whether any of the corresponding voltage overflow flags is set using the appropriate 
            // bit shifts and integer bitwise AND
            [ ChannelEnum.A ; ChannelEnum.B ; ChannelEnum.C ; ChannelEnum.D ]
            |> List.filter (fun channel -> ((1 <<< int channel) &&& (int overflowBits)) <> 0) 
            |> List.map parseInputChannel
            |> Set.ofList

        let requestStreamingLatestValues picoScope callback =
            let picoScopeCallback = // define the callback as required by the PicoScope API
                PicoScopeStreamingReady(fun _ numberOfSamples startIndex overflowBits triggeredAt triggered didAutoStop _ ->
                    // wrap the values in a StreamingValuesReady record and send them to the user callback
                    { NumberOfSamples = SampleCount numberOfSamples
                      StartIndex = SampleIndex startIndex
                      VoltageOverflows = voltageOverflowChannels overflowBits
                      TriggerPosition = parseTriggerPosition (triggered <> 0s) (SampleIndex triggeredAt)
                      DidAutoStop = didAutoStop <> 0s } |> callback)

            "Polling for latest streaming values..." |> logDeviceOp picoScope
            let response = NativeApi.GetStreamingLatestValues(handle picoScope, picoScopeCallback, System.IntPtr.Zero)
            match response with
            | AvailabilityStatus status -> succeed <| status
            | error                     -> fail    <| statusMessage error
            |> logDeviceOpResult picoScope
                ("Successfully polled for latest streaming values.")
                (sprintf "Failed to poll for latest streaming values: %A.")

        let queryAvailableStreamingValues picoScope =
            "Querying number of available streaming values after acquisition." |> logDeviceOp picoScope
            let mutable sampleIndex = 0u
            NativeApi.NumberOfStreamingValues(handle picoScope, &sampleIndex)
            |> checkStatusAndReturn (SampleIndex sampleIndex)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of available streaming samples: %A.")
                (sprintf "Failed to query number of available streaming samples: %s.")

        let runStreaming picoScope streamingParameters =
            sprintf "Starting streaming acquistion: %A." streamingParameters |> logDeviceOp picoScope 
            let (Interval (timeUnit, requestedInterval))                                 = streamingParameters.SampleInterval
            let (StreamStopParameters (autoStop, preTriggerSamples, postTriggerSamples)) = streamingParameters.StreamStop
            let (DownsamplingRatio downsamplingRatio)                                    = streamingParameters.DownsamplingRatio
            let (SampleIndex bufferLength)                                               = streamingParameters.BufferLength
            let downsamplingMode                                                         = bufferListDownsamplingMode streamingParameters.DataBuffers
            let mutable hardwareInterval = uint32 requestedInterval
            NativeApi.RunStreaming(handle picoScope, &hardwareInterval, timeUnit, preTriggerSamples, postTriggerSamples, autoStop, downsamplingRatio, downsamplingMode, bufferLength)
            |> checkStatusAndReturn (parseTimeUnitWithInterval (timeUnit, int hardwareInterval))
            |> logDeviceQueryResult picoScope
                (sprintf "Successfully started streaming acquisition with sample interval: %A.")
                (sprintf "Failed to start streaming acquisition: %s.")