namespace Endorphin.Instrument.PicoScope5000

open ExtCore.Control
open System.Text
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Parsing
open StatusCodes
open NativeModel

[<RequireQualifiedAccess>]
module PicoScope =
    
    [<AutoOpen>]
    module private Logging =
        let checkStatus =
            function
            | Ok            -> succeed ()
            | Error message -> fail message

        let checkStatusAndReturn value status = choice {
            do! checkStatus status
            return value }

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

    let openInstrument serial =
        if serial <> null then sprintf "Opening instrument %s with resolution: %A." serial Resolution_8bit |> logOp
        else sprintf "Opening first available instrument with resolution: %A." Resolution_8bit |> logOp
        
        let mutable handle = 0s 
        let status = NativeApi.OpenUnit (&handle, serial, resolutionEnum Resolution_8bit)
        
        match status with
        | PowerSourceStatus powerSource -> logOp <| sprintf "Opened instrument with power source: %A." powerSource
                                           succeed <| (PicoScope5000 handle)
        | Ok                            -> succeed <| (PicoScope5000 handle)
        | error                         -> fail    <| statusMessage error
        |> logQueryResult
            (sprintf "Successfully opened instrument: %A.")
            (sprintf "Failed to open instrument: %s.")
        |> AsyncChoice.liftChoice

    let openFirst () = openInstrument null

    let pingInstrument picoScope =
        logDeviceOp picoScope "Pinging instrument."
        NativeApi.PingUnit (handle picoScope)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully pinged instrument.")
            (sprintf "Failed to ping instrument: %s.")
        |> AsyncChoice.liftChoice

    let closeInstrument picoScope =
        logDeviceOp picoScope "Closing instrument." 
        NativeApi.CloseUnit (handle picoScope)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully closed instrument.")
            (sprintf "Failed to close instrument: %s.")
        |> AsyncChoice.liftChoice

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
        |> AsyncChoice.liftChoice

    let setLedFlash picoScope ledFlash =
        sprintf "Setting LED flash: %A." ledFlash |> logDeviceOp picoScope
        NativeApi.FlashLed (handle picoScope, ledFlashCounts ledFlash)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully set LED flash.")
            (sprintf "Failed to set LED flash: %s.")
        |> AsyncChoice.liftChoice

    let setPowerSource picoScope powerSource =
        sprintf "Setting power source: %A." powerSource |> logDeviceOp picoScope
        NativeApi.ChangePowerSource (handle picoScope, powerSourceStatusCode powerSource)
        |> checkStatus
        |> logDeviceOpResult picoScope
            ("Successfully changed power source.")
            (sprintf "Failed to change power source: %s.")
        |> AsyncChoice.liftChoice

    let queryPowerSource picoScope =
        "Querying power source." |> logDeviceOp picoScope
        let response = NativeApi.CurrentPowerSource (handle picoScope)
        match response with
        | PowerSourceStatus powerSource -> succeed <| powerSource
        | error                         -> fail    <| statusMessage error
        |> logDeviceQueryResult picoScope
            (sprintf "Power source: %A.")
            (sprintf "Failed to query power source: %s.")
        |> AsyncChoice.liftChoice
                        
    let queryDeviceInfo picoScope deviceInfo =
        sprintf "Querying device information: %A." deviceInfo |> logDeviceOp picoScope
        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        NativeApi.GetUnitInfo(handle picoScope, result, resultLength, &requiredLength, deviceInfoEnum deviceInfo)
        |> checkStatusAndReturn (result.ToString())
        |> logDeviceQueryResult picoScope
            (sprintf "%A: %s." deviceInfo)
            (sprintf "Failed to query device info: %s.")
        |> AsyncChoice.liftChoice

    module Sampling =
        let setResolution picoScope resolution =
            sprintf "Setting device resolution: %A" resolution |> logDeviceOp picoScope
            NativeApi.SetDeviceResolution(handle picoScope, resolutionEnum resolution) 
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set device resolution")
                (sprintf "Failed to set device resolution: %s.")
            |> AsyncChoice.liftChoice

        let queryResolution picoScope =
            "Querying device resolution." |> logDeviceOp picoScope
            let mutable resolution = ResolutionEnum._8bit
            NativeApi.GetDeviceResolution(handle picoScope, &resolution)
            |> checkStatusAndReturn (parseResolution resolution)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained device resolution: %A.")
                (sprintf "Failed to query device resolution: %s.")
            |> AsyncChoice.liftChoice

        let queryMinimumAdcCount picoScope =
            "Querying minimum ADC count value." |> logDeviceOp picoScope
            let mutable adcCount : AdcCount = 0s
            NativeApi.MinimumValue(handle picoScope, &adcCount)
            |> checkStatusAndReturn adcCount
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained minimum ADC count value: %d.")
                (sprintf "Failed to query minimum ADC count: %s.")
            |> AsyncChoice.liftChoice

        let queryMaximumAdcCount picoScope =
            "Querying maximum ADC count value." |> logDeviceOp picoScope
            let mutable adcCount : AdcCount = 0s
            NativeApi.MaximumValue(handle picoScope, &adcCount)
            |> checkStatusAndReturn adcCount
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum ADC count value: %d.")
                (sprintf "Failed to query maximum ADC count: %s.")

        let queryMaximumDownsamplingRatio picoScope (SampleIndex unaggregatedSamples) downsamplingMode (MemorySegment segment) =
            sprintf "Querying maximum downsampling ratio for %A mode, %d samples and %A."
                downsamplingMode unaggregatedSamples segment |> logDeviceOp picoScope
            let mutable downsamplingRatio = 0u
            NativeApi.GetMaximumDownsamplingRatio(handle picoScope, unaggregatedSamples, &downsamplingRatio, downsamplingModeEnum downsamplingMode, segment)
            |> checkStatusAndReturn (DownsamplingRatio downsamplingRatio)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum downsampling ratio: %A.")
                (sprintf "Failed to query maximum downsampling ratio: %s.")
            |> AsyncChoice.liftChoice

        let queryMaximumMemorySegments picoScope =
            "Querying maximum number of memory segments." |> logDeviceOp picoScope
            let mutable index = 0u 
            NativeApi.GetMaximumNumberOfSegments(handle picoScope, &index)
            |> checkStatusAndReturn (MemorySegment index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained maximum number of memory segments: %A.")
                (sprintf "Failed to query maximum number of memory segments: %s.")
            |> AsyncChoice.liftChoice

        let private queryIntervalAndMaxSamples picoScope (Timebase timebase) (MemorySegment index)  = 
            sprintf "Querying sample interval and maximum sample count for timebase %d and segment index %d."
                timebase index |> logDeviceOp picoScope
            let mutable interval = 0
            let mutable maxSamples = 0
            let nanosec = LanguagePrimitives.Int32WithMeasure<ns>
            NativeApi.GetTimebase(handle picoScope, timebase, 0, &interval, &maxSamples, index)
            |> checkStatusAndReturn (IntervalInNanoseconds (nanosec interval), SampleCount maxSamples)
            |> logDeviceQueryResult picoScope 
                (sprintf "Obtained sample interval and maximum number of samples for times: %A.")
                (sprintf "Failed to query sample interval and maximum number of samples: %s.")
            |> AsyncChoice.liftChoice

        let queryTimebaseParameters picoScope timebase segment =
            sprintf "Querying parameters parameters for %A and %A." timebase segment |> logDeviceOp picoScope
            asyncChoice {
                let! resolution = queryResolution picoScope
                let! (interval, maxSamples) = queryIntervalAndMaxSamples picoScope timebase segment
                return
                    { Timebase       = timebase
                      Resolution     = resolution 
                      MaximumSamples = maxSamples
                      SampleInterval = interval } }
            |> (Async.map <|
                logDeviceQueryResult picoScope
                    (sprintf "Successfully obtained timebase parameters: %A.")
                    (sprintf "Failed to obtain timebase parameters: %s."))

        let segmentMemory picoScope (MemorySegment numberOfSegments) =
            sprintf "Segmenting device memory into %d segments." numberOfSegments |> logDeviceOp picoScope
            let mutable samplesPerSegment = 0
            NativeApi.MemorySegments(handle picoScope, numberOfSegments, &samplesPerSegment)
            |> checkStatusAndReturn (SampleCount samplesPerSegment)
            |> logDeviceQueryResult picoScope
                (sprintf "Successfully segmented device memory: maximum number of samples per segment: %A.")
                (sprintf "Failed to segment device memory: %s.")
            |> AsyncChoice.liftChoice

    module ChannelSettings =
        let private availableChannelsForResolution =
            function
            | Resolution_8bit
            | Resolution_12bit 
            | Resolution_14bit -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
            | Resolution_15bit -> [ ChannelA ; ChannelB ] |> Set.ofList
            | Resolution_16bit -> [ ChannelA ] |> Set.ofList

        let queryAvailableChannels picoScope =
            asyncChoice {
                let! modelNumber = queryDeviceInfo picoScope ModelNumber
                let! resolution = Sampling.queryResolution picoScope
                let availableChannels = availableChannelsForResolution resolution
                match int <| modelNumber.[1].ToString() with
                | 2 -> return! succeed <| (Set.intersect availableChannels (Set.ofList [ ChannelA ; ChannelB ]))
                | 4 -> return! succeed <| (Set.intersect availableChannels (Set.ofList [ ChannelA ; ChannelB ; ChannelC ; ChannelD ]))
                | _ -> return! fail    <| sprintf "Unexpected model number: %s." modelNumber }

        let queryAvailableAnalogueOffsetRange picoScope range coupling =
            sprintf "Querying available analogue offset range for input range %A with %A coupling." range coupling |> logDeviceOp picoScope
            let mutable maxOffset = 0.0f
            let mutable minOffset = 0.0f
            let volts = LanguagePrimitives.Float32WithMeasure<V>
            NativeApi.GetAnalogueOffset(handle picoScope, rangeEnum range, couplingEnum coupling, &maxOffset, &minOffset)
            |> checkStatusAndReturn (VoltageInVolts (volts maxOffset), VoltageInVolts (volts minOffset))
            |> logDeviceQueryResult picoScope
                (fun ((VoltageInVolts max), (VoltageInVolts min)) -> sprintf "Channel offset range: %f V to %f V." (float min) (float max))
                (sprintf "Failed to query channel offset range: %s.")
            |> AsyncChoice.liftChoice

        let queryAvaiableChannelRanges picoScope inputChannel =
            sprintf "Querying available input ranges for channel %A." inputChannel |> logDeviceOp picoScope
            let mutable rangesLength = 12
            let ranges = Array.zeroCreate rangesLength
            NativeApi.GetChannelInformation(handle picoScope, ChannelInfoEnum.VoltageOffsetRanges, 0, ranges, &rangesLength, inputChannelEnum inputChannel)
            |> checkStatusAndReturn (ranges |> Array.toSeq |> Seq.map parseRange |> Seq.take rangesLength |> Set.ofSeq)
            |> logDeviceQueryResult picoScope 
                (Set.count >> sprintf "Obtained %d input ranges.")
                (sprintf "Failed to query available input ranges: %s.")
            |> AsyncChoice.liftChoice

        let private setBandwidthFilter picoScope inputChannel bandwidthLimit =
            sprintf "Setting bandwidth %A to channel %A." inputChannel bandwidthLimit |> logDeviceOp picoScope
            NativeApi.SetBandwidthFilter(handle picoScope, inputChannelEnum inputChannel, bandwidthLimitEnum bandwidthLimit)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set bandwidth filter.")
                (sprintf "Failed to set bandwidth filter: %s.")
            |> AsyncChoice.liftChoice
                    
        let private setChannelInputSettings picoScope inputChannel inputSettings =
            sprintf "Enabling channel %A and setting input settings : %A." inputChannel inputSettings |> logDeviceOp picoScope
            let coupling  = couplingEnum inputSettings.Coupling
            let range     = rangeEnum inputSettings.Range
            let offset    = voltageFloatInVolts inputSettings.AnalogueOffset
            NativeApi.SetChannel(handle picoScope, inputChannelEnum inputChannel, 1s, coupling, range, offset) 
            |> checkStatus 
            |> logDeviceOpResult picoScope
                ("Successfully set input settings.")
                (sprintf "Failed to set input settings: %s.")
            |> AsyncChoice.liftChoice

        let private setChannelEnabled picoScope inputChannel inputSettings =
            asyncChoice {
                do! setChannelInputSettings picoScope inputChannel inputSettings
                do! setBandwidthFilter picoScope inputChannel inputSettings.BandwidthLimit }
        
        let private setChannelDisabled picoScope inputChannel =
            sprintf "Disabling channel %A." inputChannel |> logDeviceOp picoScope
            NativeApi.SetChannel(handle picoScope, inputChannelEnum inputChannel, 0s, CouplingEnum.DC, RangeEnum._10V, 0.0f) 
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully disabled channel.")
                (sprintf "Failed to disable channel: %s.")
            |> AsyncChoice.liftChoice

        let private setChannelSettings picoScope inputChannel channelSettings =
            sprintf "Setting channel %A settings: %A." inputChannel channelSettings |> logDeviceOp picoScope
            match channelSettings with
            | EnabledChannel inputSettings -> setChannelEnabled  picoScope inputChannel inputSettings
            | DisabledChannel              -> setChannelDisabled picoScope inputChannel
            |> (Async.map <| 
                logDeviceOpResult picoScope
                    ("Successfully set channel settings.")
                    (sprintf "Failed to set channel settings: %s."))

        let setAcquisitionInputChannels picoScope acquisition =
            sprintf "Setting input channel settings for acquisition: %A." acquisition.InputSettings |> logDeviceOp picoScope
            asyncChoice {
                let requiredChannels   = Map.keys acquisition.InputSettings
                let! availableChannels = queryAvailableChannels picoScope
                if not (Set.isSubset requiredChannels availableChannels) then
                    return! fail "The specified acquisition inputs require input channels which are not available on the current device." 
                for channel in availableChannels do
                    do! Acquisition.settingsForChannel channel acquisition
                        |> setChannelSettings picoScope channel }
            |> (Async.map <| 
                logDeviceOpResult picoScope
                    ("Successfully set input channel settings for acquisition.")
                    (sprintf "Failed to set input channel settings for acquisition: %s"))


    module Triggering = 
        let private setAutoTrigger picoScope (AutoTriggerDelayInMilliseconds delay) =
            sprintf "Setting auto-trigger with delay: %d ms." (int16 delay) |> logDeviceOp picoScope
            NativeApi.SetSimpleTrigger(handle picoScope, 0s, ChannelEnum.A, 0s, ThresholdDirectionEnum.None, 0u, int16 delay)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set auto-trigger.")
                (sprintf "Failed to set auto-trigger delay: %s.")
            |> AsyncChoice.liftChoice

        let private setSimpleTrigger picoScope simpleTriggerSettings =
            sprintf "Setting simple trigger settings: %A." simpleTriggerSettings |> logDeviceOp picoScope
            let channel = triggerChannelEnum simpleTriggerSettings.TriggerChannel
            let threshold                  = simpleTriggerSettings.AdcThreshold
            let thresholdDirection = levelThresholdEnum simpleTriggerSettings.ThresholdDirection
            let (SampleIndex startSample)  = simpleTriggerSettings.StartSample
            let delay = autoTriggerDelayIntInMilliseconds simpleTriggerSettings.AutoTrigger
            NativeApi.SetSimpleTrigger(handle picoScope, 1s, channel, threshold, thresholdDirection, startSample, delay)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set simple trigger settings.")
                (sprintf "Failed to set simple trigger settings: %s.")
            |> AsyncChoice.liftChoice

        let setTriggerSettings picoScope triggerSettings =
            match triggerSettings with
            | SimpleTrigger simpleTriggerSettings -> setSimpleTrigger picoScope simpleTriggerSettings
            | AutoTrigger delay                   -> setAutoTrigger   picoScope delay

        let queryTriggerStatus picoScope =
            "Querying trigger status." |> logDeviceOp picoScope
            let mutable triggerEnabled = 0s
            let mutable pwqEnabled = 0s
            NativeApi.IsTriggerOrPulseWidthQualifierEnabled(handle picoScope, &triggerEnabled, &pwqEnabled)
            |> checkStatusAndReturn { TriggerState = parseToggleState triggerEnabled ; PulseWidthQualifierState = parseToggleState pwqEnabled }
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained trigger status: %A.")
                (sprintf "Failed to query trigger status: %s.")
            |> AsyncChoice.liftChoice

    module internal DataBuffers =
        let setDataBuffer picoScope inputChannel downsamplingMode (MemorySegment index) acquisitionBuffer =
            sprintf "Setting data buffer for channel %A with downsampling mode %A on memory segment %d." inputChannel downsamplingMode index |> logDeviceOp picoScope
            match acquisitionBuffer with
            | SingleBuffer buffer ->
                NativeApi.SetDataBuffer(handle picoScope, inputChannelEnum inputChannel, buffer, buffer.Length, index, downsamplingModeEnum downsamplingMode)
                |> checkStatus
            | BufferPair (bufferMax, bufferMin) ->
                NativeApi.SetDataBuffers(handle picoScope, inputChannelEnum inputChannel, bufferMax, bufferMin, bufferMax.Length, index,    
                    downsamplingModeEnum downsamplingMode)
                |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully set data buffer.")
                (sprintf "Failed to set data buffer: %s.")
            |> AsyncChoice.liftChoice

    module internal Acquisition =
        let stop picoScope =
            "Stopping acquisition." |> logDeviceOp picoScope 
            NativeApi.Stop (handle picoScope)
            |> checkStatus
            |> logDeviceOpResult picoScope
                ("Successfully stopped acquisition")
                (sprintf "Failed to stop acquisition: %s.")
            |> AsyncChoice.liftChoice

        let queryNumberOfCaptures picoScope =
            "Querying number of captures." |> logDeviceOp picoScope
            let mutable index = 0u
            NativeApi.GetNumberOfCaptures(handle picoScope, &index)
            |> checkStatusAndReturn (MemorySegment index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of captures: %A.")
                (sprintf "Failed to query number of captures: %s.")
            |> AsyncChoice.liftChoice

        let queryNumberOfProcessedCaptures picoScope =
            "Querying number of processed captures" |> logDeviceOp picoScope
            let mutable index = 0u
            NativeApi.GetNumberOfProcessedCaptures(handle picoScope, &index)
            |> checkStatusAndReturn (MemorySegment index)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of processed captures: %A.")
                (sprintf "Failed to query number of processed captures: %s.")
            |> AsyncChoice.liftChoice

        let private voltageOverflowChannels overflowBits =
            // determine whether any of the corresponding voltage overflow flags is set using the appropriate 
            // bit shifts and integer bitwise AND
            [ ChannelEnum.A ; ChannelEnum.B ; ChannelEnum.C ; ChannelEnum.D ]
            |> List.filter (fun channel -> ((1 <<< int channel) &&& (int overflowBits)) <> 0) 
            |> List.map parseInputChannel
            |> Set.ofList

        let pollStreamingLatestValues picoScope callback =
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
            |> AsyncChoice.liftChoice

        let queryAvailableStreamingValues picoScope =
            "Querying number of available streaming values after acquisition." |> logDeviceOp picoScope
            let mutable sampleIndex = 0u
            NativeApi.NumberOfStreamingValues(handle picoScope, &sampleIndex)
            |> checkStatusAndReturn (SampleIndex sampleIndex)
            |> logDeviceQueryResult picoScope
                (sprintf "Obtained number of available streaming samples: %A.")
                (sprintf "Failed to query number of available streaming samples: %s.")
            |> AsyncChoice.liftChoice

        let startStreaming picoScope streamingParameters =
            asyncChoice { 
                sprintf "Starting streaming acquisition: %A." streamingParameters |> logDeviceOp picoScope
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
                return! NativeApi.RunStreaming(handle picoScope, &hardwareInterval, timeUnit, preTriggerSamples, postTriggerSamples, autoStop, 
                                               downsamplingRatio, downsamplingMode, bufferLength)
                |> checkStatusAndReturn (parseTimeUnitWithInterval (timeUnit, int hardwareInterval))
                |> logDeviceQueryResult picoScope
                    (sprintf "Successfully started streaming acquisition with sample interval: %A.")
                    (sprintf "Failed to start streaming acquisition: %s.") }