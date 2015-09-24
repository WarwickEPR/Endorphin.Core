namespace Endorphin.Instrument.PicoScope3000

open System
open System.Text
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Parsing
open StatusCodes
open NativeModel
open Endorphin.Core

[<RequireQualifiedAccess>]
/// Functions for performing commands and sending requests to a PicoScope 3000 series device.
module PicoScope =
    /// Returns the device serial number.
    let private serialNumber device = device.SerialNumber
    
    /// Returns the device handle used to communicate with the native API.
    let private handle device = device.Handle 
    
    [<AutoOpen>]
    /// Utility functions related to logging.
    module private Logging =
        let private log = log4net.LogManager.GetLogger typeof<PicoScope3000>
        
        /// Checks whether the given status indicates that a command completed successfully
        /// and returns an exception if not.
        let checkStatus = function
            | Ok               -> Choice.succeed ()
            | HasError message -> Choice.fail (Exception message)

        /// Checks whether the give status indicates that q query completed successfully and
        /// returns the given value if so, or an exception otherwise.
        let checkStatusAndReturn value status = choice {
            do! checkStatus status
            return value }

        /// Checks a status code returned by the PicoScope API during initialisation and returns
        /// the device power source if so or an exception otherwise.
        let checkInitialisationStatus = function
            | Ok                            -> Choice.succeed MainsPower
            | PowerSourceStatus powerSource -> Choice.succeed powerSource
            | HasError message              -> Choice.fail (Exception message)
        
        /// Logs a string describing an operation which is about to be performed.
        let logOp = log.Info

        /// Logs a query result which can either indicate success or failure, and in each case
        /// applies the given function to create a log message for the result.
        let logQueryResult successMessageFunc failureMessageFunc input =
            match input with
            | Success result      -> successMessageFunc result      |> log.Debug
            | Failure (exn : exn) -> failureMessageFunc exn.Message |> log.Error
            input

    /// Asynchronously opens a connection to a PicoScope 3000 series with a given serial number.
    /// If the given serial number is null then the first available PicoScope 3000 series will
    /// be connected.
    let openDevice serial =
        // use a CommandRequestAgent for the underlying implementation which will serialise
        // commands to the hardware
        CommandRequestAgent.create (serialNumber >> sprintf "PicoScope %s") (fun () -> async {
            if serial <> null 
            then sprintf "Opening device %s." serial |> logOp
            else         "Opening first available device" |> logOp
            
            // open a connection and get the device handle and power source
            let mutable handle = 0s 
            let powerSourceResult = 
                NativeApi.OpenUnit (&handle, serial)
                |> checkInitialisationStatus

            match powerSourceResult with
            | Success powerSource -> 
                // get the serial number to be stored with the device identity information and used
                // to log communications with the hardware
                let resultLength = 32s
                let result = new StringBuilder(int resultLength)
                let mutable requiredLength = 0s
                let serialResult =
                    NativeApi.GetUnitInfo(handle, result, resultLength, &requiredLength, DeviceInfoEnum.SerialNumber)
                    |> checkStatusAndReturn (result.ToString())
                    |> logQueryResult
                        (sprintf "Successfully opened device with power source %A: %s." powerSource)
                        (sprintf "Failed to open device: %s.")

                match serialResult with
                | Success serial -> return Choice.succeed { SerialNumber = serial ; Handle = handle }
                | Failure exn    -> return Choice.fail exn
            | Failure exn -> return Choice.fail exn })
        |> Async.map PicoScope3000 // wrap the agent as a PicoScope3000
        
    /// Asynchronously opens a connection to the first available PicoScope 3000 series device.
    let openFirst () = openDevice null

    /// Asynchronously pings a PicoScope 3000 series device.
    let pingDevice (PicoScope3000 picoScope) =
        picoScope |> CommandRequestAgent.performCommand "Ping"
            (fun device -> NativeApi.PingUnit (handle device) |> checkStatus)

    /// Asynchronously closes the connection to a PicoScope 3000 series device.
    let close (PicoScope3000 picoScope) =
        picoScope |> CommandRequestAgent.close
            (fun device -> NativeApi.CloseUnit (handle device) |> checkStatus)

    /// Enumerates the list of connected PicoScope 3000 series devices.
    let enumerateDevices () = async {
        sprintf "Enumerating connected devices." |> logOp
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = NativeApi.EnumerateUnits (&count, serials, &stringLength)
        
        let result =
            match status with
            | Found false -> Choice.succeed <| Seq.empty
            | Found true  -> Choice.succeed <| (Seq.ofArray <| serials.ToString().Split [| ',' |])
            | error       -> Choice.fail    <| Exception (statusMessage error)
            |> logQueryResult
                (Seq.length >> sprintf "Found %d connected devices.")
                (sprintf "Failed to enumerate devices: %s.") 
            
        match result with
        | Success s   -> return s
        | Failure exn -> raise exn ; return Unchecked.defaultof<string seq> }
    
    /// Asynchronously sets the front panel LED flash of a PicoScope 3000 series device.
    let setLedFlash (PicoScope3000 picoScope) ledFlash =
        picoScope |> CommandRequestAgent.performCommand (sprintf "Set LED flash: %A" ledFlash)
            (fun device -> NativeApi.FlashLed (handle device, ledFlashCounts ledFlash) |> checkStatus)

    /// Asynchronously set the power source of a PicoScope 3000 series device.
    let setPowerSource (PicoScope3000 picoScope) powerSource =
        picoScope |> CommandRequestAgent.performCommand (sprintf "Set power source: %A" powerSource)
            (fun device -> NativeApi.ChangePowerSource (handle device, powerSourceStatusCode powerSource) |> checkStatus)

    /// Asynchronously queries the current power source of a PicoScope 3000 series device.
    let queryPowerSource (PicoScope3000 picoScope) =
        picoScope |> CommandRequestAgent.performObjectRequest "Query power source"
            (fun device ->
                let response = NativeApi.CurrentPowerSource (handle device)
                match response with
                | PowerSourceStatus powerSource -> Choice.succeed <| powerSource
                | error                         -> Choice.fail    <| Exception (statusMessage error))
                           
    /// Functions related to querying device information.
    module Info =
        
        /// Asynchronously queries the specified device info type for a PicoScope 3000 series
        /// device.
        let private queryDeviceInfo (PicoScope3000 picoScope) deviceInfoEnum =
            picoScope |> CommandRequestAgent.performObjectRequest (sprintf "Query device information: %A" deviceInfoEnum)
                (fun device ->
                    let resultLength = 32s
                    let result = new StringBuilder(int resultLength)
                    let mutable requiredLength = 0s
                    NativeApi.GetUnitInfo(handle device, result, resultLength, &requiredLength, deviceInfoEnum)
                    |> checkStatusAndReturn (result.ToString()))

        /// Asynchronously queries the PicoScope 3000 series driver version.
        let queryDriverVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.DriverVersion

        /// Asynchronously queries the PicoScope 3000 series USB version.
        let queryUsbVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.UsbVersion
        
        /// Asynchronously queries the PicoScope 3000 series hardware version.
        let queryHardwareVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.HardwareVersion
        
        /// Asynchronously queries the PicoScope 3000 series model number.
        let queryModelNumber picoScope = queryDeviceInfo picoScope DeviceInfoEnum.ModelNumber

        /// Asynchronously queries the PicoScope 3000 series serial number.
        let querySerialNumber picoScope = queryDeviceInfo picoScope DeviceInfoEnum.SerialNumber

        /// Asynchronously queries the PicoScope 3000 series calibration date.
        let queryCalibrationDate picoScope = queryDeviceInfo picoScope DeviceInfoEnum.CalibrationDate

        /// Asynchronously queries the PicoScope 3000 series kernel version.
        let queryKernelVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.KernelVersion

        /// Asynchronously queries the PicoScope 3000 series digital hardware version.
        let queryDigitalHardwareVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.DigitalHardwareVersion

        /// Asynchronously queries the PicoScope 3000 series analogue hardware version.
        let queryAnalogueHardwareVersion picoScope = queryDeviceInfo picoScope DeviceInfoEnum.AnalogueHardwareVersion

    /// Functions related to signal sampling.
    module Sampling =
        /// Asynchronously queries the current minimum ADC count value of a PicoScope 3000 series
        /// device. The minimum ADC count depends on the current vertical resolution and corresponds
        /// to a signal at the lower limit of an input channel's voltage range.
        let queryMinimumAdcCount (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performValueRequest "Query minimum ADC count value"
                (fun device ->
                    let mutable adcCount : AdcCount = 0s
                    NativeApi.MinimumValue(handle device, &adcCount)
                    |> checkStatusAndReturn adcCount)
        
        /// Asynchronously queries the current maximum ADC count value of a PicoScope 3000 series
        /// device. The maximum ADC count depends on the current vertical resolution and corresponds
        /// to a signal at the upper limit of an input channel's voltage range.
        let queryMaximumAdcCount (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performValueRequest "Query maximum ADC count value."
                (fun device ->
                    let mutable adcCount : AdcCount = 0s
                    NativeApi.MaximumValue(handle device, &adcCount)
                    |> checkStatusAndReturn adcCount)
        
        /// Asynchronously queries the maximum downsampling ratio which can be used on a PicoScope
        /// 3000 series device for the specified number of samples, memory segment and downsampling
        /// mode.
        let queryMaximumDownsamplingRatio (PicoScope3000 picoScope) (unaggregatedSamples : SampleIndex) downsamplingMode (segment : MemorySegment) =
            let description = sprintf "Query maximum downsampling ratio for %A mode, %d samples and %A"
                                downsamplingMode unaggregatedSamples segment

            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable downsamplingRatio : DownsamplingRatio = 0u
                    NativeApi.GetMaximumDownsamplingRatio(handle device, unaggregatedSamples, &downsamplingRatio, downsamplingModeEnum downsamplingMode, segment)
                    |> checkStatusAndReturn downsamplingRatio)
        
        /// Asynchronously queries the maximum number of memory segments into which the memory can
        /// be segmented on a PicoScope 3000 series device. Memory can be segmented in order to
        /// store multiple acquisition blocks on the device memory before transferring them to the
        /// computer.
        let queryMaximumMemorySegments (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performObjectRequest "Query maximum number of memory segments"
                (fun device ->
                    let mutable index : MemorySegment = 0u 
                    NativeApi.GetMaximumNumberOfSegments(handle device, &index)
                    |> checkStatusAndReturn index)

        /// Asynchronously queries the sample interval and maximum sample count for the given timebase
        /// on a PicoScope 3000 series device. The timebase depends on the current device resolution.
        let private queryIntervalAndMaxSamples (PicoScope3000 picoScope) (timebase : Timebase) (index : MemorySegment) = 
            let description = sprintf "Query sample interval and maximum sample count for timebase %d and segment index %d"
                                timebase index
            
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable interval = 0
                    let mutable maxSamples : SampleCount = 0
                    let nanosec = LanguagePrimitives.Int32WithMeasure<ns>
                    NativeApi.GetTimebase (handle device, timebase, 0, &interval, 0s, &maxSamples, index)
                    |> checkStatusAndReturn (Interval_ns (nanosec interval), maxSamples))

        /// Asynchronously queries the parameters for the specified timebase on a PicoScope 3000 series
        /// device with the current vertical resolution.
        let queryTimebaseParameters picoScope timebase segment =
            async {
                let! (interval, maxSamples) = queryIntervalAndMaxSamples picoScope timebase segment
                return
                    { Timebase       = timebase
                      MaximumSamples = maxSamples
                      SampleInterval = interval } }

        /// Asynchronously segments the memory of a PicoScope 3000 series device into the specified
        /// number of segments.
        let segmentMemory (PicoScope3000 picoScope) (numberOfSegments : MemorySegment) =
            picoScope |> CommandRequestAgent.performObjectRequest (sprintf "Segmenting device memory into %d segments." numberOfSegments)
                (fun device ->
                    let mutable samplesPerSegment : SampleCount = 0
                    NativeApi.MemorySegments(handle device, numberOfSegments, &samplesPerSegment)
                    |> checkStatusAndReturn samplesPerSegment)

    /// Functions related to input channel settings.
    module ChannelSettings =

        /// Asynchronously queries the set of available input channels on a PicoScope 3000 series device.
        let queryAvailableChannels picoScope =
            async {
                let! modelNumber = Info.queryModelNumber picoScope
                let! powerSource = queryPowerSource picoScope
                return Set.intersectMany <|
                        [ Device.availableChannelsForModel modelNumber
                          PowerSource.availableChannels powerSource ] }

        /// Asyhnchronously queries the available range of analogue voltage offsets for an input channel
        /// for a PicoScope 3000 series device using the specified input voltage range and coupling.
        let queryAvailableAnalogueOffsetRange (PicoScope3000 picoScope) range coupling =
            let description = sprintf "Query available analogue offset range for input range %A with %A coupling" range coupling
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable maxOffset = 0.0f
                    let mutable minOffset = 0.0f
                    let volts = LanguagePrimitives.Float32WithMeasure<V>
                    NativeApi.GetAnalogueOffset(handle device, rangeEnum range, couplingEnum coupling, &maxOffset, &minOffset)
                    |> checkStatusAndReturn (Voltage_V (volts maxOffset), Voltage_V (volts minOffset)))

        /// Asynchronously queries the set of avaiable channels available input channel voltage ranges
        /// for the specified input channel on a PicoScope 3000 series device.
        let queryAvailableChannelRanges (PicoScope3000 picoScope) inputChannel =
            let description = sprintf "Query available input ranges for channel %A" inputChannel
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable rangesLength = 12
                    let ranges = Array.zeroCreate rangesLength
                    NativeApi.GetChannelInformation(handle device, ChannelInfoEnum.ConfidenceIntervalRanges, 0, ranges, &rangesLength, inputChannelEnum inputChannel)
                    |> checkStatusAndReturn (ranges |> Array.toSeq |> Seq.map parseRange |> Seq.take rangesLength |> Set.ofSeq))

        /// Asynchronously sets the bandwidth filter for the specified input channel on a PicoScope 3000
        /// series device.
        let private setBandwidthFilter (PicoScope3000 picoScope) inputChannel bandwidthLimit =
            let description = sprintf "Set bandwidth %A to channel %A" inputChannel bandwidthLimit
            picoScope |> CommandRequestAgent.performCommand description 
                (fun device ->
                    NativeApi.SetBandwidthFilter(handle device, inputChannelEnum inputChannel, bandwidthLimitEnum bandwidthLimit)
                    |> checkStatus)
         
        /// Asynchronously sets the input settings for the specified input channel on a PicoScope 3000
        /// series device.
        let private setChannelInputSettings (PicoScope3000 picoScope) inputChannel inputSettings =
            let description = sprintf "Enable channel %A and setting input settings: %A" inputChannel inputSettings
            picoScope |> CommandRequestAgent.performCommand description
                (fun device ->
                    let coupling  = couplingEnum inputSettings.Coupling
                    let range     = rangeEnum inputSettings.Range
                    let offset    = voltageFloat_V inputSettings.AnalogueOffset
                    NativeApi.SetChannel(handle device, inputChannelEnum inputChannel, 1s, coupling, range, offset) 
                    |> checkStatus)

        /// Asynchronously enables the specified input channel with the given input settings on a PicoScope
        /// 3000 series device.
        let private setChannelEnabled picoScope inputChannel inputSettings =
            async {
                do! setChannelInputSettings picoScope inputChannel inputSettings
                do! setBandwidthFilter picoScope inputChannel inputSettings.BandwidthLimit }
        
        /// Asynchronously disables the specified input channel on a PicoScope 3000 series device.
        let private setChannelDisabled (PicoScope3000 picoScope) inputChannel =
            picoScope |> CommandRequestAgent.performCommand (sprintf "Disable channel %A" inputChannel)
                (fun device ->
                    NativeApi.SetChannel(handle device, inputChannelEnum inputChannel, 0s, CouplingEnum.DC, RangeEnum._10V, 0.0f) 
                    |> checkStatus)

        /// Asynchronously sets the settings for the specified input channel on a PicoScope 3000 series
        /// device.
        let private setChannelSettings picoScope inputChannel channelSettings =
            match channelSettings with
            | EnabledChannel inputSettings -> setChannelEnabled  picoScope inputChannel inputSettings
            | DisabledChannel              -> setChannelDisabled picoScope inputChannel

        /// Asynchronously sets up all input channels on a PicoScope 3000 series device with the given
        /// acquisition input settings.
        let setAcquisitionInputChannels picoScope acquisitionInputs =
            async {
                let requiredChannels   = Map.keys acquisitionInputs.InputSettings
                let! availableChannels = queryAvailableChannels picoScope
                if not (Set.isSubset requiredChannels availableChannels) then
                    failwith "The specified acquisition inputs require input channels which are not available on the current device." 
                for channel in availableChannels do
                    do! Inputs.settingsForChannel channel acquisitionInputs
                        |> setChannelSettings picoScope channel }

    /// Functions related to acquisition triggering.
    module Triggering = 

        /// Asynchronously sets a PicoScope 3000 series device to trigger automatically after the specified
        /// delay.
        let private setAutoTrigger (PicoScope3000 picoScope) (AutoTriggerDelay_ms delay) =
            let description = sprintf "Set auto-trigger with delay: %d ms" (int16 delay)
            picoScope |> CommandRequestAgent.performCommand description (fun device -> 
                NativeApi.SetSimpleTrigger(handle device, 0s, ChannelEnum.A, 0s, ThresholdDirectionEnum.None, 0u, int16 delay)
                |> checkStatus)

        /// Asynchronously sets up a simple trigger on a PicoScope 3000 series device which triggers
        /// acquisition at a voltage threshold crossing on an trigger channel or, optionally, also
        /// automatically after a delay.
        let private setSimpleTrigger (PicoScope3000 picoScope) simpleTriggerSettings =
            let description = sprintf "Set simple trigger settings: %A" simpleTriggerSettings
            picoScope |> CommandRequestAgent.performCommand description
                (fun device ->
                    let channel = triggerChannelEnum simpleTriggerSettings.TriggerChannel
                    let threshold                 = simpleTriggerSettings.AdcThreshold
                    let thresholdDirection        = levelThresholdEnum simpleTriggerSettings.ThresholdDirection
                    let startSample : SampleIndex = simpleTriggerSettings.StartSample
                    let delay = autoTriggerDelayInt_ms simpleTriggerSettings.AutoTrigger
                    NativeApi.SetSimpleTrigger(handle device, 1s, channel, threshold, thresholdDirection, startSample, delay)
                    |> checkStatus)

        /// Asynchronously sets the trigger settings for a PicoScope 3000 series device.
        let setTriggerSettings picoScope triggerSettings =
            match triggerSettings with
            | SimpleTrigger simpleTriggerSettings -> setSimpleTrigger picoScope simpleTriggerSettings
            | AutoTrigger delay                   -> setAutoTrigger   picoScope delay

        /// Asynchronously queries the trigger and pulse width qualifier mode status for a PicoScope 3000
        /// series device.
        let queryTriggerStatus (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performObjectRequest "Query trigger status"
                (fun device ->
                    let mutable triggerEnabled = 0s
                    let mutable pwqEnabled = 0s
                    NativeApi.IsTriggerOrPulseWidthQualifierEnabled(handle device, &triggerEnabled, &pwqEnabled)
                    |> checkStatusAndReturn
                        { TriggerState = parseToggleState triggerEnabled ; PulseWidthQualifierState = parseToggleState pwqEnabled })

    /// Functions related to setting up data buffers for an acquisition.
    module internal DataBuffers =
        
        /// Asynchronously sets a data buffer to a PicoScope 3000 series device which will be used to
        /// transfer samples from the device memory to the computer.
        let setDataBuffer (PicoScope3000 picoScope) inputChannel downsamplingMode (index : MemorySegment) acquisitionBuffer =
            let description = sprintf "Set data buffer for channel %A with downsampling mode %A on memory segment %d"
                                inputChannel downsamplingMode index
            
            picoScope |> CommandRequestAgent.performCommand description
                (fun device ->
                    match acquisitionBuffer with
                    | SingleBuffer buffer ->
                        NativeApi.SetDataBuffer(handle device, inputChannelEnum inputChannel, buffer, buffer.Length, index, downsamplingModeEnum downsamplingMode)
                        |> checkStatus
                    | BufferPair (bufferMax, bufferMin) ->
                        NativeApi.SetDataBuffers(handle device, inputChannelEnum inputChannel, bufferMax, bufferMin, bufferMax.Length, index,    
                            downsamplingModeEnum downsamplingMode)
                        |> checkStatus)

    /// Functions related to acquisition.
    module internal Acquisition =
        
        /// Asynchronously stops a PicoScope 3000 series acquisition currently in progress.
        let stop (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performCommand "Stop acquisition"
                (fun device -> NativeApi.Stop (handle device) |> checkStatus)

        /// Asynchronously queries the number of captures stored in the PicoScope 3000 series device memory
        /// after a rapid block acquisition has been stopped.
        let queryNumberOfCaptures (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performObjectRequest "Query number of captures"
                (fun device ->
                    let mutable index : MemorySegment = 0u
                    NativeApi.GetNumberOfCaptures(handle device, &index)
                    |> checkStatusAndReturn index)

        /// Asynchronously queries the number of process captures in the PicoScope 3000 series device memory
        /// after a rapid block acquisition has been stopped.
        let queryNumberOfProcessedCaptures (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performObjectRequest "Query number of processed captures"
                (fun device ->
                    let mutable index : MemorySegment = 0u
                    NativeApi.GetNumberOfProcessedCaptures(handle device, &index)
                    |> checkStatusAndReturn index)
    
        /// Returns the set of input channels which have their voltage overflow flags set in the given
        /// voltage overflow indicator.
        let private voltageOverflowChannels overflowBits =
            // bit shift 1 by the required number of bits for each channel and apply a bitwise AND
            // operation with the given value
            [ ChannelEnum.A ; ChannelEnum.B ; ChannelEnum.C ; ChannelEnum.D ]
            |> List.filter (fun channel -> ((1 <<< int channel) &&& (int overflowBits)) <> 0) 
            |> List.map parseInputChannel
            |> Set.ofList

        /// Asynchronously polls a PicoScope 3000 series device for the latest streaming values during a
        /// streaming acquisition. If values are available, the provided callback function will be called,
        /// indicating the position in the buffer where the values have been written.
        let pollStreamingLatestValues (PicoScope3000 picoScope) callback =
            let picoScopeCallback = // define the callback as required by the PicoScope API
                PicoScopeStreamingReady(fun _ numberOfSamples startIndex overflowBits triggeredAt triggered didAutoStop _ ->
                    // wrap the values in a StreamingValuesReady record and send them to the user callback
                    { NumberOfSamples = numberOfSamples
                      StartIndex = startIndex
                      VoltageOverflows = voltageOverflowChannels overflowBits
                      TriggerPosition = parseTriggerPosition (triggered <> 0s) (triggeredAt)
                      DidAutoStop = didAutoStop <> 0s } |> callback)
            
            picoScope |> CommandRequestAgent.performObjectRequest "Poll for latest streaming values"
                (fun device ->
                    let response = NativeApi.GetStreamingLatestValues(handle device, picoScopeCallback, System.IntPtr.Zero)
                    match response with
                    | AvailabilityStatus status -> Choice.succeed <| status
                    | error                     -> Choice.fail    <| Exception (statusMessage error))
        
        /// Asynchronously queries the number of samples stored in the PicoScope 3000 series device memory
        /// after a streaming acquisition has been stopped.
        let queryAvailableStreamingValues (PicoScope3000 picoScope) =
            let description = "Query number of available streaming values after acquisition"
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable sampleIndex : SampleIndex = 0u
                    NativeApi.NumberOfStreamingValues(handle device, &sampleIndex)
                    |> checkStatusAndReturn sampleIndex)

        /// Asynchronously initiates a streaming acquisition on a PicoScope 3000 series device with the given
        /// streaming parameters. Note that input channel settings, trigger settings and acquisition buffers
        /// must be set up before this point. The device must then be polled for the latest streaming values
        /// during the acquisition.
        let startStreaming (PicoScope3000 picoScope) streamingParameters =
            let description = sprintf "Start streaming acquisition: %A" streamingParameters
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let (requestedInterval, timeUnit)                     = intervalAndTimeUnitEnum streamingParameters.SampleInterval
                    let (autoStop, preTriggerSamples, postTriggerSamples) = streamStopParameters streamingParameters.StreamStop
                    let (bufferLength)                                    = streamingParameters.BufferLength
                
                    let downsamplingMode =
                        streamingParameters.Inputs.InputSampling
                        |> Set.map (fun sampling -> sampling.DownsamplingMode)
                        |> downsamplingModeEnumForSet

                    let downsamplingRatio = 
                        match streamingParameters.DownsamplingRatio with
                        | Some downsamplingRatio -> downsamplingRatio
                        | None                   -> 1u

                    let mutable hardwareInterval = uint32 requestedInterval
                    NativeApi.RunStreaming(handle device, &hardwareInterval, timeUnit, preTriggerSamples, postTriggerSamples,
                                            autoStop, downsamplingRatio, downsamplingMode, bufferLength)
                    |> checkStatusAndReturn (parseIntervalWithInterval (int hardwareInterval, timeUnit)))