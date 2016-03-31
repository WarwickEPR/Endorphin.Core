// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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
    
    let private orDefault fallback value = defaultArg value fallback

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

        /// Checks whether the give status indicates that q query completed successfully and
        /// sends on to a callback if so, or an exception otherwise.
        let checkStatusAndForward callback value status = choice {
            do! checkStatus status
            callback value }

        /// Checks a status code returned by the PicoScope API during initialisation and returns
        /// the device power source if so or an exception otherwise.
        let checkInitialisationStatus = function
            | Ok                            -> Choice.succeed MainsPower
            | PowerSourceStatus powerSource -> Choice.succeed powerSource
            | HasError message              -> Choice.fail (Exception message)
        
        /// Logs a string describing an operation which is about to be performed.
        let logOp = log.Info

        let logWarning = log.Warn

        let logDebug = log.Debug

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
            match serial with
            | null -> "Opening first available device" |> logOp
            | _    -> sprintf "Opening device %s." serial |> logOp
            
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
            (handle >> NativeApi.PingUnit >> checkStatus)

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
        /// -- Documentation says ADC counts are scaled to 16 bits
        let queryMinimumAdcCount (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performValueRequest "Query minimum ADC count value"
                (fun device ->
                    let mutable adcCount : AdcCount = 0s
                    NativeApi.MinimumValue(handle device, &adcCount)
                    |> checkStatusAndReturn adcCount)
        
        /// Asynchronously queries the current maximum ADC count value of a PicoScope 3000 series
        /// device. The maximum ADC count depends on the current vertical resolution and corresponds
        /// to a signal at the upper limit of an input channel's voltage range.
        /// -- Documentation says ADC counts are scaled to 16 bits
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
                    let status = NativeApi.GetTimebase (handle device, timebase, 0, &interval, 0s, &maxSamples, index)
                    match status with
                    | StatusCode.TooManySamples -> Choice.succeed None
                    | _                         -> status |> checkStatusAndReturn (Some (Interval_ns (nanosec <| interval), maxSamples))) 

        /// Asynchronously queries the parameters for the specified timebase on a PicoScope 3000 series
        /// device with the current vertical resolution.
        let internal queryTimebaseParameters picoScope timebase segment =
            async {
                let! deviceTimebase = queryIntervalAndMaxSamples picoScope timebase segment
                match deviceTimebase with
                | Some (interval, maxSamples) ->
                    return Some { Timebase       = timebase
                                  MaximumSamples = maxSamples
                                  SampleInterval = interval }
                | None -> return None }

        /// Find the best matching available timebase for the requested sample interval
        let internal findTimebaseForSampleInterval picoScope segment (interval : Interval) =

            // guess timebase based on documented capabilities
            // some resolutions might not be available, depending on enabled channels so check with the instrument
            let calculatedTimebase = Timebase.timebase interval

            let rec findLongerInterval proposed = async {
                let! response = queryTimebaseParameters picoScope proposed segment
                match response with
                | None -> return! findLongerInterval (proposed+1u)
                | Some param  when (param.SampleInterval < interval) ->
                     return! findLongerInterval (proposed+1u) // exceeds current max samples
                | Some param ->
                     return (proposed,param) }

            let rec findShorterInterval proposed (fastestKnown:TimebaseParameters) = async {
                let! response = queryTimebaseParameters picoScope proposed segment
                match response with
                | None ->
                    return Choice.fail
                           << Exception
                           << sprintf "No matching timebase available. Fastest current available in the current configuration is %O"
                           <| fastestKnown.SampleInterval
                | Some param when (param.SampleInterval <= interval) ->
                    return Choice.succeed param
                | Some param when (proposed > 0u) ->
                    return! findShorterInterval (proposed-1u) param
                | Some param ->  // timebase = 0, no faster timebase available
                    return Choice.fail
                           << Exception
                           << sprintf "No matching timebase available. Fastest available on this device is %O"
                           <| fastestKnown.SampleInterval }

            async {
                let! response = queryTimebaseParameters picoScope calculatedTimebase segment
                match response with
                | Some parameters when (parameters.SampleInterval = interval)
                    ->  logDebug <| sprintf "Found requested timebase %d %A" parameters.Timebase interval
                        return parameters
                | _ -> logDebug <| "Seeking closest available timebase"
                       let! (upper,known) = findLongerInterval calculatedTimebase
                       let! shorter = findShorterInterval upper known
                       return Choice.bindOrRaise shorter }

        /// Asynchronously segments the memory of a PicoScope 3000 series device into the specified
        /// number of segments.  Returns the number of samples which will fit in each new segment.
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

        /// Asynchronously queries the available range of analogue voltage offsets for an input channel
        /// for a PicoScope 3000 series device using the specified input voltage range and coupling.
        let queryAvailableAnalogueOffsetRange (PicoScope3000 picoScope) range coupling =
            let description = sprintf "Query available analogue offset range for input range %A with %A coupling" range coupling
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let mutable maxOffset = 0.0f
                    let mutable minOffset = 0.0f
                    let volts = LanguagePrimitives.Float32WithMeasure<V>
                    NativeApi.GetAnalogueOffset(handle device, rangeEnum range, couplingEnum coupling, &maxOffset, &minOffset)
                    |> checkStatusAndReturn (volts maxOffset, volts minOffset))

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
        /// series device. Not all 3000 series devices support this, so if it  just log and carry on
        let private setBandwidthFilter (PicoScope3000 picoScope) inputChannel bandwidthLimit =
            let description = sprintf "Set channel %A to bandwidth %A" inputChannel bandwidthLimit
            picoScope |> CommandRequestAgent.performCommand description 
                (fun device ->
                    let response = NativeApi.SetBandwidthFilter(handle device, inputChannelEnum inputChannel, bandwidthLimitEnum bandwidthLimit)
                    match response with
                    | Ok      -> Choice.succeed ()
                    | StatusCode.NotUsed -> sprintf "%s not available on this model" |> logWarning
                                            Choice.succeed ()
                    | HasError message   -> Choice.fail (Exception message))
         
        /// Asynchronously sets the input settings for the specified input channel on a PicoScope 3000
        /// series device.
        let private setChannelInputSettings (PicoScope3000 picoScope) inputChannel settings =
            let description = sprintf "Enable channel %A and setting input settings: %A" inputChannel settings
            picoScope |> CommandRequestAgent.performCommand description
                (fun device ->
                    let coupling  = couplingEnum settings.Coupling
                    let range     = rangeEnum settings.Range
                    let offset    = float32 settings.AnalogueOffset
                    NativeApi.SetChannel(handle device, inputChannelEnum inputChannel, 1s, coupling, range, offset) 
                    |> checkStatus)

        let private enableDigitalPort (PicoScope3000 picoScope) port settings =
            let enableDigitalPort' device =
                let level = Adc.logicLevelFromVoltage settings.LogicLevel
                NativeApi.SetDigitalPort(handle device, digitalPortEnum port, 1s, level)
                |> checkStatus

            let description = sprintf "Enable digital channel %A with settings: %A" port settings
            picoScope |> CommandRequestAgent.performCommand description enableDigitalPort'

        /// Asynchronously enables the specified input channel with the given input settings on a PicoScope
        /// 3000 series device.
        let private setChannelEnabled picoScope (inputChannel : InputChannel) inputSettings =
            match (inputChannel,inputSettings) with
            | (Analogue channel,AnalogueSettings settings) -> async {
                do! setChannelInputSettings picoScope channel settings
                do! setBandwidthFilter picoScope channel settings.BandwidthLimit }
            | (Digital port,DigitalSettings settings) ->
                enableDigitalPort picoScope port settings
            | (_,_) -> failwith "Cannot enable channel with invalid settings"

        /// Asynchronously disables the specified analogue input channel on a PicoScope 3000 series device.
        let private setAnalogueChannelDisabled picoScope channel =
            picoScope |> CommandRequestAgent.performCommand (sprintf "Disable channel %A" channel)
                (fun device ->
                    NativeApi.SetChannel(handle device, inputChannelEnum channel, 0s, CouplingEnum.DC, RangeEnum._10V, 0.0f) 
                    |> checkStatus)

        /// Asynchronously disables the specified input channel on a PicoScope 3000 series device.
        let private disableDigitalPort picoScope port =
            picoScope |> CommandRequestAgent.performCommand (sprintf "Disable digital port %A" port)
                (fun device ->
                    NativeApi.SetDigitalPort(handle device, digitalPortEnum port, 0s, 0s)
                    |> checkStatus)

        /// Asynchronously disables the specified input channel on a PicoScope 3000 series device.
        let private setChannelDisabled (PicoScope3000 picoScope) inputChannel =
            match inputChannel with
            | Analogue channel -> setAnalogueChannelDisabled picoScope channel
            | Digital port    -> disableDigitalPort picoScope port

        /// Asynchronously sets the settings for the specified input channel on a PicoScope 3000 series
        /// device.
        let private setChannelSettings picoScope inputChannel channelSettings =
            match channelSettings with
            | EnabledChannel inputSettings ->
                setChannelEnabled picoScope inputChannel inputSettings
            | DisabledChannel ->
                setChannelDisabled picoScope inputChannel

        /// Asynchronously sets up all input channels on as PicoScope 3000 series device with the given
        /// acquisition input settings.
        let setAcquisitionInputChannels picoScope acquisitionInputs = async {
            let requiredChannels   = Map.keys acquisitionInputs.InputSettings
            let! availableChannels = queryAvailableChannels picoScope
            if not (Set.isSubset requiredChannels availableChannels) then
                invalidArg "Input channels" "The specified acquisition inputs require input channels which are not available on the current device."
            for channel in availableChannels do
                do! Inputs.settingsForChannel channel acquisitionInputs
                    |> setChannelSettings picoScope channel }

    /// Functions related to acquisition triggering.
    module Triggering = 
        /// Asynchronously sets a PicoScope 3000 series device to trigger automatically after the specified
        /// delay.
        let private setAutoTrigger (PicoScope3000 picoScope) (AutoTriggerDelay_ms auto) =
            let description = sprintf "Set auto-trigger with delay: %d ms" (int16 auto)
            picoScope |> CommandRequestAgent.performCommand description (fun device ->
                NativeApi.SetSimpleTrigger(handle device, 0s, TriggerChannelEnum.A, 0s, ThresholdDirectionEnum.None, 0u, int16 auto)
                |> checkStatus)

        [<AutoOpen>]
        module Simple =
            open Simple
            /// Asynchronously sets up a simple trigger on a PicoScope 3000 series device which triggers
            /// acquisition at a voltage threshold crossing on an trigger channel or, optionally, also
            /// automatically after a delay.
            let internal setSimpleTrigger (PicoScope3000 picoScope) (triggerSettings : Triggering.Simple.Trigger) delay (AutoTriggerDelay_ms auto) =
                let description = sprintf "Set simple trigger settings: %A %A %A" triggerSettings delay auto
                picoScope |> CommandRequestAgent.performCommand description
                    (fun device ->
                        let channel = triggerChannelEnum triggerSettings.Source
                        let threshold                 = triggerSettings.Threshold
                        let thresholdDirection        = Simple.conditionEnum triggerSettings.Direction
                        NativeApi.SetSimpleTrigger(handle device, 1s, channel, threshold, thresholdDirection, delay, int16 auto)
                        |> checkStatus)

        [<AutoOpen>]
        module Complex =
            open Complex
            let private setTriggerChannelConditionsV2 (PicoScope3000 picoScope) conditions =
                let description = sprintf "Set trigger conditions: %A" conditions
                picoScope |> CommandRequestAgent.performCommand description
                    (fun device ->
                        NativeApi.SetTriggerChannelConditionsV2(handle device, conditions, int16 conditions.Length)
                        |> checkStatus )

            let private setTriggerChannelDirections (PicoScope3000 picoScope) conditions =
                let description = sprintf "Set trigger directions: %A+" conditions
                let channelDirection channel =
                    Map.tryFind (Channel channel)
                    >> Option.map Complex.conditionEnum
                    >> orDefault ThresholdDirectionEnum.None
                let channelA = conditions |> channelDirection (AnalogueTrigger ChannelA)
                let channelB = conditions |> channelDirection (AnalogueTrigger ChannelB)
                let channelC = conditions |> channelDirection (AnalogueTrigger ChannelC)
                let channelD = conditions |> channelDirection (AnalogueTrigger ChannelD)
                let ext = conditions |> channelDirection ExternalTrigger
                let aux = conditions |> channelDirection AuxiliaryTrigger
                picoScope |> CommandRequestAgent.performCommand description
                    (fun device ->
                        NativeApi.SetTriggerChannelDirections(handle device, channelA, channelB, channelC, channelD, ext, aux)
                        |> checkStatus )

            let private buildProperty channel (lowerThreshold : Complex.AdcThreshold) (upperThreshold : Complex.AdcThreshold) thresholdMode =
                new TriggerChannelProperties( upperThreshold.AdcThreshold, upperThreshold.Hysteresis |> orDefault Complex.noHysteresis |> uint16,
                                              lowerThreshold.AdcThreshold, lowerThreshold.Hysteresis |> orDefault Complex.noHysteresis |> uint16,
                                              triggerChannelEnum <| channel, thresholdMode)

            let private buildPropertyFromCondition toAdc channel condition =
                match condition with
                | Complex.LevelCondition level ->
                    let level' = toAdc channel level
                    buildProperty channel level' level' ThresholdModeEnum.Level
                | Complex.WindowCondition (lower,upper) ->
                    buildProperty channel (toAdc channel lower) (toAdc channel upper) ThresholdModeEnum.Window

            let private setTriggerChannelProperties (PicoScope3000 picoScope) toAdc conditions (AutoTriggerDelay_ms auto) =
                let properties = conditions |> Map.fold (fun props source condition ->
                                                           let channel = triggerChannelFromSource source
                                                           (buildPropertyFromCondition toAdc channel condition ) :: props) []
                let description = sprintf "Set trigger properties: %A %A" properties auto
                picoScope |> CommandRequestAgent.performCommand description
                    (fun device ->
                        NativeApi.SetTriggerChannelProperties( handle device,
                                                               List.toArray properties,
                                                               int16 properties.Length,
                                                               0s,
                                                               int auto)
                        |> checkStatus )

            let private setTriggerDelay (PicoScope3000 picoScope) delay =
                let description = sprintf "Set trigger acquisition delay: %A" delay
                picoScope |> CommandRequestAgent.performCommand description
                    (fun device ->
                        NativeApi.SetTriggerDelay( handle device, delay ) |> checkStatus)

            let private toConditionsV2 states =
                let toTriggerState = Option.map triggerStateEnum >> orDefault TriggerStateEnum.DontCare
                let channelA = states |> Map.tryFind (Channel (AnalogueTrigger ChannelA)) |> toTriggerState
                let channelB = states |> Map.tryFind (Channel (AnalogueTrigger ChannelB)) |> toTriggerState
                let channelC = states |> Map.tryFind (Channel (AnalogueTrigger ChannelC)) |> toTriggerState
                let channelD = states |> Map.tryFind (Channel (AnalogueTrigger ChannelD)) |> toTriggerState
                let ext      = states |> Map.tryFind (Channel ExternalTrigger)            |> toTriggerState
                let aux      = states |> Map.tryFind (Channel AuxiliaryTrigger)           |> toTriggerState
                let pwq      = states |> Map.tryFind PulseWidthQualifierTrigger |> toTriggerState
                let digital  = states |> Map.tryFind DigitalTrigger             |> toTriggerState
                new TriggerConditionsV2(channelA,channelB,channelC,channelD,ext,aux,pwq,digital)

            let private flattenConditionTree (tree:State) =
                let isLeaf = function | Require _ -> true | _ -> false
                let rec f = function
                    | Require (c,s) -> Require (c,s)
                    | And (Or (a,b), c) -> Or (And (f a,f c), And (f b,f c))
                    | And (a, Or (b,c)) -> Or (And (f a,f b), And (f a,f c))
                    | And (left,right) when isLeaf left && isLeaf right -> And (left,right)
                    | And (a,b) -> f <| And (f a,f b)
                    | Or (a,b) -> Or (f a, f b)
                let rec flattenOr = function
                    | Or (a,b)  -> flattenOr a @ flattenOr b
                    | other -> [ other ]
                let rec flattenAnd = function
                    | And (a,b) -> flattenAnd a @ flattenAnd b
                    | Require (c,s) -> [ (c,s) ]
                    | Or (a,b) -> failwith "Or term not expected"
                tree |> f |> flattenOr |> List.map (flattenAnd >> Map.ofList >> toConditionsV2)

            let flattenConditionTreeToStrings = flattenConditionTree >> List.map conditionsV2ToString

            /// Asynchronously sets the trigger settings for a PicoScope 3000 series device.
            let internal setComplexTrigger picoScope toAdc settings delay auto = async {
                    do! setTriggerChannelDirections picoScope settings.AnalogueConditions
                    do! setTriggerChannelProperties picoScope toAdc settings.AnalogueConditions auto
                    do! setTriggerDelay picoScope delay
                    let conditions = flattenConditionTree settings.TriggerState |> List.toArray
                    do! setTriggerChannelConditionsV2 picoScope conditions }

        let internal setTrigger picoScope toAdc settings =
            let delay = defaultArg settings.Delay noDelay |> uint32
            let auto  = defaultArg settings.Auto noAutoTrigger
            match settings.Trigger with
            | AutoTrigger -> setAutoTrigger picoScope auto
            | SimpleTrigger t -> setSimpleTrigger picoScope t delay auto
            | ComplexTrigger t -> setComplexTrigger picoScope toAdc t delay auto

    /// Functions related to setting up data buffers for an acquisition.
    module internal DataBuffers =
        /// Asynchronously sets a data buffer to a PicoScope 3000 series device which will be used to
        /// transfer samples from the device memory to the computer.
        let setDataBuffer (PicoScope3000 picoScope) inputChannel downsamplingMode (index : MemorySegment) acquisitionBuffer =
            let description = sprintf "Set data buffer for channel %+A with downsampling mode %A on memory segment %d"
                                inputChannel downsamplingMode index
            
            picoScope |> CommandRequestAgent.performCommand description
                (fun device ->
                    match acquisitionBuffer with
                    | SingleBuffer buffer ->
                        NativeApi.SetDataBuffer(handle device, bufferEnum inputChannel, buffer, buffer.Length,
                                                index, downsamplingModeEnum downsamplingMode) |> checkStatus
                    | BufferPair (bufferMax, bufferMin) ->
                        NativeApi.SetDataBuffers(handle device, bufferEnum inputChannel, bufferMax, bufferMin, bufferMax.Length,
                                                 index, downsamplingModeEnum downsamplingMode) |> checkStatus)

    /// Functions related to acquisition.
    module internal Acquisition =
        /// Asynchronously stops a PicoScope 3000 series acquisition currently in progress.
        let stop (PicoScope3000 picoScope) =
            picoScope |> CommandRequestAgent.performCommand "Stop acquisition"
                (handle >> NativeApi.Stop >> checkStatus)

        /// Set number of captures. Must be no more than the number of memory segments
        let setNumberOfCaptures (PicoScope3000 picoScope) count =
            picoScope |> CommandRequestAgent.performCommand "Set number of captures"
                (fun device ->
                    NativeApi.SetNumberOfCaptures (handle device, count)
                    |> checkStatus )

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
                    { ValuesReady = { Capture = 0u
                                      StartIndex = startIndex
                                      NumberOfSamples = numberOfSamples
                                      VoltageOverflows = voltageOverflowChannels overflowBits }
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
        let startStreaming (PicoScope3000 picoScope) (streamingParameters:StreamingParameters) =
            let description = sprintf "Start streaming acquisition: %+A" streamingParameters
            picoScope |> CommandRequestAgent.performObjectRequest description
                (fun device ->
                    let (requestedInterval, timeUnit)                     = intervalAndTimeUnitEnum streamingParameters.Acquisition.SampleInterval
                    let (autoStop, preTriggerSamples, postTriggerSamples) = streamStopParameters streamingParameters.StreamStop
                    let (bufferLength)                                    = streamingParameters.Acquisition.BufferLength
                
                    let downsamplingMode =
                        streamingParameters.Acquisition.Inputs.InputSampling
                        |> Set.map (fun sampling -> sampling.DownsamplingMode)
                        |> downsamplingModeEnumForSet

                    let downsamplingRatio = 
                        match streamingParameters.Acquisition.DownsamplingRatio with
                        | Some downsamplingRatio -> downsamplingRatio
                        | None                   -> 1u

                    let mutable hardwareInterval = uint32 requestedInterval
                    NativeApi.RunStreaming(handle device, &hardwareInterval, timeUnit, preTriggerSamples, postTriggerSamples,
                                            autoStop, downsamplingRatio, downsamplingMode, uint32 bufferLength)
                    |> checkStatusAndReturn (parseIntervalWithInterval (int hardwareInterval, timeUnit)))


        let private runBlock' (acquisition : BlockAcquisition) timebase device =
            let mutable timeIndisposed : int = 0
            let parameters = acquisition.Parameters
            let guard = new Async.ContinuationGuard ()

            async {
                let! ct = Async.CancellationToken // get the token for this context

                // On success, continue with a Choice.Success via cont
                // On failure, return with a Choice.Failure via cont
                // On cancellation in this scope, continue via ccont
                // The exception continuation is not used
                // Exactly one continuation function must be called
                let continuations (cont,econt,ccont) =

                    // On cancellation in this scope, just pass control to the cancellation continuation
                    let cancellationCompensation() =
                        if guard.Cancel then
                            OperationCanceledException() |> ccont
                    use reg = ct.Register <| Action cancellationCompensation

                    // Handling the callback. On success or error continue via cont
                    let blockReadyStatus _ status _ =
                        if guard.Finish then
                            checkStatus status |> cont

                    // Set up the callback. On failure continue via cont
                    try
                        NativeApi.RunBlock( handle device,
                                            parameters.PreTriggerSamples,
                                            parameters.PostTriggerSamples,
                                            timebase,
                                            0s,
                                            &timeIndisposed,
                                            MemorySegment.zero, // Always use the first memory segment for single acquisitions
                                            PicoScopeBlockReady(blockReadyStatus),
                                            nativeint 0) |> checkStatus |> Choice.bindOrRaise
                    with
                    | exn -> if guard.Finish then Choice.fail exn |> cont

                return! Async.FromContinuations continuations }

        let runBlock (PicoScope3000 picoScope) (acquisition : BlockAcquisition) timebase =
            let description = sprintf "Start block acquisition: %+A" acquisition
            async {
                let parameters = acquisition.Common.Parameters
                return! picoScope |> CommandRequestAgent.performCommandAsync description (runBlock' acquisition timebase)  }
        
        let private downsamplingMode (acq:AcquisitionCommon) =
            acq.Parameters.Inputs.InputSampling
            |> Set.map (fun sampling -> sampling.DownsamplingMode)
            |> downsamplingModeEnumForSet

        let private downsamplingRatio (acq:AcquisitionCommon) = 
            match acq.Parameters.DownsamplingRatio with
            | Some downsamplingRatio -> downsamplingRatio
            | None                   -> 1u




        let getValues' (acquisition : BlockAcquisition) segment startIndex numberOfSamples device =
            let mutable noOfSamples : uint32 = numberOfSamples
            let mutable overflow : int16 = 0s

            NativeApi.GetValues( handle device,
                                 startIndex,
                                 &noOfSamples,
                                 downsamplingRatio acquisition.Common,
                                 downsamplingMode acquisition.Common,
                                 segment,
                                 &overflow )
                |> checkStatusAndReturn { Capture = segment
                                          StartIndex = 0u
                                          NumberOfSamples = int noOfSamples
                                          VoltageOverflows = voltageOverflowChannels overflow }
            
        let getValues (PicoScope3000 picoScope) (acquisition : BlockAcquisition) segment startIndex numberOfSamples =
            let description = sprintf "Get values from %d to %d in segment %d from block acquisition: %+A" startIndex numberOfSamples segment acquisition

            picoScope |> CommandRequestAgent.performObjectRequest description (getValues' acquisition segment startIndex numberOfSamples)


        let getValuesBulk' (acquisition : BlockAcquisition) fromSegment toSegment numberOfSamples device =
            let mutable noOfSamples : uint32 = numberOfSamples
            let numberOfSegments = int toSegment - int fromSegment + 1
            let overflow = Array.create numberOfSegments 0s

            let statusCode = NativeApi.GetValuesBulk( handle device,
                                                      &noOfSamples,
                                                      fromSegment,
                                                      toSegment,
                                                      downsamplingRatio acquisition.Common,
                                                      downsamplingMode acquisition.Common,
                                                      overflow )
            let response = seq {
                    for capture in fromSegment .. toSegment do
                        let index = int capture - int fromSegment
                        let voltageOverflow = voltageOverflowChannels overflow.[index]
                        yield { Capture = capture
                                StartIndex = 0u
                                NumberOfSamples = int noOfSamples
                                VoltageOverflows = voltageOverflow } }

            checkStatusAndReturn response statusCode
            
        let getValuesBulk (PicoScope3000 picoScope) (acquisition : BlockAcquisition) fromSegment toSegment numberOfSamples =
            let description = sprintf "Get values in bulk from segments %d to %d in from block acquisition: %+A" fromSegment toSegment acquisition

            picoScope |> CommandRequestAgent.performObjectRequest description (getValuesBulk' acquisition fromSegment toSegment numberOfSamples)
