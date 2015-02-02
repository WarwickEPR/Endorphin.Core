namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Linq
open System.Text
open System.Runtime.InteropServices
open log4net

/// Represents the set of messages which can be sent to a PicoScope5000 agent mailbox. This is only used internally in this assembly.
/// The functionality is exposed by the PicoScope5000 class members which queue messages to the agent mailbox and, where appropriate,
/// return asynchronous workflows to await the agent's response.
type internal Message =
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
    // ... implement in PicoScope5000Session to avoid complications?
    // | IsUnitMainsPowered of replyChannel : AsyncReplyChannel<bool> 
    // | SetMainsPower of useMainsPower : bool

    // Device resolution settings - SetDeviceResolution not yet implemented: device resolution is fixed at initialisation
    // ... implement in PicoScope5000Session to avoid complications?
    | GetDeviceResolution of replyChannel : AsyncReplyChannel<Resolution>
    // | SetDeviceResolution of resolution : Resolution

    // Channel setup
    | GetAvailableChannels of replyChannel : AsyncReplyChannel<Set<Channel>>
    | GetAvailableChannelRanges of channel : Channel *  replyChannel : AsyncReplyChannel<Range seq>
    | GetAnalogueOffsetLimits of range : Range * coupling : Coupling * replyChannel : AsyncReplyChannel<float<V> * float<V>>
    | SetChannelSettings of channel : Channel * channelSettings : ChannelSettings

    // Trigger setup
    | SetTrigger of triggerSettings : TriggerSettings
    | SetTriggerDelay of sampleCount : uint32
    | IsTriggerEnabled of replyChannel : AsyncReplyChannel<bool>
    
    // Buffer setup and memory segmentation
    | GetTimebaseInterval of timebase : uint32 * memorySegment : uint32 * replyChannel : AsyncReplyChannel<int<ns> * int32>
    | GetMaximumDownsamplingRatio of unprocessedSampleCount : uint32 * downsampling : DownsamplingMode * memorySegment : uint32 * replyChannel : AsyncReplyChannel<uint32>
    | GetMaximumNumberOfSegments of replyChannel : AsyncReplyChannel<uint32>
    | SetNumberOfMemorySegments of memorySegments : uint32 * replyChannel : AsyncReplyChannel<int32>
    | SetDataBuffer of channel : Channel * buffer : int16 array * segmentIndex : uint32 * downsampling : DownsamplingMode
    | SetAggregateDataBuffers of channel : Channel * bufferMax : int16 array * bifferMin : int16 array * segmentIndex : uint32
    | DiscardDataBuffers
    
    // Miscelaneous
    | Ping of replyChannel : AsyncReplyChannel<unit>
    | SetLedFlash of ledFlash : LedFlash

    // Acqusition
    | RunStreaming of streamingParameters : StreamingParameters * replyChannel : AsyncReplyChannel<IStreamingAcquisition>
    | GetStreamingLatestValues of callback : (StreamingValuesReady -> unit) * replyChannel : AsyncReplyChannel<unit>
    | StopAcquisition

/// Specifies the parameters for an open PicoScope 5000 connection.
type SessionParameters = {
    /// The device handle which can be used to send commands to the hardware via the PicoScope API.
    Handle : int16
    /// The serial number of the device.
    SerialNumber : string
    /// The vertical resolution currently set for the device.
    Resolution : Resolution
    /// The model number of the device.
    ModelNumber : string }
    
    with 

    /// The set of hardware input channels computed based on the device model number. Note that some of these may not be available depending 
    /// on the device resolution.
    member sessionParams.InputChannels =
        let numberOfInputs = int (sessionParams.ModelNumber.[1].ToString()) // number of input channels is the second digit in the model number
        match numberOfInputs with
        | 2 -> Set.ofList [ Channel.A ; Channel.B ]
        | 4 -> Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ]
        | _ ->  failwithf "PicoScope %s has unexpected model number %s." sessionParams.SerialNumber sessionParams.ModelNumber

/// Provides a control interface for sending messages to a PicoScope 5000 series device and acquiring data once the device has
/// been opened by instantiating a PicoScope5000Session object. This control interface is instantiated by requesting control of
/// the hardware from that object. Internally, the control interface uses a MailboxProcessor agent to send commands to the
/// hardware one at a time. The members queue messages to the mailbox and return and, where appropriate return asynchronous 
/// workflows to await the agent's response. 
type PicoScope5000(sessionParams, eventSyncContext : System.Threading.SynchronizationContext) =
    static let log = LogManager.GetLogger typeof<PicoScope5000> // logger

    // events
    let error = new Event<Exception>() // fires when an error occurs during the message processing loop
    let sessionReleased = new Event<unit>() // fires when the PicoScope5000Session is released and the agent is shut down

    // checks PicoStatus values returned by the PicoScope driver and raise an exception if this occurs, logging the action which
    // caused it
    let checkStatus action (status : PicoStatus) =
        match status.ErrorMessage with
        | None -> ()
        | Some error ->
            let logMessage = sprintf "PicoScope %s command %s failed: %s." (sessionParams.SerialNumber) (action.ToString()) error
            let exn = status.Exception (action.ToString())
            log.Error (logMessage, exn)
            raise exn

    // calls the PicoScope 5000 driver API to obtain information about the hardware
    let getUnitInfo info message =
        sprintf "Getting unit info %A from PicoScope %s." info (sessionParams.SerialNumber) |> log.Info

        // get the requested info from the driver
        let resultLength = 32s
        let result = new StringBuilder(int resultLength)
        let mutable requiredLength = 0s
        Api.GetUnitInfo(sessionParams.Handle, result, resultLength, &requiredLength, info)
        |> checkStatus message // and check the PicoStatus response
        
        result.ToString() // obtain the string written by the driver

    // returns all available information about the hardware from the PicoScope driver API
    let getAllUnitInfos message =
        seq { 
            // for each possible value of PicoInfo
            for value in (int PicoInfo.DriverVersion) .. (int PicoInfo.FirmwareVersion2) do
                let info = enum<PicoInfo>(value)
                // return a sequence element with the corresponding hardware info
                yield (info, getUnitInfo info message) }
        |> Seq.toList // and return the sequence as a list

    // start a MailboxProcessor which will handle commands and requests sequentially and communicate with the PicoScope hardware
    let agent = Agent.Start(fun mailbox ->

        // define the workflow for processing messages while the device is preparing for an acquisition
        // the dataBuffers parameter is used to store the currently set acquisition buffers with their respective GCHandle objects
        // used to prevent the garbage collector from moving the data buffers during managed memory defragmentation
        let rec prepare (dataBuffers : (int16 array * GCHandle) list) = async {           
            sprintf "(Re)entering PicoScope %s agent preparation loop." sessionParams.SerialNumber |> log.Info
            
            let! message = mailbox.Receive() // read the next message in the queue or asynchronously wait for one
            sprintf "PicoScope %s received messsage %A." sessionParams.SerialNumber message |> log.Info

            // process the message according to its type
            match message with
            
            // Release the PicoScope5000Session                
            
            | ReleaseSession ->
                sprintf "PicoScope %s releasing session." sessionParams.SerialNumber |> log.Info
                // if the data buffer list is non-empty, unpin the buffers and raise an exception
                if dataBuffers <> [] then
                    sprintf "PicoScope %s discaring data buffers after attempting to release session." sessionParams.SerialNumber |> log.Info
                    Api.Stop sessionParams.Handle |> ignore // discard the data buffers
                
                    // unpin all the pinned buffers
                    dataBuffers
                    |> List.iter (fun (_, gcHandle) -> 
                        sprintf "PicoScope %s released GC handle for buffer." sessionParams.SerialNumber |> log.Info
                        gcHandle.Free())
                    sprintf "PicoScope %s successfully released GC handles for all data buffers." sessionParams.SerialNumber |> log.Info
                
                    failwithf "PicoScope %s received ReleaseSession message with a non-empty list of data buffers" sessionParams.SerialNumber


                // if there are unprocessed messages in the queue raise an exception
                if mailbox.CurrentQueueLength <> 0 then
                    failwithf "PicoScope %s received ReleaseSession message when message queue is non-empty." sessionParams.SerialNumber

                // raise an event to indicate that the PicoScope5000 has released the session and terminate the workflow
                eventSyncContext.RaiseEvent sessionReleased ()
                return () // no continuation, so no further messages will be processed

            // Device info requests
            
            | GetUnitDriverVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.DriverVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitUsbVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.UsbVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.HardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitModelNumber replyChannel -> 
                let info = getUnitInfo PicoInfo.ModelNumber message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitSerial replyChannel -> 
                let info = getUnitInfo PicoInfo.SerialNumber message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitCalibrationDate replyChannel -> 
                let info = getUnitInfo PicoInfo.CalibrationDate message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitKernelVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.KernelVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitDigitalHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.DigitalHardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitAnalogueHardwareVersion replyChannel -> 
                let info = getUnitInfo PicoInfo.AnalogueHardwareVersion message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitFirmwareVersion1 replyChannel -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion1 message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetUnitFirmwareVersion2 replyChannel -> 
                let info = getUnitInfo PicoInfo.FirmwareVersion2 message
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message info |> log.Info
                info |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetAllUnitInfo replyChannel -> 
                let infos = getAllUnitInfos message

                // print all device information to the log
                let infoStrings = infos |> Seq.map (fun (info, value) -> sprintf "(%A: %s)" info value)
                sprintf "PicoScope %s repsponding to message %A with\n    %A." sessionParams.SerialNumber message
                    (String.Join("\n   ", infoStrings)) |> log.Info

                infos |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            // Device resolution settings

            | GetDeviceResolution replyChannel -> 
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message sessionParams.Resolution |> log.Info
                sessionParams.Resolution |> replyChannel.Reply // reply to the request with the value stored in sessionParams
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            // Channel setup
            
            | GetAvailableChannels replyChannel ->
                let availableChannels = sessionParams.InputChannels 
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message availableChannels |> log.Info
                availableChannels |> replyChannel.Reply // reply with the available channels stored in sessionParms
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetAvailableChannelRanges (channel, replyChannel) ->
                // if the channel is not available on this device according to sessionParams, raise an exception
                if not (sessionParams.InputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                // obtain the available channel voltage input ranges from the PicoScope driver
                let mutable rangesLength = 12
                let ranges = Array.zeroCreate(rangesLength)
                Api.GetChannelInformation(sessionParams.Handle, ChannelInfo.VoltageOffsetRanges, 0, ranges, &rangesLength, channel)
                |> checkStatus message // and check the PicoStatus response
                
                let availableChannelRanges = 
                    ranges
                    |> Array.toSeq // format the result as a sequence
                    |> Seq.take rangesLength // and only return the elements set by the driver

                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message availableChannelRanges |> log.Info
                availableChannelRanges |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetAnalogueOffsetLimits (range, coupling, replyChannel) ->
                // obtain the available analogue voltage offset range from the driver
                let mutable maxOffset = 0.0f
                let mutable minOffset = 0.0f
                Api.GetAnalogueOffset(sessionParams.Handle, range, coupling, &maxOffset, &minOffset)
                |> checkStatus message // and check the PicoStatus response
                
                let offsetLimits = (float maxOffset * 1.0<V>, float minOffset * 1.0<V>) // format the result as a tuple
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message offsetLimits |> log.Info
                offsetLimits |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | SetChannelSettings (channel, channelSettings) ->
                // if the channel is not available on this device according to sessionParams, raise an exception
                if not (sessionParams.InputChannels.Contains channel) then
                    invalidArg "channel" "Channel not available on PicoScope unit." channel

                match channelSettings with
                | Enabled inputSettings ->
                    // if the channel should be enabled then set the required input settings
                    Api.SetChannel(
                        sessionParams.Handle, channel, 1s, inputSettings.Coupling, inputSettings.Range, 
                        float32 inputSettings.AnalogueOffset) 
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully set channel settings on %A." sessionParams.SerialNumber channel |> log.Info
                    
                    // also set the bandwidth limit on the channel
                    Api.SetBandwidthFilter(sessionParams.Handle, channel, inputSettings.BandwidthLimit) 
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully set channel bandwidth on %A." sessionParams.SerialNumber channel |> log.Info

                | Disabled -> 
                    // if the channel should be disabled, then disable it with some default input settings
                    Api.SetChannel(sessionParams.Handle, channel, 0s, Coupling.DC, Range._10V, 0.0f) 
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully disabled %A." sessionParams.SerialNumber channel |> log.Info

                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            // Trigger setup
            
            | IsTriggerEnabled replyChannel ->
                // check whether the trigger and/or pulse width qualifier are enabled
                let mutable triggerEnabled = 0s
                let mutable pwqEnabled = 0s
                Api.IsTriggerOrPulseWidthQualifierEnabled(sessionParams.Handle, &triggerEnabled, &pwqEnabled)
                |> checkStatus message // and check the PicoStatus response

                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message (triggerEnabled <> 0s) |> log.Info
                (triggerEnabled <> 0s) |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | SetTrigger trigger ->
                match trigger with
                | AutoTrigger delay ->
                    if delay = 0s<ms> then // zero delay corresponds to no auto triggering, and something needs to trigger capture
                        invalidArg "delay" "AutoTrigger delay must be non-zero." delay

                    // if requested to set an AutoTrigger, then disable the channel trigger and set the specified auto trigger delay
                    Api.SetSimpleTrigger(sessionParams.Handle, 0s, Channel.A, 0s, ThresholdDirection.None, 0u, int16 delay)
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully set auto triggering with delay %dms." sessionParams.SerialNumber (int16 delay) |> log.Info

                | SimpleTrigger settings ->
                    // if requested to set a SimpleTrigger
                    let autoTriggerDelay = // then choose the auto trigger delay according to the AutoTrigger parameter
                        match settings.AutoTrigger with
                        | None -> int16 0 // 0 corresponds to no auto trigger
                        | Some delayMillisec ->
                            if delayMillisec = 0s<ms> then
                                invalidArg "delay" "AutoTrigger delay must be non-zero. Use 'None' instead." settings.AutoTrigger
                            int16 delayMillisec // otherwise, set the appropriate number of milliseconds

                    // set the simple trigger settings
                    Api.SetSimpleTrigger(sessionParams.Handle, 1s, settings.Channel, settings.AdcThreshold, settings.ThresholdDirection,
                        settings.DelaySamplesAfterTrigger, autoTriggerDelay)
                    |> checkStatus message // and check the PicoStatus response

                    sprintf "PicoScope %s successfully set simple trigger settings." sessionParams.SerialNumber |> log.Info
                // advanced trigger settings are not yet implemented

                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | SetTriggerDelay sampleCount ->
                // set the number of samples between the trigger event and the start of acquisition
                Api.SetTriggerDelay(sessionParams.Handle, sampleCount)
                |> checkStatus message // and check the PicoStatus response

                sprintf "PicoScope %s successfully set trigger delay." sessionParams.SerialNumber |> log.Info
                return! prepare dataBuffers // and continue processing messages with the preparation workflow
                
            // Buffer setup and memory segmentation

            | GetTimebaseInterval (timebase, segment, replyChannel) ->
                // get the sample interval and maximum number of samples for the specified timebase from the driver
                let mutable interval = 0
                let mutable maxSamples = 0
                Api.GetTimebase(sessionParams.Handle, timebase, 0, &interval, &maxSamples, segment)
                |> checkStatus message // and check the PicoStatus response
                
                let timebase = (interval * 1<ns>, maxSamples) // appply nanoseconds unit and format the result as atuple
                sprintf "PicoScope %s responding to message %A with %A." sessionParams.SerialNumber message timebase |> log.Info
                timebase |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetMaximumDownsamplingRatio (sampleCount, downsampling, memorySegment, replyChannel) ->
                // get the maximum downsampling ratio from the driver
                let mutable maxDownsamplingRatio = 0u
                Api.GetMaximumDownsamplingRatio(sessionParams.Handle, sampleCount, &maxDownsamplingRatio, downsampling, memorySegment) 
                |> checkStatus message // and check the PicoStatus response

                sprintf "PicoScope %s responding to message %A with %d." sessionParams.SerialNumber message maxDownsamplingRatio |> log.Info
                maxDownsamplingRatio |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | GetMaximumNumberOfSegments replyChannel ->
                // get the maximum number of segments from the driver
                let mutable maxSegments = 0u
                Api.GetMaximumNumberOfSegments(sessionParams.Handle, &maxSegments)
                |> checkStatus message // and check the PicoStatus response
                
                sprintf "PicoScope %s responding to message %A with %d." sessionParams.SerialNumber message maxSegments |> log.Info
                maxSegments |> replyChannel.Reply // reply to the request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | SetNumberOfMemorySegments (memorySegmentCount, replyChannel) ->
                // set the number of memory segments on the device
                let mutable samplesPerSegement = 0
                Api.MemorySegments(sessionParams.Handle, memorySegmentCount, &samplesPerSegement) |> checkStatus message
                sprintf "PicoScope %s succesfully segmented device memory." sessionParams.SerialNumber |> log.Info

                sprintf "PicoScope %s replying to message %A with %d." sessionParams.SerialNumber message samplesPerSegement |> log.Info
                samplesPerSegement |> replyChannel.Reply // reply to the request, specifying the maximum number of samples per segment
                return! prepare dataBuffers // and continue processing messages with the preparation workflow
                
            | SetDataBuffer (channel, buffer, segmentIndex, downsampling) ->
                if downsampling = DownsamplingMode.Aggregate then // if the mode is DownsamplingMode.Aggregate
                    invalidArg "downsampling" // raise an exception, as aggregate downsampling requires a pair of buffers
                        "Attempted to set data buffer with aggregate downsampling. Use SetAggregateDataBuffers instead." downsampling
                if not (sessionParams.InputChannels.Contains channel) then // if the specified channel is not available on the hardware
                    invalidArg "channel" "Channel not available on PicoScope unit." channel // then raise an exception

                // pin the specified data buffer so that the garbage collector does not move it when defragmenting managed memory
                // before/during acquisiton
                let pinnedBuffer = (buffer, GCHandle.Alloc(buffer, GCHandleType.Pinned))
                sprintf "PicoScope %s pinned buffer." sessionParams.SerialNumber |> log.Info

                // send the buffer location to the PicoScope driver
                Api.SetDataBuffer(sessionParams.Handle, channel, buffer, buffer.Length, segmentIndex, downsampling)
                |> checkStatus message // and check the PicoStatus response
                sprintf "PicoScope %s successfully set data buffer for %A with buffer length %d." sessionParams.SerialNumber channel (buffer.Length) |> log.Info

                // add the new buffer to the list of set data buffers and continue processing messages with the preparation workflow
                return! prepare (pinnedBuffer :: dataBuffers) 

            | SetAggregateDataBuffers (channel, bufferMax, bufferMin, segmentIndex) ->
                if bufferMax.Length <> bufferMin.Length then // if the length of the two buffers is different
                    invalidArg "(bufferMax, bufferMin)" // then raise an exception
                        "Attempted to set aggregate data buffers of different lengths" (bufferMax, bufferMin)
                if not (sessionParams.InputChannels.Contains channel) then // if the specified channel is not available on the hardware
                    invalidArg "channel" "Channel not available on PicoScope unit." channel // then raise an exception
                
                // pin the specified data buffers so that the garbage collector does not move them when defragmenting managed memory
                // before/during acquisition
                let pinnedBufferMax = (bufferMax, GCHandle.Alloc(bufferMax, GCHandleType.Pinned))
                sprintf "PicoScope %s pinned buffer." sessionParams.SerialNumber |> log.Info
                let pinnedBufferMin = (bufferMin, GCHandle.Alloc(bufferMin, GCHandleType.Pinned))
                sprintf "PicoScope %s pinned buffer." sessionParams.SerialNumber |> log.Info

                // send the buffer location to PicoScope driver
                Api.SetDataBuffers(sessionParams.Handle, channel, bufferMax, bufferMin, bufferMax.Length, segmentIndex, DownsamplingMode.Aggregate) 
                |> checkStatus message // and check the PicoStatus response
                sprintf "PicoScope %s successfully set aggregate data buffers for %A with buffer length %d." sessionParams.SerialNumber channel (bufferMax.Length) |> log.Info

                // add the new buffers to the list of set data buffers and continue processing messages with the preparation workflow
                return! prepare (pinnedBufferMax :: pinnedBufferMin :: dataBuffers)

            | DiscardDataBuffers ->
                // call the Api.Stop routine to discard the data buffers
                let status = Api.Stop sessionParams.Handle
                sprintf "PicoScope %s successfully discarded data buffers." sessionParams.SerialNumber |> log.Info
                
                // unpin all the pinned buffers
                dataBuffers
                |> List.iter (fun (_, gcHandle) -> 
                    sprintf "PicoScope %s released GC handle for buffer." sessionParams.SerialNumber |> log.Info
                    gcHandle.Free())
                sprintf "PicoScope %s successfully released GC handles for all data buffers." sessionParams.SerialNumber |> log.Info
                
                // check PicoStatus response for Api.Stop
                checkStatus message status

                return! prepare [] // clear the list of set data buffers and continue processing messages with the preparation workflow

            // Miscelaneous

            | Ping replyChannel -> 
                // ping the hardware 
                Api.PingUnit(sessionParams.Handle) 
                |> checkStatus message // and check the PicoStatus response

                sprintf "PicoScope %s responding to ping request." sessionParams.SerialNumber |> log.Info
                replyChannel.Reply() // reply to the ping request
                return! prepare dataBuffers // and continue processing messages with the preparation workflow

            | SetLedFlash ledFlash ->
                match ledFlash with
                | LedOff ->
                    // switch off the front panel LED
                    Api.FlashLed(sessionParams.Handle, 0s)
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully stopped LED flashing." |> log.Info

                | LedRepeat counts ->
                    if counts <= 0s then // if the specifiec counts value is less than or equal to zero, raise an exception
                        invalidArg "conunts" "The device LED can only be flashed a positive, non-zero number of times." counts
                    // flash the LED the specified number of times
                    Api.FlashLed(sessionParams.Handle, counts) 
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully set LED to flash %d times." sessionParams.SerialNumber counts |> log.Info

                | LedIndefiniteRepeat ->
                    // set the LED to flash indefinitely
                    Api.FlashLed(sessionParams.Handle, -1s)
                    |> checkStatus message // and check the PicoStatus response
                    sprintf "PicoScope %s successfully set LED to flash indefinitely." sessionParams.SerialNumber |> log.Info

                return! prepare dataBuffers // continue processing messages with the preparation workflow

            // Acquisition

            | RunStreaming (streamingParameters, replyChannel) ->
                // start the streaming acquisition according to the specified parameters                  
                let (interval, timeUnit) = TimeUnit.FromNanoseconds streamingParameters.SampleInterval
                let (autoStop, maxPreTriggerSamples, maxPostTriggerSamples) = streamingParameters.StreamStop.ToAutoStopAndMaxTriggerSamples()
                let mutable hardwareInterval = interval
                Api.RunStreaming(sessionParams.Handle, &hardwareInterval, timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop, 
                    streamingParameters.DownsamplingRatio, streamingParameters.DownsamplingModes, streamingParameters.BufferLength)
                |> checkStatus message // and check the PicoStatus response

                // determine the hardware sampling interval which may differ from the requested interval so that it is
                // the nearest multiple of the device clock rate
                let intervalWithDimension = timeUnit.ToNanoseconds hardwareInterval
                sprintf "PicoScope %s successfully initiated streaming acquisition with sample interval %A ns." 
                    sessionParams.SerialNumber intervalWithDimension |> log.Info
                
                // build a new instance of the IStreaming acquisition interface
                let acquisition = { new IStreamingAcquisition with

                    // set the sample interval property to the hardware sample interval
                    member __.SampleInterval = intervalWithDimension

                    // define the GetLatestValues function so that posts a GetStreamingLatestValues message to the agent mailbox
                    member __.GetLatestValues callback =
                        sprintf "PicoScope %s polling for latest streaming values." sessionParams.SerialNumber |> log.Info
                        (fun replyChannel -> GetStreamingLatestValues(callback, replyChannel))
                        |> mailbox.PostAndAsyncReply
                    
                    // define IDisposable.Dispose so that it stops posts a StopAcquisition message to the agent mailbox:
                    // this way, a 'use' statement can be used to automatically stop the acquisition even if an error occurs in 
                    // the client code during the acquisition
                    member __.Dispose() = 
                        sprintf "PicoScope %s stopping acquisition." sessionParams.SerialNumber |> log.Info
                        StopAcquisition |> mailbox.Post }
                    
                sprintf "PicoScope %s replying to message %A with sample initerval %Ans and stop acquisition callback."
                    sessionParams.SerialNumber message intervalWithDimension |> log.Info
                acquisition |> replyChannel.Reply // reply to the request

                return! stream dataBuffers // and process subsequent messages with the streaming workflow
                
            | _ -> // if an invalid message is posted, such as GetStreamingLatestValues
                invalidArg "message" // raise an exception
                    (sprintf "PicoScope %s received invalid message in preparing state." sessionParams.SerialNumber) message }
        
        // define the workflow for processing messages while streaming data
        and stream dataBuffers = async {

            // define a function which stops the streaming acquisition and unpins the data buffers
            let stopAcquisition reason =
                let status = Api.Stop(sessionParams.Handle) // stop the streaming acuqisition

                // unpin the data buffers
                dataBuffers
                |> List.iter (fun (_, gcHandle) -> 
                    sprintf "PicoScope %s released GC handle for buffer after %s." sessionParams.SerialNumber reason |> log.Info
                    gcHandle.Free())
                sprintf "PicoScope %s successfully released GC handles for all data buffers." sessionParams.SerialNumber |> log.Info

                checkStatus "Stopping streaming acquisition" status // check the PicoStatus response from the call to Api.Stop
            
            (sprintf "(Re)entering PicoScope %s agent streaming loop." sessionParams.SerialNumber) |> log.Info
            
            // read the next message in the queue or asynchronously wait for one
            let! message = mailbox.TryReceive 1000 // wait for up to 1000 ms
            if message.IsNone then // if no message was received
                let errorMessage = sprintf "PicoScope %s streaming acquisition failed due to timeout." sessionParams.SerialNumber
                log.Error errorMessage
                stopAcquisition "timeout" // then stop the acquisition
                failwith errorMessage // and raise an exception
             
            (sprintf "PicoScope %s received messsage %A." sessionParams.SerialNumber message) |> log.Info

            // if a message was received then process it according to its type
            match message.Value with

            | GetStreamingLatestValues(callback, replyChannel) ->
                try
                    sprintf "PicoScope %s creating streaming callback." sessionParams.SerialNumber |> log.Debug
                    let picoScopeCallback = // define the callback as required by the PicoScope API
                        PicoScopeStreamingReady(fun _ numberOfSamples startIndex overflows triggeredAt triggered didAutoStop _ ->
                            // wrap the values in a StreamingValuesReady record and send them to the user callback
                            { NumberOfSamples = numberOfSamples
                              StartIndex = startIndex
                              VoltageOverflows = 
                                  sessionParams.InputChannels // take all input channels
                                  |> Set.filter (fun channel -> ((1 <<< int channel) &&& (int overflows)) <> 0) // and determine whether any of
                                  // the corresponding voltage overflow flags is set using the appropriate bit shifts and integer bitwise AND
                              TriggerPosition = TriggerPosition.FromTriggeredAndPosition(triggered, startIndex + uint32 triggeredAt)
                              DidAutoStop = didAutoStop <> 0s } |> callback)

                    // poll the driver for the latest values
                    let status = Api.GetStreamingLatestValues(sessionParams.Handle, picoScopeCallback, IntPtr.Zero)
                    sprintf "PicoScope %s GetStreamingLatestValues API call status: %A." sessionParams.SerialNumber status |> log.Debug
                    
                    match status with
                    | PicoStatus.Busy -> // if the driver is busy, just let the user try again
                        sprintf "PicoScope %s did not request latest streaming values: driver is busy." sessionParams.SerialNumber |> log.Info
                    | _ ->
                        status |> checkStatus message // otherwise check the PicoStatus
                        sprintf "PicoScope %s requested latest streaming values." sessionParams.SerialNumber |> log.Info

                    // reply to the request, indicating that it has been processed
                    replyChannel.Reply()

                with
                | exn -> // if an error occurs
                    log.Error (sprintf "PicoScope %s acquisition due to error %A." sessionParams.SerialNumber exn, exn)
                    stopAcquisition "error while requesting latest values" // stop the acquisition and unpin the buffers
                    raise exn // then propagate the exception

                return! stream dataBuffers // continue processing messages with the streaming acquisition workflow

            | StopAcquisition ->
                stopAcquisition "finishing stream" // stop the acquisiton
                sprintf "PicoScope %s stopped data acquisition." sessionParams.SerialNumber |> log.Info
                return! prepare [] // and return to the preparation message-processing workflow
            
            | _ -> // if an invalid message is posted
                stopAcquisition "unexpected message" // stop the acquisition 
                invalidArg "message" // and raise an exception
                    (sprintf "PicoScope %s received invalid message in streaming acquisition state." sessionParams.SerialNumber) message }

        // initialise in preparing state with empty data buffer list
        prepare [])

    do // propagate the agent error event on the specified System.Threading.SynchronizationContext.
        agent.Error |> Event.add (fun exn -> eventSyncContext.RaiseEvent error exn)

    /// Event indicating that an error has occured while processing a message and the MailboxProcessor agent has stopped. Events are fired
    /// on the System.Threading.SynchronizationContext specified at initialisation.
    member __.Error = error.Publish

    /// Event indiciating that the PicoScope5000Session has been released. Events are fired on the System.Threading.SynchronizationContext
    /// specified at initialisation.
    member __.SessionReleased = sessionReleased.Publish

    interface IDisposable with
        /// Releases the PicoScope5000Session.
        member __.Dispose() =
            ReleaseSession |> agent.Post

    // Device info requests

    /// Asynchronously requests the PicoScope driver version.
    member __.GetUnitDriverVersionAsync() =
        GetUnitDriverVersion
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device USB version used for the connection.
    member __.GetUnitUsbVersionAsync() =
        GetUnitUsbVersion
        |> agent.PostAndAsyncReply
       
    /// Asynchronously requests the PicoScope device hardware version.
    member __.GetUnitHardwareVersionAsync() =
        GetUnitHardwareVersion
        |> agent.PostAndAsyncReply
       
    /// Asynchronously requests the PicoScope device model number.
    member __.GetUnitModelNumberAsync() =
        GetUnitModelNumber
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device serial number.
    member __.GetUnitSerialAsync() =
        GetUnitSerial
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device calibration date.
    member __.GetUnitCalibrationDateAsync() =
        GetUnitCalibrationDate
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device kernel version.
    member __.GetUnitKernelVersionAsync() =
        GetUnitKernelVersion
        |> agent.PostAndAsyncReply
    
    /// Asynchronously requests the PicoScope device digital hardware version.
    member __.GetUnitDigitalHardwareVersionAsync() =
        GetUnitDigitalHardwareVersion
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device analogue hardware version.
    member __.GetUnitAnalogueHardwareVersionAsync() =
        GetUnitAnalogueHardwareVersion
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device firmware version (part 1).
    member __.GetUnitFirmwareVersion1Async() =
        GetUnitFirmwareVersion1
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the PicoScope device firmware version (part 2).
    member __.GetUnitFirmwareVersion2Async() =
        GetUnitFirmwareVersion2
        |> agent.PostAndAsyncReply

    /// Asynchronously requests all PicoScope device information.
    member __.GetAllUnitInfoAsync() =
        GetAllUnitInfo 
        |> agent.PostAndAsyncReply

    // Device resolution settings
       
    /// Asynchronously requests the PicoScope's current vertical resolution.
    member __.GetDeviceResolutionAsync() =
        GetDeviceResolution
        |> agent.PostAndAsyncReply

    // Channel settings

    /// Asynchronously requests the set of available input channels on the PicoScope hardware. Note that some of these may be unusable in
    /// the currently set vertical resolution.
    member __.GetAvailableChannelsAsync() =
        GetAvailableChannels
        |> agent.PostAndAsyncReply
    
    /// Asynchronously requests the available voltage input range settings for the specified channel.
    member __.GetAvailableChannelRangesAsync channel =
        fun replyChannel -> GetAvailableChannelRanges(channel, replyChannel)
        |> agent.PostAndAsyncReply
    
    /// Asynchronously requests the analogue voltage offset limits for the specified voltage range and input copupling.
    member __.GetAnalogueOffsetLimitsAsync(range, coupling) =
        fun replyChannel -> GetAnalogueOffsetLimits(range, coupling, replyChannel)
        |> agent.PostAndAsyncReply
       
    /// Posts a message to the PicoScope agent to set the specified channel settings to a channel.
    member __.SetChannelSettings(channel, channelSettings) =
        SetChannelSettings(channel, channelSettings)
        |> agent.Post

    /// Returns a conversion function from ADC counts to voltage for a specified voltage input range and analogue offset.
    member __.GetAdcCountToVoltageConversion(range : Range, analogueOffset) =
        let maxAdcCounts = // as defined on page 6 of the programming guide
            match sessionParams.Resolution with
            | Resolution._8bit -> 0x7F00s
            | _ -> 0x7FFFs

        let voltageRange = range.ToVolts()
        fun (adcCounts : int16) -> (voltageRange * (float adcCounts) / (float maxAdcCounts) + analogueOffset)

    /// Returns a conversion function from voltage to ADC counts for a specified voltage input range and analogue offset.
    member __.GetVoltageToAdcCountConversion(range : Range, analogueOffset) : (float<V> -> int16) =
        let maxAdcCounts = // as defined on page 6 of the programming guide
            match sessionParams.Resolution with
            | Resolution._8bit -> 0x7F00s
            | _ -> 0x7FFFs

        let voltageRange = range.ToVolts() 
        fun voltage -> int16 (((voltage - analogueOffset) / voltageRange) * (float maxAdcCounts))

    // Trigger settings
    
    /// Asynchronously checkes whether a trigger is currently enabled on the PicoScope.
    member __.IsTriggerEnabledAsync() =
        IsTriggerEnabled
        |> agent.PostAndAsyncReply
    
    /// Post a message to the PicoScope agent to set the specified trigger settings.
    member __.SetTrigger triggerSettings =
        SetTrigger triggerSettings
        |> agent.Post

    /// Posts a message to the PicoScope agent to set the specified delay between the trigger event and the start of acquisition.
    member __.SetTriggerDelay sampleCount =
        SetTriggerDelay sampleCount
        |> agent.Post

    // Buffer setup and memory segmentation
    
    /// Asynchronously requests the sample interval and maximum number of samples which can be stored for a specified timebase and
    /// memory segment. Depends on the currently set device resolution.
    member __.GetTimebaseIntervalAndMaxSamplesAsync(timebase, memorySegment) =
        fun replyChannel -> GetTimebaseInterval(timebase, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the largest available downsampling ratio for a specified sample count, downsampling mode and memory
    /// segment. 
    member __.GetMaximumDownsamplingRatioAsync(unprocessedSampleCount, downsampling, memorySegment) =
        fun replyChannel -> GetMaximumDownsamplingRatio(unprocessedSampleCount, downsampling, memorySegment, replyChannel)
        |> agent.PostAndAsyncReply

    /// Asynchronously requests the maximum number of memory segments which can be set on the device.
    member __.GetMaximumNumberOfSegmentsAsync() =
        GetMaximumNumberOfSegments
        |> agent.PostAndAsyncReply

    /// Posts a message to the PicoScope agent to set the specified number of memory segments on the device.
    member __.SetNumberOfMemorySegments memorySegments =
        SetNumberOfMemorySegments memorySegments
        |> agent.Post

    /// Posts a message to the PicoScope agent to use the specified data buffer to (down)sample the next acquisition for the specified
    /// channel and memory segment index. Note that the buffer array will be pinned so that it cannot be moved or garbage-collected
    /// until either an acquisition is started and stopped or data buffers are discarded so insure that one of these occurs to prevent
    /// a memory leak.
    member __.SetDataBuffer(channel, buffer, segmentIndex, downsampling) =
        SetDataBuffer(channel, buffer, segmentIndex, downsampling)
        |> agent.Post

    /// Posts a message to the PicoScope agent to use the specified data buffers to downsample the next acquisition for the specified
    /// channel and memory segment index in DownsamplingMode.Aggregate. Note that the buffer array will be pinned so that it cannot be
    /// moved or garbage-collected until either an acquisition is started and stopped or data buffers are discarded so insure that one
    /// of these occurs to prevent a memory leak.
    member __.SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex) =
        SetAggregateDataBuffers(channel, bufferMax, bufferMin, segmentIndex)
        |> agent.Post

    /// Discards the data buffers which have been set up and unpins the arrays so that the memory can be released by the garbage collector.
    member __.DiscardDataBuffers() =
        DiscardDataBuffers
        |> agent.Post

    // Miscelaneous
    
    /// Asynchronously pings the device.
    member __.PingAsync() =
        Ping
        |> agent.PostAndAsyncReply

    /// Posts a message to the PicoScope agent to set the front panel LED to flash or stop flashing.
    member __.SetLedFlash ledFlash =
        SetLedFlash ledFlash
        |> agent.Post
    
    // Acquisition
    
    /// Asynchronously initiates a streaming acquisition with the specified streaming parameters. Note that the device chanels, triggeing
    /// and data buffers already need to be set up at this stage. The asynchronous workflow returns an IStreamingAcquisition interface
    /// which allows the user to poll the device for the latest streaming values asynchronously and to stop the acquisition. Note that the
    /// hardware sample interval may differ from the one requested in the streaming parameters in order to be a multiple of the hardware
    /// clock rate. The actual sample interval can be obtained via IStreamingAcquisition.
    member __.RunStreamingAsync streamingParameters =
        fun replyChannel -> RunStreaming(streamingParameters, replyChannel)
        |> agent.PostAndAsyncReply