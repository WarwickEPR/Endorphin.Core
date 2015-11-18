namespace Endorphin.Instrument.PicoScope3000

open System.Text
open System.Runtime.InteropServices
open NativeModel
open StatusCodes

/// Holds the PicoScope driver functions imported from ps3000a.dll which can only be accessed internally in this assembly. Function
/// descriptions are copied from the PicoScope 3000 series programming guide. See the guide for argument description details.
[<RequireQualifiedAccess>]
module internal NativeApi =
    module Quantities =
        /// Maximum number of ADC counts on one channel.  Number found in the PicoScope 3000A C# console
        /// source file of the SDK.
        [<Literal>]
        let maximumAdcCounts = 32512

        /// Minimum value that the logic level of a digital port may take.
        [<Literal>]
        let minimumLogicLevel = -32767

        /// Maximum value that the logic level of a digital port may take.
        [<Literal>]
        let maximumLogicLevel = 32767

    [<Literal>]
    let dllName = "ps3000a.dll" // import dll name

    [<DllImport(dllName, EntryPoint = "ps3000aChangePowerSource")>]
    /// This function selects the power supply mode. If USB power is required, you must explicitly allow it by calling this function.
    /// If the AC power adapter is connected or disconnected during use, you must also call this function. If you change power source
    /// to StatusCode.PowerSupplyNotConnected and channels C/D are currently enabled, they will be switched off. If a trigger is set
    /// using channels C/D the trigger settings for those channels will also be removed.
    extern StatusCode ChangePowerSource (int16 handle, StatusCode powerSource)

    [<DllImport(dllName, EntryPoint = "ps3000aCloseUnit")>]
    /// This function shuts down a PicoScope 3000A oscilloscope.
    extern StatusCode CloseUnit (int16 handle)

    [<DllImport(dllName, EntryPoint = "ps3000aCurrentPowerSource")>]
    /// This function returns the current power state of the device.
    extern StatusCode CurrentPowerSource (int16 handle)

    [<DllImport(dllName, EntryPoint = "ps3000aEnumerateUnits")>]
    /// This function counts the number of PicoScope 3000A units connected to the computer, and returns a list of serial numbers as a
    /// string.
    extern StatusCode EnumerateUnits (int16& count, StringBuilder serials, int16& serialsLength)

    [<DllImport(dllName, EntryPoint = "ps3000aFlashLed")>]
    /// This function flashes the LED on the front of the scope without blocking the calling thread. Calls to RunStreaming and RunBlock
    /// cancel any flashing started by this function. It is not possible to set the LED to be constantly illuminated, as this state is
    /// used to indicate that the scope has not been initialised.
    extern StatusCode FlashLed (int16 handle, int16 count)

    [<DllImport(dllName, EntryPoint = "ps3000aGetAnalogueOffset")>]
    /// This function is used to get the maximum and minimum allowable analogue offset for a specific voltage range.
    extern StatusCode GetAnalogueOffset (int16 handle, RangeEnum range, CouplingEnum coupling,
                                         float32& maxVoltage, float32& minVoltage)

    [<DllImport(dllName, EntryPoint = "ps3000aGetChannelInformation")>]
    /// This function queries which ranges are available on a scope device.
    extern StatusCode GetChannelInformation (int16 handle, ChannelInfoEnum info, int probe,
                                             RangeEnum[] ranges, int& length, ChannelEnum channel)

    [<DllImport(dllName, EntryPoint = "ps3000aGetMaxDownSampleRatio")>]
    /// This function returns the maximum downsampling ratio that can be used for a given number of samples in a given downsampling mode.
    extern StatusCode GetMaximumDownsamplingRatio (int16 handle, uint32 numberOfUnaggregatedSamples,
                                                   uint32& maxDownsamplingRatio,
                                                   DownsamplingModeEnum downsampling, uint32 segmentIndex)

    [<DllImport(dllName, EntryPoint = "ps3000aGetMaxEtsValues")>]
    /// This function returns the maximum number of cycles and maximum interleaving factor that can be used
    /// for the selected scope device in ETS mode.  These values are the upper limits for the `etsCycles` and
    /// `etsInterleave` arguments supplied to `ps3000SetEts()`
    extern StatusCode GetMaximumEtsValues (int16 handle, int16& etsCycles, int16& etsInterleave)

    [<DllImport(dllName, EntryPoint = "ps3000aGetMaxSegments")>]
    /// This function returns the maximum number of segments allowed for the opened device. Refer to
    /// `ps3000aMemorySegments` for specific figures.
    extern StatusCode GetMaximumNumberOfSegments (int16 handle, uint32& maxSegments)

    [<DllImport(dllName, EntryPoint = "ps3000aGetNoOfCaptures")>]
    /// This function finds out how many captures are available in rapid block mode after RunBlock has been called when either the
    /// collection completed or the collection of waveforms was interrupted by calling Stop. The returned value can then be used to iterate
    /// through the number of segments using GetValues, or in a single call to GetValuesBulk where it is used to calculate the toSegmentIndex
    /// parameter.
    extern StatusCode GetNumberOfCaptures (int16 handle, uint32& numberOfCaptures)

    [<DllImport(dllName, EntryPoint = "ps3000aGetNoOfProcessedCaptures")>]
    /// This function finds out how many captures in rapid block mode have been processed after ps3000aRunBlock has been called when either the
    /// collection completed or the collection of waveforms was interrupted by calling Stop. The returned value can then be used to iterate
    /// through the number of segments using GetValues, or in a single call to GetValuesBulk where it is used to calculate the toSegmentIndex
    /// parameter.
    extern StatusCode GetNumberOfProcessedCaptures (int16 handle, uint32& numberOfProcessedCaptures)

    [<DllImport(dllName, EntryPoint = "ps3000aGetStreamingLatestValues")>]
    /// This function instructs the driver to return the next block of values to your PicoScope StreamingReady callback function. You must have
    /// previously called RunStreaming beforehand to set up streaming.
    extern StatusCode GetStreamingLatestValues (int16 handle, PicoScopeStreamingReady callback,
                                                nativeint state)

    [<DllImport(dllName, EntryPoint = "ps3000aGetTimebase")>]
    /// This function calculates the sampling rate and maximum number of samples for a given timebase under the specified conditions. The result
    /// will depend on the number of channels enabled by the last call to SetChannel.
    extern StatusCode GetTimebase  (int16 handle, uint32 timebase, int numberOfSamples,
                                    int& timeInterval_ns, int16 oversample, int& maxSamples,
                                    uint32 segmentIndex)

    [<DllImport(dllName, EntryPoint = "ps3000aGetTimebase2")>]
    /// This function is an upgraded version of `ps3000aGetTimebase()`, and returns the time interval as a
    /// float32 rather than an int.  This allows it to return sub-nanosecond time intervals.
    extern StatusCode GetTimebase2 (int16 handle, uint32 timebase, int numberOfSamples,
                                    float32& timeInterval_ns, int16 oversample, int& maxSamples,
                                    uint32 segmentIndex)

    [<DllImport(dllName, EntryPoint = "ps3000aGetTriggerInfoBulk")>]
    /// This function returns trigger information in rapid block mode.
    extern StatusCode GetTriggerInfoBulk (int16 handle, TriggerInfo& triggerInfo, uint32 fromSegmentIndex,
                                          uint32 toSegmentIndex )

    [<DllImport(dllName, EntryPoint = "ps3000aGetTriggerTimeOffset64")>]
    /// This function gets the time, as a single 64-bit value, at which the trigger occurred. Call it after block-mode data has been captured or
    /// when data has been retrieved from a previous block-mode capture.
    extern StatusCode GetTriggerTimeOffset (int16 handle, int64& time, TimeUnitEnum& timeUnit,
                                            uint32 segmentIndex)

    [<DllImport(dllName, EntryPoint = "ps3000aGetUnitInfo")>]
    /// This function retrieves information about the specified oscilloscope. If the device fails to open, or no device is opened only the driver
    /// version is available.
    extern StatusCode GetUnitInfo (int16 handle, StringBuilder result, int16 stringLength,
                                   int16& requiredSize, DeviceInfoEnum info)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValues")>]
    /// This function returns block-mode data, with or without downsampling, starting at the specified sample number. It is used to get the stored
    /// data from the driver after data collection has stopped.
    extern StatusCode GetValues (int16 handle, uint32 startIndex, uint32& numberOfSamples,
                                 uint32 downsamplingRatio, DownsamplingModeEnum downsampling,
                                 uint32 segmentIndex, int16& overflow)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValuesAsync")>]
    /// This function returns data either with or without downsampling, starting at the specified sample number. It is used to get the stored data
    /// from the scope after data collection has stopped. It returns the data using a callback.
    extern StatusCode GetValuesAsync (int16 handle, uint32 startIndex, uint32 numberOfSamples,
                                      uint32 downsamplingRatio, DownsamplingModeEnum downsampling,
                                      uint32 segmentIndex, PicoScopeDataReady callback, nativeint state)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValuesBulk")>]
    /// This function retrieves waveforms captured using rapid block mode. The waveforms must have been collected sequentially and in the same run.
    extern StatusCode GetValuesBulk (int16 handle, uint32& numberOfSamples, uint32 fromSegmentIndex,
                                     uint32 toSegmentIndex, uint32 downsamplingRatio,
                                     DownsamplingModeEnum downsampling, int16& overflow)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValuesOverlapped")>]
    /// This function allows you to make a deferred data-collection request, which will later be executed, and the arguments validated, when you
    /// call RunBlock in block mode. The advantage of this function is that the driver makes contact with the scope only once, when you call RunBlock,
    /// compared with the two contacts that occur when you use the conventional RunBlock, GetValues calling sequence. This slightly reduces the dead
    /// time between successive captures in block mode.
    extern StatusCode GetValuesOverlapped (int16 handle, uint32 startIndex, uint32& numberOfSamples,
                                           uint32 downsamplingRatio, DownsamplingModeEnum downsampling,
                                           uint32 segmentIndex, int16& overflow)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValuesOverlappedBulk")>]
    /// This function allows you to make a deferred data-collection request, which will later be executed, and the arguments validated, when you call
    /// RunBlock in rapid block mode. The advantage of this method is that the driver makes contact with the scope only once, when you call RunBlock,
    /// compared with the two contacts that occur when you use the conventional RunBlock, GetValuesBulk calling sequence. This slightly reduces the
    /// dead time between successive captures in rapid block mode.
    extern StatusCode GetValuesOverlappedBulk (int16 handle, uint32 startIndex, uint32& numberOfSamples,
                                               uint32 downsamplingRatio, DownsamplingModeEnum downsampling,
                                               uint32 fromSegmentIndex, uint32 toSegmentIndex,
                                               int16& overflow)

    [<DllImport(dllName, EntryPoint = "ps3000aGetValuesTriggerTimeOffsetBulk64")>]
    /// This function retrieves the 64-bit time offsets for waveforms captured in rapid block mode.
    extern StatusCode GetValuesTriggerTimeOffsetBulk (int16 handle, int64[] times, TimeUnitEnum[] timeUnits,
                                                      uint32 fromSegmentIndex, uint32 toSegmentIndex)

    [<DllImport(dllName, EntryPoint = "ps3000aIsReady")>]
    /// This function may be used instead of a callback function to receive data from RunBlock. To use this method, pass a NULL pointer as the lpReady
    /// argument to RunBlock. You must then poll the driver to see if it has finished collecting the requested samples.
    extern StatusCode IsReady (int16 handle, int16& ready)

    [<DllImport(dllName, EntryPoint = "ps3000aIsTriggerOrPulseWidthQualifierEnabled")>]
    /// This function discovers whether a trigger, or pulse width triggering, is enabled. 
    extern StatusCode IsTriggerOrPulseWidthQualifierEnabled (int16 handle, int16& triggerEnabled,
                                                             int16& pulseWidthQualifierEnabled)

    [<DllImport(dllName, EntryPoint = "ps3000aMaximumValue")>]
    /// This function returns a status code and outputs the maximum ADC count value to a parameter. The output value depends on the currently selected
    /// resolution.
    extern StatusCode MaximumValue (int16 handle, int16& value)

    [<DllImport(dllName, EntryPoint = "ps3000aMinimumValue")>]
    /// This function returns a status code and outputs the minimum ADC count value to a parameter. The output value depends on the currently selected
    /// resolution.
    extern StatusCode MinimumValue (int16 handle, int16& value)

    [<DllImport(dllName, EntryPoint = "ps3000aMemorySegments")>]
    /// This function sets the number of memory segments that the scope will use. When the scope is opened, the number of segments defaults to 1,
    /// meaning that each capture fills the scope's available memory. This function allows you to divide the memory into a number of segments so that
    /// the scope can store several waveforms sequentially.
    extern StatusCode MemorySegments (int16 handle, uint32 numberOfSegments, int& samplesPerSegment)

    [<DllImport(dllName, EntryPoint = "ps3000aNoOfStreamingValues")>]
    /// This function returns the number of samples available after data collection in streaming mode. Call it after calling Stop.
    extern StatusCode NumberOfStreamingValues (int16 handle, uint32& numberOfValues)

    [<DllImport(dllName, EntryPoint = "ps3000aOpenUnit")>]
    /// This function opens a PicoScope 3000 Series scope attached to the computer. The maximum number
    /// of units that can be opened depends on the operating system, the kernel driver and the computer.
    /// If OpenUnit is called without the power supply connected, the driver returns
    /// StatusCode.PowerSupplyNotConnected.
    extern StatusCode OpenUnit (int16& handle, string serial)

    [<DllImport(dllName, EntryPoint = "ps3000aOpenUnitAsync")>]
    /// This function opens a scope without blocking the calling thread. You can find out when it has
    /// finished by periodically calling OpenUnitProgress until that function returns a non-zero value.
    extern StatusCode OpenUnitAsync (int16& handle, string serial)

    [<DllImport(dllName, EntryPoint = "ps3000aOpenUnitProgress")>]
    /// This function checks on the progress of a request made to OpenUnitAsync to open a scope.
    extern StatusCode OpenUnitProgress (int16& handle, int16& progressPercent, int16& complete)

    [<DllImport(dllName, EntryPoint = "ps3000aPingUnit")>]
    /// This function can be used to check that the already opened device is still connected to the USB port
    /// and communication is successful.
    extern StatusCode PingUnit (int16 handle)

    [<DllImport(dllName, EntryPoint = "ps3000aRunBlock")>]
    /// This function starts collecting data in block mode. See the programming guide for details.
    extern StatusCode RunBlock (int16 handle, int numberOfPreTriggerSamples, int numberOfPostTriggerSamples,
                                uint32 timebase, int16 oversample, int& timeIndisposed_ms,
                                uint32 segmentIndex, PicoScopeBlockReady callback, nativeint state)

    [<DllImport(dllName, EntryPoint = "ps3000aRunStreaming")>]
    /// This function tells the oscilloscope to start collecting data in streaming mode. When data has been
    /// collected from the device it is downsampled if necessary and then delivered to the application. Call
    /// GetStreamingLatestValues to retrieve the data. See the programming guide for details.
    extern StatusCode RunStreaming (int16 handle, uint32& sampleInterval,
                                    TimeUnitEnum sampleIntervalTimeUnit, uint32 maxPreTriggerSamples,
                                    uint32 maxPostTriggerSamples, int16 autoStop, uint32 downsamplingRatio,
                                    DownsamplingModeEnum downsampling, uint32 bufferSize)

    [<DllImport(dllName, EntryPoint = "ps3000aSetBandwidthFilter")>]
    /// This function specifies the bandwidth limit for a channel.
    extern StatusCode SetBandwidthFilter (int16 handle, ChannelEnum channel, BandwidthLimitEnum bandwidth)

    [<DllImport(dllName, EntryPoint = "ps3000aSetChannel")>]
    /// This function specifies whether an input channel is to be enabled, its input coupling type, voltage
    /// range and analog offset.
    extern StatusCode SetChannel (int16 handle, ChannelEnum channel, int16 enabled, CouplingEnum coupling,
                                  RangeEnum range, float32 analogueOffset)

    [<DllImport(dllName, EntryPoint = "ps3000aSetDataBuffer")>]
    /// This function tells the driver where to store the data, either unprocessed or downsampled, that will be returned after the next call to one
    /// of the GetValues functions. The function allows you to specify only a single buffer, so for aggregation mode, which requires a pair of buffers,
    /// you need to call SetDataBuffers instead. You must allocate memory for the buffer before calling this function. The buffer needs to be pinned
    /// while it is in use by the PicoScope driver to prevent the garbage collector from moving it to defragment managed memory. This is achieved using
    /// a System.Runtime.InteropServices.GCHandle.
    extern StatusCode SetDataBuffer (int16 handle, ChannelEnum channel, int16[] buffer, int bufferLength,
                                     uint32 segmentIndex, DownsamplingModeEnum downsampling)

    [<DllImport(dllName, EntryPoint = "ps3000aSetDataBuffers")>]
    /// This function tells the driver the location of a pair of buffers for receiving data. You need to allocate memory for the buffers before calling
    /// this function. If you are not using aggregate mode, then you should use SetDataBuffer instead. The buffer needs to be pinned while it is in use
    /// by the PicoScope driver to prevent the garbage collector from moving it to defragment managed memory. This is achieved using a 
    /// System.Runtime.InteropServices.GCHandle.
    extern StatusCode SetDataBuffers (int16 handle, ChannelEnum channel, int16[] bufferMax,
                                      int16[] bufferMin, int bufferLength, uint32 segmentIndex,
                                      DownsamplingModeEnum downsampling)

    [<DllImport(dllName, EntryPoint = "ps3000aSetDigitalPort")>]
    /// This function is used to enable the digital port and set the logic level (the voltage at which the
    /// state transitions from 0 to 1).
    extern StatusCode SetDigitalPort (int16 handle, DigitalPortEnum port, int16 enabled, int16 logicLevel)

    [<DllImport(dllName, EntryPoint = "ps3000aSetEts")>]
    /// This function is used to enable or disable ETS (equivalent-time sampling) and to set the ETS
    /// parameters. See ETS overview for an explanation of ETS mode.
    extern StatusCode SetEts (int16 handle, EtsModeEnum etsMode, int16 etsCycles, int16 etsInterleave,
                              int& sampleInterval_ps)
 
    [<DllImport(dllName, EntryPoint = "ps3000aSetEtsTimeBuffer")>]
    /// This function tells the driver where to find your application's ETS time buffers. These buffers contain the 64-bit timing information for each ETS
    /// sample after you run a block-mode ETS capture. The buffer needs to be pinned while it is in use by the PicoScope driver to prevent the garbage
    /// collector from moving it to defragment managed memory. This is achieved using a System.Runtime.InteropServices.GCHandle.
    extern StatusCode SetEtsTimeBuffer (int16 handle, int64[] buffer, int bufferLength)
 
    [<DllImport(dllName, EntryPoint = "ps3000aSetNoOfCaptures")>]
    /// This function sets the number of captures to be collected in one run of rapid block mode. If you do not call this function before a run, the driver
    /// will capture only one waveform. Once a value has been set, the value remains constant unless changed.
    extern StatusCode SetNumberOfCaptures (int16 handle, uint32 numberOfCaptures)

    [<DllImport(dllName, EntryPoint = "ps3000aSetPulseWidthDigitalPortProperties")>]
    /// This function will set the individual digital cahnnels' pulse-wifth trigger directions.  Each trigger
    /// direction consistss of a channel name and a directin.  If the channel is not included in the array of
    /// `PS3000A_DIGITAL_CHANNEL_DIRECTIONS` the driver assumes the digital channel's pulse-width trigger
    /// direction is `PS3000A_DIGITAL_DONT_CARE`.
    extern StatusCode SetPulseWidthDigitalPortProperties (int16 handle, DigitalChannelDirections[] directions,
                                                          int16 directionsLength)
 
    [<DllImport(dllName, EntryPoint = "ps3000aSetPulseWidthQualifier")>]
    /// This function sets up pulse-width qualification, which can be used on its own for pulse- width triggering or combined with window triggering to
    /// produce more complex triggers. The pulse-width qualifier is set by defining one or more structures that are then ORed together. Each structure is
    /// itself the AND of the states of one or more of the inputs. This AND-OR logic allows you to create any possible Boolean function of the scope's inputs.
    /// See the programming guide for details. 
    extern StatusCode SetPulseWidthQualifier (int16 handle, PulseWidthQualifierConditions[] conditions,
                                              int16 conditionsLength, ThresholdDirectionEnum direction,
                                              uint32 lower, uint32 upper, PulseWidthTypeEnum pulseWidthType)
 
    [<DllImport(dllName, EntryPoint = "ps3000aSetSigGenArbitrary")>]
    /// This function programs the signal generator to produce an arbitrary waveform. See the programming guide for details.
    extern StatusCode SetSignalGeneratorArbitrary (int16 handle, int offsetVoltage, uint32 peakToPeak,
                                                   uint32 startDeltaPhase, uint32 stopDeltaPhase,
                                                   uint32 deltaPhaseIncrement, uint32 dwellCount,
                                                   int16[] waveform, int waveformLength,
                                                   SignalGeneratorSweepTypeEnum sweepType,
                                                   SignalGeneratorExtrasEnum extras,
                                                   SignalGeneratorIndexModeEnum indexMode,
                                                   uint32 shots, uint32 sweeps,
                                                   SignalGeneratorTriggerTypeEnum triggerType,
                                                   SignalGeneratorTriggerSourceEnum triggerSource, 
                                                   int16 extInThreshold)

    [<DllImport(dllName, EntryPoint = "ps3000aSetSigGenBuiltInV2")>]
    /// This function sets up the signal generator to produce a signal from a list of built-in waveforms. If different start and stop frequencies are
    /// specified, the device will sweep either up, down or up and down.
    extern StatusCode SetSignalGeneratorBuiltIn (int16 handle, int offsetVoltage, uint32 peakToPeak,
                                                 SignalGeneratorWaveTypeEnum waveType,
                                                 float startFrequency, float stopFrequency,
                                                 float increment, float dwellTime,
                                                 SignalGeneratorSweepTypeEnum sweepType,
                                                 SignalGeneratorExtrasEnum extras, uint32 shots,
                                                 uint32 sweeps, SignalGeneratorTriggerTypeEnum triggerType,
                                                 SignalGeneratorTriggerSourceEnum triggerSource,
                                                 int16 extInThreshold)

    [<DllImport(dllName, EntryPoint = "ps300aSetSigGenPropertiesArbitrary")>]
    /// This function reprograms the arbitrary waveform generator.  All values can be reprogrammed while the
    /// signal generator is waiting for a trigger.
    extern StatusCode SetSignalGeneratorPropertiesArbitrary (int16 handle, uint32 startDeltaPhase,
                                                             uint32 stopDeltaPhase,
                                                             uint32 deltaPhaseIncrement, uint32 dwellCount,
                                                             SignalGeneratorSweepTypeEnum sweepType,
                                                             uint32 shots, uint32 sweeps,
                                                             SignalGeneratorTriggerTypeEnum triggerType,
                                                             SignalGeneratorTriggerSourceEnum triggerSource,
                                                             int16 extinThreshold)

    [<DllImport(dllName, EntryPoint = "ps300aSetSigGenPropertiesBuiltIn")>]
    /// This function reprograms the arbitrary waveform generator.  All values can be reprogrammed while the
    /// signal generator is waiting for a trigger.
    extern StatusCode SetSignalGeneratorPropertiesBuiltIn (int16 handle, double startFrequency,
                                                           double stopFrequency,
                                                           double increment, double dwellTime,
                                                           SignalGeneratorSweepTypeEnum sweepType,
                                                           uint32 shots, uint32 sweeps,
                                                           SignalGeneratorTriggerTypeEnum triggerType,
                                                           SignalGeneratorTriggerSourceEnum triggerSource,
                                                           int16 extinThreshold)

    [<DllImport(dllName, EntryPoint = "ps3000aSetSimpleTrigger")>]
    /// This function simplifies arming the trigger. It supports only the LEVEL trigger types and does not allow more than one channel to have a trigger
    /// applied to it. Any previous pulse width qualifier is cancelled.
    extern StatusCode SetSimpleTrigger (int16 handle, int16 enabled, ChannelEnum channel, int16 threshold,
                                        ThresholdDirectionEnum direction, uint32 delay, int16 autoTrigger_ms)

    [<DllImport(dllName, EntryPoint = "ps3000aSetTriggerChannelConditions")>]
    /// This function sets up trigger conditions on the scope's inputs. The trigger is defined by one or more TriggerConditions structures that are then ORed
    /// together. Each structure is itself the AND of the states of one or more of the inputs. This AND-OR logic allows you to create any possible Boolean
    /// function of the scope's inputs. If complex triggering is not required, use SetSimpleTrigger instead.
    extern StatusCode SetTriggerChannelConditions (int16 handle, TriggerConditions[] conditions,
                                                   int16 conditionsLength);

    [<DllImport(dllName, EntryPoint = "ps3000aSetTriggerChannelDirections")>]
    /// This function sets the direction of the trigger for each channel.
    extern StatusCode SetTriggerChannelDirections (int16 handle,
                                                   ThresholdDirectionEnum channelA,
                                                   ThresholdDirectionEnum channelB,
                                                   ThresholdDirectionEnum channelC,
                                                   ThresholdDirectionEnum channelD,
                                                   ThresholdDirectionEnum external,
                                                   ThresholdDirectionEnum auxiliary)

    [<DllImport(dllName, EntryPoint = "ps3000aSetTriggerChannelProperties")>]
    /// This function is used to enable or disable triggering and set its parameters.
    extern StatusCode SetTriggerChannelProperties (int16 handle,
                                                   TriggerChannelProperties[] channelProperties,
                                                   int16 channelPropertiesLength, int16 auxOutputEnabled,
                                                   int autoTrigger_ms)

    [<DllImport(dllName, EntryPoint = "ps3000aSetTriggerDelay")>]
    /// This function sets the post-trigger delay, which causes capture to start a defined time after the trigger event.
    extern StatusCode SetTriggerDelay (int16 handle, uint32 delay)

    [<DllImport(dllName, EntryPoint = "ps3000aSetTriggerDigitalPortProperties")>]
    /// This function will set the individual digital channels' trigger directions.  Each trigger direction
    /// consists of a channel name and a direction.  If the channel is not included in the arra of
    /// `PS3000A_DIGITAL_CHANNEL_DIRECTIONS` the driver assumes the digital channel's trigger direction is
    /// `PS3000A_DIGITAL_DONT_CARE`.
    extern StatusCode SetTriggerDigitalPortProperites (int16 handle, DigitalChannelDirections[] directions,
                                                       int16 directionsLength)

    [<DllImport(dllName, EntryPoint = "ps3000aSigGenArbitraryMinMaxValues")>]
    /// This function returns the range of possible sample values and waveform buffer sizes that can be
    /// supplied to `ps3000aSetSigGenArbitrary()` for setting up the arbitrary waveform generator.  These
    /// values vary between different models in the PicoScope 3000 Series.
    extern StatusCode SignalGeneratorArbitraryMinimumMaximumValues (int16 handle,
                                                                    int16& minimumArbitraryWaveformValue,
                                                                    int16& maximumArbitraryWaveformValue,
                                                                    uint32& minimumArbitraryWaveformSize,
                                                                    uint32& maximumArbitraryWaveformSize)

    [<DllImport(dllName, EntryPoint = "ps3000aSigGenFrequencyToPhase")>]
    /// This function converts a frequency to a phase count for use with the arbitrary waveofrm generator.
    /// The value returned depends on the length of the buffer, the index mode passed and the device model.
    /// The phase count can then be sent to the driver through `ps3000aSetSigGenArbitrary()` or
    /// `ps3000aSetSigGenPropertiesArbitrary()`.
    extern StatusCode SignalGeneratorFrequencyToPhase (int16 handle, double frequency,
                                                       SignalGeneratorIndexModeEnum indexMode,
                                                       uint32 bufferLength,
                                                       uint32& phase)

    [<DllImport(dllName, EntryPoint = "ps3000aSigGenSoftwareControl")>]
    /// This function causes a trigger event, or starts and stops gating. It is used when the signal generator is set to SignalGeneratorTriggerSource.Software.
    extern StatusCode SignalGeneratorSoftwareControl (int16 handle, int16 state)

    [<DllImport(dllName, EntryPoint = "ps3000aStop")>]
    /// This function stops the scope device from sampling data. If this function is called before a trigger event occurs, the oscilloscope may not contain valid
    /// data. Always call this function after the end of a capture to ensure that the scope is ready for the next capture.
    extern StatusCode Stop (int16 handle)
