module PicoScope5000Api

open PicoScope5000Status
open System.Runtime.InteropServices

type Channel =
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | Ext = 4
    | Aux = 5
    | None = 6

type Coupling = 
    | DC = 0
    | AC = 1

type Range =
    | _10mV = 0
    | _20mV = 1
    | _50mV = 2
    | _100mV = 3
    | _200mV = 4
    | _500mV = 5
    | _1V = 6
    | _2V = 7
    | _5V = 8
    | _10V = 9
    | _20V = 10
    | _50V = 11

type TimeUnit =
    | Femtoseconds = 0
    | Picoseconds = 1
    | Nanoseconds = 2
    | Microseconds = 3
    | Milliseconds = 4
    | Seconds = 5

type SignalGeneratorWaveType =
    | Sine = 0
    | Square = 1
    | Triangle = 2
    | RampUp = 3
    | RampDown = 4
    | Sinc = 5
    | Gaussian = 6
    | HalfSine = 7
    | DcVoltage = 8
    | MAX_WAVE_TYPES = 9

type SignalGeneratorSweepType = 
    | Up = 0
    | Down = 1
    | UpDown = 2
    | DownUp = 3

type SignalGeneratorExtras =
    | None = 0
    | WhiteNoise = 1
    | PseudoRandomBitStream = 2

type SignalGeneratorTriggerType =
    | Rising = 0
    | Falling = 1
    | GateHigh = 2
    | GateLow = 3

type SignalGeneratorTriggerSource =
    | None = 0
    | Scope = 1
    | Auxiliary = 2
    | External = 3
    | Software = 4

type SignalGeneratorIndexMode =
    | Single = 0
    | Dual = 1
    | Quad = 2

type EtsMode =
    | Off = 0
    | Fast = 1
    | Slow = 2

type ThresholdMode =
    | Level = 0
    | Window = 1

type ThresholdDirection = 
    // Values for level threshold mode
    | Above = 0
    | Below = 1
    | Rising = 2
    | Falling = 3
    | RisingOrFalling = 4
    // Values for window threshold mode
    | Inside = 0
    | Outside = 1
    | Enter = 2
    | Exit = 3
    | EnterOrExit = 4
    // None
    | None = 2

type DownSamplingMode =
    | None = 0 
    | Aggregate = 1

type PulseWidthType =
    | None = 0
    | LessThan = 1
    | GreaterThan = 2
    | InRange = 3
    | OutOfRange = 4

type TriggerState =
    | DontCare = 0
    | True = 1
    | False = 2

type Resolution =
    | _8bit = 0
    | _12bit = 1
    | _14bit = 2
    | _15bit = 3
    | _16bit = 4

type ChannelInfo =
    | VoltageOffsetRanges = 0

type Downsampling =
    | None = 0
    | Aggregate = 1
    | Decimated = 2
    | Averaged = 4
    | Distribution = 8

type BandwidthLimit =
    | Full = 0
    | _20MHz = 1

// this sequential StructLayout may result in non-verifiable IL code 
// when FieldOffset attributes are also used but not in this case
#nowarn "9"

open System.Text

[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type TriggerChannelProperties =
    struct 
        val ThresholdMajor : int16 
        val ThresholdMinor : int16
        val Hysteresis : uint16
        val Channel : Channel
        val ThresholdMode : ThresholdMode

        new(thresholdMajor : int16,
            thresholdMinor : int16,
            hysteresis : uint16,
            channel : Channel,
            thresholdMode : ThresholdMode) = 
            { ThresholdMajor = thresholdMajor
              ThresholdMinor = thresholdMinor
              Hysteresis = hysteresis
              Channel = channel
              ThresholdMode = thresholdMode }
    end

[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type TriggerConditions = 
    struct
        val ChannelA : TriggerState
        val ChannelB : TriggerState
        val ChannelC : TriggerState
        val ChannelD : TriggerState;
        val External : TriggerState
        val Auxiliary : TriggerState
        val PulseWidthQualifier : TriggerState

        new(channelA : TriggerState,
            channelB : TriggerState,
            channelC : TriggerState,
            channelD : TriggerState,
            external : TriggerState,
            auxiliary : TriggerState,
            pulseWidthQualifier : TriggerState) =
            { ChannelA = channelA
              ChannelB = channelB
              ChannelC = channelC
              ChannelD = channelD
              External = external
              Auxiliary = auxiliary
              PulseWidthQualifier = pulseWidthQualifier }
    end


[<StructLayout(LayoutKind.Sequential, Pack = 1)>]
type PulseWidthQualifierConditions =
    struct
        val ChannelA : TriggerState
        val ChannelB : TriggerState
        val ChannelC : TriggerState
        val ChannelD : TriggerState
        val External : TriggerState
        val Auxiliary : TriggerState

      new(channelA : TriggerState,
          channelB : TriggerState,
          channelC : TriggerState,
          channelD : TriggerState,
          external : TriggerState,
          auxiliary : TriggerState) =
          { ChannelA = channelA
            ChannelB = channelB
            ChannelC = channelC
            ChannelD = channelD
            External = external
            Auxiliary = auxiliary }
    end

type PicoScopeBlockReady = 
    // handle, status, state -> void
    delegate of int16 * int16 * nativeint -> unit

type PicoScopeStreamingReady =
    // handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

type PicoScopeDataReady =
    // handle, numberOfSamples, overflows, triggeredAt, triggered, state
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit


[<Literal>] 
let dllName = "ps5000a.dll"

[<DllImport(dllName, EntryPoint = "ps5000aChangePowerSource")>]
extern PicoStatus ChangePowerSource(int16 handle, PicoStatus powerSource)
 
[<DllImport(dllName, EntryPoint = "ps5000aCurrentPowerSource")>]
extern PicoStatus CurrentPowerSource(int16 handle);

[<DllImport(dllName, EntryPoint = "ps5000aCloseUnit")>]
extern PicoStatus CloseUnit(int16 handle);

[<DllImport(dllName, EntryPoint = "ps5000aEnumerateUnits")>]
extern PicoStatus EnumerateUnits(int16& count, StringBuilder serials, int16& serialsLength)

[<DllImport(dllName, EntryPoint = "ps5000aFlashLed")>]
extern PicoStatus FlashLed(int16 handle, int16 count)

[<DllImport(dllName, EntryPoint = "ps5000aGetAnalogueOffset")>]
extern PicoStatus GetAnalogueOffset(int16 handle, Range range, Coupling coupling, float32& maxVoltage, float32& minVoltage)

[<DllImport(dllName, EntryPoint = "ps5000aGetChannelInformation")>]
extern PicoStatus GetChannelInformation(int16 handle, ChannelInfo info, int probe, Range[] ranges, int& length, Channel channel)

[<DllImport(dllName, EntryPoint = "ps5000aGetDeviceResolution")>]
extern PicoStatus GetDeviceResolution(int16 handle, Resolution& resolution)

[<DllImport(dllName, EntryPoint = "ps5000aGetMaxDownSampleRatio")>]
extern PicoStatus GetMaximumDownsamplingRatio(int16 handle, uint32 numberOfUnaggregatedSamples, uint32& maxDownsamplingRatio, Downsampling downsampling,
    uint32 segmentIndex)

[<DllImport(dllName, EntryPoint = "ps5000aGetMaxSegments")>]
extern PicoStatus GetMaximumNumberOfSegments(int16 handle, uint32& maxSegments)

[<DllImport(dllName, EntryPoint = "ps5000aGetNoOfCaptures")>]
extern PicoStatus GetNumberOfCaptures(int16 handle, uint32& numberOfCaptures)

[<DllImport(dllName, EntryPoint = "ps5000aGetNoOfProcessedCaptures")>]
extern PicoStatus GetNumberOfProcessedCaptures(int16 handle, uint32& numberOfProcessedCaptures)

[<DllImport(dllName, EntryPoint = "ps5000aGetStreamingLatestValues")>]
extern PicoStatus GetStreamingLatestValues(int16 handle, PicoScopeStreamingReady callback, nativeint state)

[<DllImport(dllName, EntryPoint = "ps5000aGetTimebase2")>]
extern PicoStatus GetTimebase(int16 handle, uint32 timebase, int numberOfSamples, float32& timebaseIntervalInNanosec, int& maxSamples, uint32 segmentIndex)

[<DllImport(dllName, EntryPoint = "ps5000aGetTriggerTimeOffset64")>]
extern PicoStatus GetTriggerTimeOffset(int16 handle, int64& time, TimeUnit& timeUnit, uint32 segmentIndex)

[<DllImport(dllName, EntryPoint = "ps5000aGetUnitInfo")>]
extern PicoStatus GetUnitInfo(int16 handle, StringBuilder result, int16 stringLength, int16& requiredSize, PicoInfo info)

[<DllImport(dllName, EntryPoint = "ps5000aGetValues")>]
extern PicoStatus GetValues(int16 handle, uint32 startIndex, uint32& numberOfSamples, uint32 downsamplingRatio, Downsampling downsampling,
    uint32 segmentIndex, int16& overflow)

[<DllImport(dllName, EntryPoint = "ps5000aGetValuesAsync")>]
extern PicoStatus GetValuesAsync(int16 handle, uint32 startIndex, uint32 numberOfSamples, uint32 downsamplingRatio, Downsampling downsampling,
    uint32 segmentIndex, PicoScopeDataReady callback, nativeint state)

[<DllImport(dllName, EntryPoint = "ps5000aGetValuesBulk")>]
extern PicoStatus GetValuesBulk(int16 handle, uint32& numberOfSamples, uint32 fromSegmentIndex, uint32 toSegmentIndex, uint32 downsamplingRatio,
    Downsampling downsampling, int16& overflow)

[<DllImport(dllName, EntryPoint = "ps5000aGetValuesOverlapped")>]
extern PicoStatus GetValuesOverlapped(int16 handle, uint32 startIndex, uint32& numberOfSamples, uint32 downsamplingRatio, Downsampling downsampling,
    uint32 segmentIndex, int16& overflow)

[<DllImport(dllName, EntryPoint = "ps5000aGetValuesOverlappedBulk")>]
extern PicoStatus GetValuesOverlappedBulk(int16 handle, uint32 startIndex, uint32& numberOfSamples, uint32 downsamplingRatio, Downsampling downsampling,
    uint32 fromSegmentIndex, uint32& toSegmentIndex, int16& overflow)
 
[<DllImport(dllName, EntryPoint = "ps5000aGetValuesTriggerTimeOffsetBulk64")>]
extern PicoStatus GetValuesTriggerTimeOffsetBulk(int16 handle, int64[] times, TimeUnit[] timeUnits, uint32 fromSegmentIndex, uint32 toSegmentIndex);
 
[<DllImport(dllName, EntryPoint = "ps5000aIsReady")>]
extern PicoStatus IsReady(int16 handle, int16& ready);
 
[<DllImport(dllName, EntryPoint = "ps5000aIsTriggerOrPulseWidthQualifierEnabled")>]
extern PicoStatus IsTriggerOrPulseWidthQualifierEnabled(int16 handle, int16& triggerEnabled, int16& pulseWidthQualifierEnabled);
 
[<DllImport(dllName, EntryPoint = "ps5000aMaximumValue")>]
extern PicoStatus MaximumValue(int16 handle, int16& value);
 
[<DllImport(dllName, EntryPoint = "ps5000aMemorySegments")>]
extern PicoStatus MemorySegments(int16 handle, uint32 numberOfSegments, int& samplesPerSegment);
 
[<DllImport(dllName, EntryPoint = "ps5000aMinimumValue")>]
extern PicoStatus MinimumValue(int16 handle, int16& value);
 
[<DllImport(dllName, EntryPoint = "ps5000aNoOfStreamingValues")>]
extern PicoStatus NumberOfStreamingValues(int16 handle, uint32& numberOfValues);
 
[<DllImport(dllName, EntryPoint = "ps5000aOpenUnit")>]
extern PicoStatus OpenUnit(int16& handle, string serial, Resolution resolution)
 
[<DllImport(dllName, EntryPoint = "ps5000aOpenUnitAsync")>]
extern PicoStatus OpenUnitAsync(int16& handle, string serial, Resolution resolution);
 
[<DllImport(dllName, EntryPoint = "ps5000aOpenUnitProgress")>]
extern PicoStatus OpenUnitProgress(int16& handle, int16& progressPercent, int16& complete);
 
[<DllImport(dllName, EntryPoint = "ps5000aPingUnit")>]
extern PicoStatus PingUnit(int16 handle);
 
[<DllImport(dllName, EntryPoint = "ps5000aRunBlock")>]
extern PicoStatus RunBlock(int16 handle, int numberOfPreTriggerSamples, int numberOfPostTriggerSamples, uint32 timebase, int& timeIndisposedInMillisec,
    uint32 segmentIndex, PicoScopeBlockReady callback, nativeint state);
 
[<DllImport(dllName, EntryPoint = "ps5000aRunStreaming")>]
extern PicoStatus RunStreaming(int16 handle, uint32& sampleInterval, TimeUnit sampleIntervalTimeUnit, uint32 maxPreTriggerSamples,
    uint32 maxPostTriggerSamples, int16 autoStop, uint32 downsamplingRatio, Downsampling downsampling, uint32 bufferSize);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetBandwidthFilter")>]
extern PicoStatus SetBandwidthFilter(int16 handle, Channel channel, BandwidthLimit bandwidth);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetChannel")>]
extern PicoStatus SetChannel(int16 handle, Channel channel, int16 enabled, Coupling coupling, Range range, float32 analogueOffset);

[<DllImport(dllName, EntryPoint = "ps5000aSetDataBuffer")>]
extern PicoStatus SetDataBuffer(int16 handle, Channel channel, int16[] buffer, int bufferLength, uint32 segmentIndex, Downsampling downsampling);

[<DllImport(dllName, EntryPoint = "ps5000aSetDataBuffers")>]
extern PicoStatus SetDataBuffers(int16 handle, Channel channel, int16[] bufferMax, int16[] bufferMin, int bufferLength, uint32 segmentIndex,
    Downsampling downsampling);

[<DllImport(dllName, EntryPoint = "ps5000aSetDeviceResolution")>]
extern PicoStatus SetDeviceResolution(int16 handle, Resolution resolution);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetEts")>]
extern PicoStatus SetEts(int16 handle, EtsMode etsMode, int16 etsCycles, int16 etsInterleave, int& sampleIntervalInPicosec);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetEtsTimeBuffer")>]
extern PicoStatus SetEtsTimeBuffer(int16 handle, int64[] buffer, int bufferLength);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetEtsTimeBuffers")>]
extern PicoStatus SetEtsTimeBuffers(int16 handle, uint32[] timeUpper, uint32[] timeLower, int bufferLength);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetNoOfCaptures")>]
extern PicoStatus SetNumberOfCaptures(int16 handle, uint32 numberOfCaptures);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetPulseWidthQualifier")>]
extern PicoStatus SetPulseWidthQualifier(int16 handle, PulseWidthQualifierConditions[] conditions, int16 conditionsLength, ThresholdDirection direction,
    uint32 lower, uint32 upper, PulseWidthType pulseWidthType);
 
[<DllImport(dllName, EntryPoint = "ps5000aSetSigGenArbitrary")>]
extern PicoStatus SetSignalGeneratorArbitrary(int16 handle, int offsetVoltage, uint32 peakToPeak, uint32 startDeltaPhase, uint32 stopDeltaPhase,
    uint32 deltaPhaseIncrement, uint32 dwellCount, int16[] waveform, int waveformLength, SignalGeneratorSweepType sweepType,
    SignalGeneratorExtras extras, SignalGeneratorIndexMode indexMode, uint32 shots, uint32 sweeps, SignalGeneratorTriggerType triggerType,
    SignalGeneratorTriggerSource triggerSource, int16 extInThreshold);

[<DllImport(dllName, EntryPoint = "ps5000aSetSigGenBuiltIn")>]
extern PicoStatus SetSignalGeneratorBuiltIn(int16 handle, int offsetVoltage, uint32 peakToPeak, SignalGeneratorWaveType waveType,
    float32 startFrequency, float32 stopFrequency, float32 increment, float32 dwellTime, SignalGeneratorSweepType sweepType,
    SignalGeneratorExtras extras, uint32 shots, uint32 sweeps, SignalGeneratorTriggerType triggerType, SignalGeneratorTriggerSource triggerSource,
    int16 extInThreshold);

[<DllImport(dllName, EntryPoint = "ps5000aSetSimpleTrigger")>]
extern PicoStatus SetSimpleTrigger(int16 handle, int16 enabled, Channel channel, int16 threshold, ThresholdDirection direction, uint32 delay,
    int16 autoTriggerMillisec);

[<DllImport(dllName, EntryPoint = "ps5000aSetTriggerChannelConditions")>]
extern PicoStatus SetTriggerChannelConditions(int16 handle, TriggerConditions[] conditions, int16 conditionsLength);

[<DllImport(dllName, EntryPoint = "ps5000aSetTriggerChannelDirections")>]
extern PicoStatus SetTriggerChannelDirections(int16 handle, ThresholdDirection channelA, ThresholdDirection channelB, ThresholdDirection channelC,
    ThresholdDirection channelD, ThresholdDirection external, ThresholdDirection auxiliary);

[<DllImport(dllName, EntryPoint = "ps5000aSetTriggerChannelProperties")>]
extern PicoStatus SetTriggerChannelProperties(int16 handle, TriggerChannelProperties[] channelProperties, int16 channelPropertiesLength,
    int16 auxOutputEnabled, int autoTriggerMillisec);

[<DllImport(dllName, EntryPoint = "ps5000aSetTriggerDelay")>]
extern PicoStatus SetTriggerDelay(int16 handle, uint32 delay);

[<DllImport(dllName, EntryPoint = "ps5000aSetSigGenSoftwareControl")>]
extern PicoStatus SignalGeneratorSoftwareControl(int16 handle, int16 state);

[<DllImport(dllName, EntryPoint = "ps5000aStop")>]
extern PicoStatus Stop(int16 handle);
