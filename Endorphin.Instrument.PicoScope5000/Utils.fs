namespace Endorphin.Instrument.PicoScope5000

open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<Extension>]
type Methods() =
    static let MaximumNumberOfChannelsForResolution =
        function
        | Resolution._8bit | Resolution._12bit | Resolution._14bit -> 4
        | Resolution._15bit -> 2
        | Resolution._16bit -> 1
        | _ -> failwith "Unexpected resolution."

    static let GetFastestStreamingIntervalInNanosec(resolution : Resolution, channelCount : int) = 
        match (resolution, channelCount) with
        | (Resolution._12bit, 4)
        | (Resolution._12bit, 3)
        | (Resolution._14bit, 4)
        | (Resolution._14bit, 3) -> 256.0
        | (Resolution._8bit, 4) 
        | (Resolution._8bit, 3) 
        | (Resolution._12bit, 2) 
        | (Resolution._14bit, 2) 
        | (Resolution._15bit, 2) -> 128.0
        | (Resolution._8bit, 3)
        | (Resolution._12bit, 1)
        | (Resolution._14bit, 1)
        | (Resolution._15bit, 1)
        | (Resolution._16bit, 2) -> 64.0
        | (Resolution._8bit, 1) -> 32.0
        | (resolution, channelCount) when channelCount > MaximumNumberOfChannelsForResolution resolution ->
            failwith "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
        | parameters -> failwith "Unexpected number of channels or resolution: %A." parameters

    [<Extension>]
    static member ToVolts(range : Range) =
        match range with
        | Range._10mV -> 0.010<V>
        | Range._20mV -> 0.020<V>
        | Range._50mV -> 0.050<V>
        | Range._100mV -> 0.100<V>
        | Range._200mV -> 0.200<V>
        | Range._500mV -> 0.500<V>
        | Range._1V -> 1.0<V>
        | Range._2V -> 2.0<V>
        | Range._5V -> 5.0<V>
        | Range._10V -> 10.0<V>
        | Range._20V -> 20.0<V>
        | Range._50V -> 50.0<V>
        | _ -> failwith "Unexpected range."

    [<Extension>]
    static member FastestTimebase(resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 0u
        | Resolution._12bit -> 1u
        | Resolution._14bit
        | Resolution._15bit -> 3u
        | Resolution._16bit -> 4u
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member MaximumNumberOfChannels(resolution : Resolution) =
        MaximumNumberOfChannelsForResolution resolution

    [<Extension>]
    static member FastestStreamingIntervalInNanosec(resolution : Resolution, channelCount : int) =
        GetFastestStreamingIntervalInNanosec(resolution, channelCount)

    [<Extension>]
    static member IntegerValue(resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 8
        | Resolution._12bit -> 12
        | Resolution._14bit -> 14
        | Resolution._15bit -> 15
        | Resolution._16bit -> 16
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member BitMask(resolution : Resolution) =
        // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
        // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
        // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
        // correspond to the device resolution are 1s and the lowest order bits are 0s.
        ~~~ ((1s <<< (16 - Methods.IntegerValue(resolution))) - 1s)
    
    [<Extension>]
    static member ToIntegerIntervalWithTimeUnit interval =
        match interval with
        | interval when interval >= 1.0<s> -> (uint32 interval, TimeUnit.Seconds)
        | interval when interval >= 1e-3<s> -> (uint32 (interval * 1e3), TimeUnit.Milliseconds)
        | interval when interval >= 1e-6<s> -> (uint32 (interval * 1e6), TimeUnit.Microseconds)
        | interval when interval >= 1e-9<s> -> (uint32 (interval * 1e9), TimeUnit.Nanoseconds)
        | interval when interval >= 1e-12<s> -> (uint32 (interval * 1e12), TimeUnit.Picoseconds)
        | interval when interval >= 1e-15<s> -> (uint32 (interval * 1e15), TimeUnit.Femtoseconds)
        | _ -> invalidArg "interval" "Time units smaller than femtoseconds are not supported." interval

    [<Extension>]
    static member ToInvervalInSecondsFromTimeUnit (integerInterval : uint32, timeUnit) =
        match (integerInterval, timeUnit) with
        | (interval, TimeUnit.Seconds) -> float interval * 1.0<s>
        | (interval, TimeUnit.Milliseconds) -> float interval * 1e-3<s>
        | (interval, TimeUnit.Microseconds) -> float interval * 1e-6<s>
        | (interval, TimeUnit.Nanoseconds) -> float interval * 1e-9<s>
        | (interval, TimeUnit.Picoseconds) -> float interval * 1e-12<s>
        | (interval, TimeUnit.Femtoseconds) -> float interval * 1e-15<s>
        | _ -> invalidArg "timeUnit" "Invalid time unit." timeUnit

    [<Extension>]
    static member ToAutoStopAndMaxTriggerSamples (streamStop) = 
        match streamStop with
        | AutoStop(preTriggerSamples, postTriggerSampless) -> (1s, preTriggerSamples, postTriggerSampless)
        | ManualStop -> (0s, 0u, 1u) 