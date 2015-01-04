﻿namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Units
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
    static member ToVolts (range : Range) =
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
    static member SmallestInputRangeForVoltage (inputVoltageRange : float<V>) =
        match inputVoltageRange with
        | voltageRange when voltageRange <= 0.010<V> -> Range._10mV
        | voltageRange when voltageRange <= 0.020<V> -> Range._20mV
        | voltageRange when voltageRange <= 0.050<V> -> Range._50mV
        | voltageRange when voltageRange <= 0.100<V> -> Range._100mV
        | voltageRange when voltageRange <= 0.200<V> -> Range._200mV
        | voltageRange when voltageRange <= 0.500<V> -> Range._500mV
        | voltageRange when voltageRange <= 1.0<V> -> Range._1V
        | voltageRange when voltageRange <= 2.0<V> -> Range._2V
        | voltageRange when voltageRange <= 5.0<V> -> Range._5V
        | voltageRange when voltageRange <= 10.0<V> -> Range._10V
        | voltageRange when voltageRange <= 20.0<V> -> Range._20V
        | _ -> failwith "Requested voltage exceed maximum device input range."

    [<Extension>]
    static member FastestTimebase (resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 0u
        | Resolution._12bit -> 1u
        | Resolution._14bit
        | Resolution._15bit -> 3u
        | Resolution._16bit -> 4u
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member MaximumNumberOfChannels (resolution : Resolution) =
        MaximumNumberOfChannelsForResolution resolution

    [<Extension>]
    static member FastestStreamingIntervalInNanosec (resolution : Resolution, channelCount : int) =
        GetFastestStreamingIntervalInNanosec(resolution, channelCount)

    [<Extension>]
    static member IntegerValue (resolution : Resolution) =
        match resolution with
        | Resolution._8bit -> 8
        | Resolution._12bit -> 12
        | Resolution._14bit -> 14
        | Resolution._15bit -> 15
        | Resolution._16bit -> 16
        | _ -> failwith "Unexpected resolution."

    [<Extension>]
    static member BitMask (resolution : Resolution) =
        // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
        // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
        // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
        // correspond to the device resolution are 1s and the lowest order bits are 0s.
        ~~~ ((1s <<< (16 - Methods.IntegerValue(resolution))) - 1s)
    
    [<Extension>]
    static member ToIntegerIntervalWithTimeUnit interval =
        match interval with
        | interval when interval >= 1000000000<ns> -> (uint32 (interval / 1000000000), TimeUnit.Seconds)
        | interval when interval >= 1000000<ns> -> (uint32 (interval / 1000000), TimeUnit.Milliseconds)
        | interval when interval >= 1000<ns> -> (uint32 (interval / 1000), TimeUnit.Microseconds)
        | interval -> (uint32 interval, TimeUnit.Nanoseconds)

    [<Extension>]
    static member ToInvervalInNanoecondsFromTimeUnit (integerInterval : uint32, timeUnit) =
        match (integerInterval, timeUnit) with
        | (interval, TimeUnit.Seconds) -> (int interval) * 1000000000<ns>
        | (interval, TimeUnit.Milliseconds) -> (int interval) * 1000000<ns>
        | (interval, TimeUnit.Microseconds) -> (int interval) * 1000<ns>
        | (interval, TimeUnit.Nanoseconds) -> (int interval) * 1<ns>
        | _ -> invalidArg "timeUnit" "Smallest supported time unit is nanoseconds." timeUnit

    [<Extension>]
    static member ToAutoStopAndMaxTriggerSamples (streamStop) = 
        match streamStop with
        | AutoStop(preTriggerSamples, postTriggerSampless) -> (1s, preTriggerSamples, postTriggerSampless)
        | ManualStop -> (0s, 0u, 1u)
    
    [<Extension>]
    static member Buffer (downsampling : Downsampling) =
        match downsampling with
        | Downsampling.None -> Single AllSamples
        | Downsampling.Aggregate -> Pair (AggregateMax, AggregateMin)
        | Downsampling.Decimated -> Single Decimated
        | Downsampling.Averaged -> Single Averaged
        | _ -> invalidArg "downsampling" "Unexpected Downsampling enumeration type." downsampling