namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Units
open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Utils =
    type Range with
        member this.ToVolts() =
            match this with
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

    type Resolution with
        member this.FastestTimebase =
            match this with
            | Resolution._8bit -> 0u
            | Resolution._12bit -> 1u
            | Resolution._14bit
            | Resolution._15bit -> 3u
            | Resolution._16bit -> 4u
            | _ -> failwith "Unexpected resolution."

        member this.MaximumNumberOfChannels =
            match this with
            | Resolution._8bit | Resolution._12bit | Resolution._14bit -> 4
            | Resolution._15bit -> 2
            | Resolution._16bit -> 1
            | _ -> failwith "Unexpected resolution."

        member this.FastestStreamingInterval channelCount =
            match (this, channelCount) with
            | (Resolution._12bit, 4)
            | (Resolution._12bit, 3)
            | (Resolution._14bit, 4)
            | (Resolution._14bit, 3) -> 256.0<ns>
            | (Resolution._8bit, 4) 
            | (Resolution._8bit, 3) 
            | (Resolution._12bit, 2) 
            | (Resolution._14bit, 2) 
            | (Resolution._15bit, 2) -> 128.0<ns>
            | (Resolution._8bit, 3)
            | (Resolution._12bit, 1)
            | (Resolution._14bit, 1)
            | (Resolution._15bit, 1)
            | (Resolution._16bit, 2) -> 64.0<ns>
            | (Resolution._8bit, 1) -> 32.0<ns>
            | (resolution, channelCount) when channelCount > this.MaximumNumberOfChannels ->
                failwith "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
            | parameters -> failwith "Unexpected number of channels or resolution: %A." parameters

        member this.Value =
            match this with
            | Resolution._8bit -> 8<bits>
            | Resolution._12bit -> 12<bits>
            | Resolution._14bit -> 14<bits>
            | Resolution._15bit -> 15<bits>
            | Resolution._16bit -> 16<bits>
            | _ -> failwith "Unexpected resolution."

        member this.BitMask =
            // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
            // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
            // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
            // correspond to the device resolution are 1s and the lowest order bits are 0s.
            ~~~ ((1s <<< (int (16<bits> - this.Value))) - 1s)

    type TimeUnit with
        static member FromNanoseconds interval =
            match interval with
            | interval when interval >= 1000000000<ns> -> (uint32 (interval / 1000000000), TimeUnit.Seconds)
            | interval when interval >= 1000000<ns> -> (uint32 (interval / 1000000), TimeUnit.Milliseconds)
            | interval when interval >= 1000<ns> -> (uint32 (interval / 1000), TimeUnit.Microseconds)
            | interval -> (uint32 interval, TimeUnit.Nanoseconds)

        member this.ToNanoseconds (integerInterval : uint32) =
            match (integerInterval, this) with
            | (interval, TimeUnit.Seconds) -> (int interval) * 1000000000<ns>
            | (interval, TimeUnit.Milliseconds) -> (int interval) * 1000000<ns>
            | (interval, TimeUnit.Microseconds) -> (int interval) * 1000<ns>
            | (interval, TimeUnit.Nanoseconds) -> (int interval) * 1<ns>
            | _ -> invalidArg "TimeUnit" "Smallest supported time unit is nanoseconds." this

    type StreamStop with
        member this.ToAutoStopAndMaxTriggerSamples() = 
            match this with
            | AutoStop(preTriggerSamples, postTriggerSampless) -> (1s, preTriggerSamples, postTriggerSampless)
            | ManualStop -> (0s, 0u, 1u)
    
    type DownsamplingMode with
        member this.Buffer =
            match this with
            | DownsamplingMode.None -> Single NoDownsampling
            | DownsamplingMode.Aggregate -> Pair (AggregateMax, AggregateMin)
            | DownsamplingMode.Decimated -> Single Decimated
            | DownsamplingMode.Averaged -> Single Averaged
            | _ -> invalidArg "Downsampling" "Unexpected Downsampling enumeration type." this