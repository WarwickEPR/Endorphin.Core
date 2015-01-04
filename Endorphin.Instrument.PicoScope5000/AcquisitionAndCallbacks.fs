namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Units
open System

type Sample = int16

type Downsampling =
    | None = 0
    | Aggregate = 1
    | Decimated = 2
    | Averaged = 4

type BufferDescription =
    | AllSamples
    | AggregateMax
    | AggregateMin
    | Decimated
    | Averaged

type Buffer =
    | Single of buffer : BufferDescription
    | Pair of bufferMax : BufferDescription * bufferMin : BufferDescription

type StreamStop =
    | ManualStop
    | AutoStop of maxPreTriggerSamples : uint32 * maxPostTriggerSamples : uint32
    
type StreamingValuesReady = 
    { numberOfSamples : int
      startIndex : uint32
      voltageOverflows : Set<Channel>
      triggerPosition : TriggerPosition
      didAutoStop : bool }

type StreamingParameters = 
    { sampleInterval : int<ns>
      streamStop : StreamStop
      downsamplingRatio : uint32
      downsampling : Downsampling
      bufferLength : uint32 }

type IStreamingAcquisition =
    inherit IDisposable
    abstract member SampleInterval : int<ns>
    abstract member GetLatestValues : (StreamingValuesReady -> unit) -> unit

type PicoScopeBlockReady = 
    // handle, status, state -> unit 
    delegate of int16 * int16 * nativeint -> unit

type PicoScopeStreamingReady =
    // handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

type PicoScopeDataReady =
    // handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit
