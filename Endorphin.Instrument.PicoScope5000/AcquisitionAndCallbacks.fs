namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

[<AutoOpen>]
module Acquisition =
    /// Specifies the downsampling mode PicoScope 5000 series buffer or buffer pair (min and max buffer, e.g. in Aggregate mode). 
    /// Use a logical OR operation to specify multiple modes for the same channel.
    type internal DownsamplingModeEnum =
        | None      = 0
        | Aggregate = 1
        | Decimated = 2
        | Averaged  = 4

    /// Specifies the downsampling for a PicoScope 5000 series data buffer. Note that a single DownsamplingMode corresponds to
    /// a pair of BufferDownsamplings in the case of DownsamplingMode.Aggregate.
    type Buffer =
        | NoDownsampling of buffer : AdcCount array
        | Decimated      of buffer : AdcCount array
        | Averaged       of buffer : AdcCount array
        | Aggregate      of bufferMax : AdcCount array * bufferMin : AdcCount array

    let internal bufferDownsamplingMode = 
        function
        | NoDownsampling _ -> DownsamplingModeEnum.None
        | Decimated _      -> DownsamplingModeEnum.Decimated
        | Averaged _       -> DownsamplingModeEnum.Averaged
        | Aggregate _      -> DownsamplingModeEnum.Aggregate

    let internal bufferListDownsamplingMode =
        List.map bufferDownsamplingMode
        >> List.fold (|||) DownsamplingModeEnum.None

    let internal (|SingleBuffer|BufferPair|) =
        function
        | NoDownsampling buffer            -> SingleBuffer (DownsamplingModeEnum.None,      buffer)
        | Decimated buffer                 -> SingleBuffer (DownsamplingModeEnum.Decimated, buffer)
        | Averaged buffer                  -> SingleBuffer (DownsamplingModeEnum.Averaged,  buffer)
        | Aggregate (bufferMax, bufferMin) -> BufferPair   (DownsamplingModeEnum.Aggregate, bufferMax, bufferMin)

    /// Specifies whether a streaming acquisition should be stopped manually or automatically after a specified number of samples
    /// is acquired.
    type StreamStop =
        | ManualStop
        | AutoStop of maxPreTriggerSamples : SampleIndex * maxPostTriggerSamples : SampleIndex

    let internal (|StreamStopParameters|) =
        function
        | ManualStop                                                               -> StreamStopParameters (0s, 0u, 1u)
        | AutoStop (SampleIndex preTriggerSamples, SampleIndex postTriggerSamples) -> StreamStopParameters (1s, preTriggerSamples, postTriggerSamples)

    /// Specifies whether a trigger event occured during a block of values written to the data buffer by the PicoScope 5000 driver
    /// and its position if that is the case.
    type TriggerPosition =
        | NotTriggered
        | Triggered of SampleIndex

    let internal parseTriggerPosition triggered position =
        if triggered then Triggered position
        else NotTriggered

    /// Contains the parameters sent by the PicoScope 5000 series driver in a PicoScopeStreamingReady callback.
    type StreamingValuesReady = 
        { NumberOfSamples  : SampleCount
          StartIndex       : SampleIndex
          VoltageOverflows : Set<InputChannel>
          TriggerPosition  : TriggerPosition
          DidAutoStop      : bool }

    /// Contains the parameters sent to the PicoScope 5000 series hardware to start a streaming acquisition.
    type StreamingParameters = 
        { SampleInterval    : Interval
          StreamStop        : StreamStop
          BufferLength      : SampleIndex
          DownsamplingRatio : DownsamplingRatio
          DataBuffers       : Buffer list }

    /// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a block acquisition.
    /// Format: handle, status, state -> unit 
    type internal PicoScopeBlockReady = 
        delegate of int16 * int16 * nativeint -> unit

    /// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a streaming acquisition.
    /// Format; handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
    type internal PicoScopeStreamingReady =
        delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

    /// Callback delegate type used by the PicoScope driver to indicate that it has finished writing data to a buffer when reading data already
    /// stored in the device memory.
    /// Format: handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
    type internal PicoScopeDataReady =
        delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit