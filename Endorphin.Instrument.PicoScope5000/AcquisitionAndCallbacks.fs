namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core.Units
open System

/// Specifies the downsampling mode PicoScope 5000 series buffer or buffer pair (min and max buffer, e.g. in Aggregate mode). 
/// Use a logical OR operation to specify multiple modes for the same channel.
type DownsamplingMode =
    | None = 0
    | Aggregate = 1
    | Decimated = 2
    | Averaged = 4

/// Specifies the downsampling for a PicoScope 5000 series data buffer. Note that a single DownsamplingMode corresponds to
/// a pair of BufferDownsamplings in the case of DownsamplingMode.Aggregate.
type BufferDownsampling =
    | NoDownsampling
    | AggregateMax
    | AggregateMin
    | Decimated
    | Averaged

/// Specifies the buffer format for a particular downsampling mode.
type BufferFormat =
    | Single of buffer : BufferDownsampling
    | Pair of bufferMax : BufferDownsampling * bufferMin : BufferDownsampling

    /// Returns the buffer specification for a particular downsampling mode.
    static member FromDownsamplingMode downsamplingMode =
        match downsamplingMode with
        | DownsamplingMode.None -> Single NoDownsampling
        | DownsamplingMode.Aggregate -> Pair (AggregateMax, AggregateMin)
        | DownsamplingMode.Decimated -> Single Decimated
        | DownsamplingMode.Averaged -> Single Averaged
        | _ -> invalidArg "downsamplingMode" "Unexpected downsampling mode." downsamplingMode

[<DefaultAugmentation(false)>]
/// Specifies whether a streaming acquisition should be stopped manually or automatically after a specified number of samples
/// is acquired.
type StreamStop =
    | ManualStop
    | AutoStop of maxPreTriggerSamples : uint32 * maxPostTriggerSamples : uint32
    
    /// Creates an instance of StreamStop specifiying that a streaming acquisition will be manually stopped.
    static member Manual = ManualStop

    /// Creates an instance of StreamStop specifying that a streaming acquisition shouldd stop automatically after the specified
    /// number of pre- and post-trigger samples has been acquired.
    static member Auto (maxPreTriggerSamples, maxPostTriggerSamples) =
        AutoStop(maxPreTriggerSamples, maxPostTriggerSamples)
    
    /// Converts this StreamStop valuue to an  autostop flag and values for pre- and post-trigger sasmples as required for
    /// PicoScope driver API calls.
    member streamStop.ToAutoStopAndMaxTriggerSamples() = 
        match streamStop with
        | AutoStop (preTriggerSamples, postTriggerSamples) -> (1s, preTriggerSamples, postTriggerSamples)
        | ManualStop -> (0s, 0u, 1u)
    

/// Specifies whether a trigger event occured during a block of values written to the data buffer by the PicoScope 5000 driver
/// and its position if that is the case.
type TriggerPosition =
    | NotTriggered
    | Triggered of uint32

    /// Instantiates a TriggerPostion from the triggered flag and the trigger positio provided by the PicoScope driver in a
    /// PicoScopeStreamingReady / PicoScopeDataReady callback.
    static member FromTriggeredAndPosition(triggered, position) =
        if triggered <> 0s then Triggered(position)
        else NotTriggered

/// Contains the parameters sent by the PicoScope 5000 series driver in a PicoScopeStreamingReady callback.
type StreamingValuesReady = 
    { NumberOfSamples : int
      StartIndex : uint32
      VoltageOverflows : Set<Channel>
      TriggerPosition : TriggerPosition
      DidAutoStop : bool }

/// Contains the parameters sent to the PicoScope 5000 series hardware to start a streaming acquisition.
type StreamingParameters = 
    { SampleInterval : int<ns>
      StreamStop : StreamStop
      DownsamplingRatio : uint32
      DownsamplingModes : DownsamplingMode
      BufferLength : uint32 }

/// Provides an interface for requesting the latest streaming values from the PicoScope 5000 driver once a streaming acquisition 
/// has been initiated.
type IStreamingAcquisition =
    // IDisposable.Dispose should the acquisition. This way, a 'use' statement can be used to automatically stop the acquisition even if
    // an error occurs in the client code during the acquisition.
    inherit IDisposable 

    /// Specifies the sample interval for the streaming acquisition. Note that this may differ from the requested sample interval so that
    /// it is the nearest number of integer clock cycles of the device. 
    abstract member SampleInterval : int<ns>

    /// Returns an asynchronous workflow which polls the PicoScope driver for the latest streaming values. If the driver has written new 
    /// values to the data buffer then it will then the provided callback function will be called, specifying the location and number of
    /// new values in the buffer. The callback executes on the PicoScope driver thread and as such, should not involve any heavy processing.
    /// Only use this callback to initate thread pool tasks which will read data from the buffer.
    abstract member GetLatestValues : (StreamingValuesReady -> unit) -> Async<unit>

/// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a block acquisition.
/// Format: handle, status, state -> unit 
type PicoScopeBlockReady = 
    delegate of int16 * int16 * nativeint -> unit

/// Callback delegate type used by the PicoScope driver to indicate that it has written new data to the buffer during a streaming acquisition.
/// Format; handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
type PicoScopeStreamingReady =
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

/// Callback delegate type used by the PicoScope driver to indicate that it has finished writing data to a buffer when reading data already
/// stored in the device memory.
/// Format: handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
type PicoScopeDataReady =
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit

[<AutoOpen>]
/// Provides useful extension members for types related to PicoScope data acquisition.
module AcquisitionExtensions =
    type TimeUnit with
        
        /// Returns a tuple representing the given time interval in nanoseconds, converted to a sample interval with a TimeUnit, as required
        /// for some PicoScope driver calls.
        static member FromNanoseconds (interval : int<ns>) =
            match interval with
            | interval -> (uint32 interval, TimeUnit.Nanoseconds)

        /// Returns the specified integer number of this TimeUnit to an integer value in nanoseconds.
        member timeUnit.ToNanoseconds (integerInterval : uint32) =
            match (integerInterval, timeUnit) with
            | (interval, TimeUnit.Seconds) -> (int interval) * 1000000000<ns>
            | (interval, TimeUnit.Milliseconds) -> (int interval) * 1000000<ns>
            | (interval, TimeUnit.Microseconds) -> (int interval) * 1000<ns>
            | (interval, TimeUnit.Nanoseconds) -> (int interval) * 1<ns>
            | _ -> invalidArg "TimeUnit" "Smallest supported time unit is nanoseconds." timeUnit

    type DownsamplingMode with
        
        /// Returns the BufferFormat for this downsampling mode.
        member downsampling.BufferFormat =
            match downsampling with
            | DownsamplingMode.None -> Single NoDownsampling
            | DownsamplingMode.Aggregate -> Pair (AggregateMax, AggregateMin)
            | DownsamplingMode.Decimated -> Single Decimated
            | DownsamplingMode.Averaged -> Single Averaged
            | _ -> invalidArg "Downsampling" "Unexpected Downsampling enumeration type." downsampling