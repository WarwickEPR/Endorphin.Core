namespace Endorphin.Instrument.PicoScope5000

type Downsampling =
    | None = 0
    | Aggregate = 1
    | Decimated = 2
    | Averaged = 4
    | Distribution = 8

type StreamStop =
    | ManualStop
    | AutoStop of maxPreTriggerSamples : uint32 * maxPostTriggerSamples : uint32

type StreamingValuesReady = 
    { numberOfSamples : int
      startIndex : uint32
      voltageOverflows : Channel list
      triggerPosition : TriggerPosition
      didAutoStop : bool }

type PicoScopeBlockReady = 
    // handle, status, state -> unit 
    delegate of int16 * int16 * nativeint -> unit

type PicoScopeStreamingReady =
    // handle, numberOfSamples, startIndex, overflows, triggeredAt, triggered, autoStop, state -> unit
    delegate of int16 * int * uint32 * int16 * uint32 * int16 * int16 * nativeint -> unit

type PicoScopeDataReady =
    // handle, numberOfSamples, overflows, triggeredAt, triggered, state -> unit
    delegate of int16 * int * int16 * uint32 * int16 * nativeint -> unit
