namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Quantities =
    /// Enumeration representing the possible time units for samlpe intervals of a PicoScope 5000 series device.
    type internal TimeUnitEnum =
        | Femtoseconds = 0
        | Picoseconds  = 1
        | Nanoseconds  = 2
        | Microseconds = 3
        | Milliseconds = 4
        | Seconds      = 5

    type Interval =
        | IntervalInFemtoseconds of interval : int<fs>
        | IntervalInPicoseconds  of interval : int<ps>
        | IntervalInNanoseconds  of interval : int<ns>
        | IntervalInMicroseconds of inverval : int<us>
        | IntervalInMilliseconds of interval : int<ms>
        | IntervalInSeconds      of interval : int<s>

    let internal parseTimeUnitWithInterval (unit, interval) =
        match unit with
        | TimeUnitEnum.Femtoseconds -> IntervalInFemtoseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Picoseconds  -> IntervalInPicoseconds  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Nanoseconds  -> IntervalInNanoseconds  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Microseconds -> IntervalInMicroseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Milliseconds -> IntervalInMilliseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Seconds      -> IntervalInSeconds      (LanguagePrimitives.Int32WithMeasure interval)
        | enum                      -> failwithf "Unexpected time unit enum value: %A" enum

    let internal (|Interval|) =
        function
        | IntervalInFemtoseconds interval -> (TimeUnitEnum.Femtoseconds, int interval)
        | IntervalInPicoseconds  interval -> (TimeUnitEnum.Picoseconds,  int interval)
        | IntervalInNanoseconds  interval -> (TimeUnitEnum.Nanoseconds,  int interval)
        | IntervalInMicroseconds interval -> (TimeUnitEnum.Microseconds, int interval)
        | IntervalInMilliseconds interval -> (TimeUnitEnum.Milliseconds, int interval)
        | IntervalInSeconds      interval -> (TimeUnitEnum.Seconds,      int interval)
    
    type AdcCount = int16 // only alias int16 for performance
    type Voltage = VoltageInVolts of voltage : float32<V>
    type DownsamplingRatio = DownsamplingRatio of ratio : uint32
    type SegmentIndex = SegmentIndex of index : uint32
    type SampleCount = SampleCount of count : int
    type SampleIndex = SampleIndex of index : uint32
    type Timebase = Timebase of timebase : uint32
    
    type TimebaseParameters =
        { Timebase       : Timebase
          Resolution     : Resolution
          MaximumSamples : SampleCount 
          SampleInterval : Interval } 
        