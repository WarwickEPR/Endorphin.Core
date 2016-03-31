// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

open NativeModel.ChannelSettings
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

[<RequireQualifiedAccess>]
/// Functions related to channel input voltage range.
module Range =
    /// Converts the given input range to a voltage. The device can sample inputs ranging between the
    /// analogue voltage offset plus and minus this value.
    let voltage = function
        | Range_10mV  -> 0.010f<V>
        | Range_20mV  -> 0.020f<V>
        | Range_50mV  -> 0.050f<V>
        | Range_100mV -> 0.100f<V>
        | Range_200mV -> 0.200f<V>
        | Range_500mV -> 0.500f<V>
        | Range_1V    -> 1.0f<V>
        | Range_2V    -> 2.0f<V>
        | Range_5V    -> 5.0f<V>
        | Range_10V   -> 10.0f<V>
        | Range_20V   -> 20.0f<V>
        | Range_50V   -> 50.0f<V>

    /// Returns the smallest input range which will handle the specified zero-to-peak input voltage.
    let minimumForVoltage inputVoltage =
        seq { for x in System.Enum.GetValues typeof<RangeEnum> -> x :?> RangeEnum } 
        |> Seq.map (Parsing.ChannelSettings.parseRange)
        |> Seq.filter (fun range -> voltage range > inputVoltage)
        |> Seq.minBy voltage

    /// Returns the smallest input range which will provide the required offset voltage, if there is one.
    let minimumForOffsetVoltage =
        function
        | v when abs v <= 0.25f<V> -> Some Range_100mV
        | v when abs v <= 2.5f<V>  -> Some Range_500mV
        | v when abs v <= 20.0f<V> -> Some Range_5V
        | v                        -> None


module internal Adc =

    open NativeApi.Quantities

    let logicLevelFromVoltage volts =
        let perVolt = (float32 (int maximumLogicLevelAdc - int minimumLogicLevelAdc))/(10.0f<V>)
        int16 (volts * perVolt) : AdcCount

    let voltageToAdc range voltage =
        let rangeV = range |> Range.voltage
        // Adc counts are scaled to 16 bits from the actual sample resolution
        let perVolt = (float32 (int maximumAdcCounts - int minimumAdcCounts))/(2.0f * rangeV)
        int16 (voltage * perVolt) : AdcCount

    let voltsToAdc range volts =
        voltageToAdc range volts 
