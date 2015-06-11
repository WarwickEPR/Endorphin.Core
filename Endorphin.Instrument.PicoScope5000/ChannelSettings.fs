namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
module Range =
    let voltage =
        function
        | Range_10mV  -> VoltageInVolts 0.010f<V>
        | Range_20mV  -> VoltageInVolts 0.020f<V>
        | Range_50mV  -> VoltageInVolts 0.050f<V>
        | Range_100mV -> VoltageInVolts 0.100f<V>
        | Range_200mV -> VoltageInVolts 0.200f<V>
        | Range_500mV -> VoltageInVolts 0.500f<V>
        | Range_1V    -> VoltageInVolts 1.0f<V>
        | Range_2V    -> VoltageInVolts 2.0f<V>
        | Range_5V    -> VoltageInVolts 5.0f<V>
        | Range_10V   -> VoltageInVolts 10.0f<V>
        | Range_20V   -> VoltageInVolts 20.0f<V>
        | Range_50V   -> VoltageInVolts 50.0f<V>

    /// Returns the smallest available input range which will handle a specified zero-to-peak input voltage.
    let minimumForVoltage availableRanges inputVoltage =
        availableRanges
        |> Seq.sortBy voltage
        |> Seq.tryFind (fun rangeVoltage -> rangeVoltage > inputVoltage)