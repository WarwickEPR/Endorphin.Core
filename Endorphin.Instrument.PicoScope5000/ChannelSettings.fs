namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
/// Functions related to channel input voltage range.
module Range =

    /// Converts the given input range to a voltage. The device can sample inputs ranging between the
    /// analogue voltage offset plus and minus this value.
    let voltage =
        function
        | Range_10mV  -> Voltage_V 0.010f<V>
        | Range_20mV  -> Voltage_V 0.020f<V>
        | Range_50mV  -> Voltage_V 0.050f<V>
        | Range_100mV -> Voltage_V 0.100f<V>
        | Range_200mV -> Voltage_V 0.200f<V>
        | Range_500mV -> Voltage_V 0.500f<V>
        | Range_1V    -> Voltage_V 1.0f<V>
        | Range_2V    -> Voltage_V 2.0f<V>
        | Range_5V    -> Voltage_V 5.0f<V>
        | Range_10V   -> Voltage_V 10.0f<V>
        | Range_20V   -> Voltage_V 20.0f<V>
        | Range_50V   -> Voltage_V 50.0f<V>

    /// Returns the smallest available input range which will handle the specified zero-to-peak input voltage.
    let minimumForVoltage availableRanges inputVoltage =
        availableRanges
        |> Seq.sortBy voltage
        |> Seq.tryFind (fun rangeVoltage -> rangeVoltage > inputVoltage)