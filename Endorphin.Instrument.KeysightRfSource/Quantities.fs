namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// functions to translate quantities used in the model
[<AutoOpen>]
module Quantities =
    let internal parseAmplitudeInDbm (str : string) = PowerInDbm (float str * 1.0<dBm>)
    let internal amplitudeString (PowerInDbm amplitude) = sprintf "%e dBm" (float amplitude)
    let internal parseFrequencyInHz (str : string) = FrequencyInHz (float str * 1.0<Hz>)
    let internal frequencyString (FrequencyInHz frequency) = sprintf "%e Hz" (float frequency)        
    let internal parsePhaseInRad (str : string) = PhaseInRad (float str * 1.0<rad>)
    let internal phaseString =
        function
        | PhaseInRad phase -> sprintf "%e RAD" (float phase)
        | PhaseInDeg phase -> sprintf "%e DEG" (float phase)
    let internal parseDurationInSec (str : string) = DurationInSec (float str * 1.0<s>)
    let internal durationString (DurationInSec duration) = sprintf "%e s" (float duration)
    let internal parsePercentage (str : string) = Percentage (float str * 1.0<pct>)
    let internal percentageString (Percentage percentage) = sprintf "%e PCT" (float percentage)
    let internal parseDecibelRatio (str : string) = DecibelRatio (float str * 1.0<dB>)
    let internal decibelRatioString (DecibelRatio ratio) = sprintf "%e dB" (float ratio)

    let internal parseImpedance =
        function
        | "50"      -> Impedance_50Ohm
        | "600"     -> Impedance_600Ohm
        | "1000000" -> Impedance_1MOhm
        | str       -> failwithf "Unexpected impedance string: %s." str

    let internal impedanceString =
        function
        | Impedance_50Ohm  -> "50"
        | Impedance_600Ohm -> "600"
        | Impedance_1MOhm  -> "1000000"
    
    let internal parseDirection str =
        match upperCase str with
        | "UP"   -> Up
        | "DOWN" -> Down
        | _      -> failwithf "Unexpected direction string: %s." str

    let internal directionString =
        function
        | Up   -> "UP"
        | Down -> "DOWN"

    let internal parseCoupling str =
        match upperCase str with
        | "AC" -> AC
        | "DC" -> DC
        | _    -> failwithf "Unexpected coupling string: %s." str

    let internal couplingString =
        function
        | AC -> "AC"
        | DC -> "DC"

    let internal parseOnOffState str =
        match upperCase str with
        | "0" | "OFF" -> Off
        | "1" | "ON"  -> On
        | str         -> failwithf "Unexpected on-off string: %s." str

    let internal onOffStateString =
        function
        | Off -> "OFF"
        | On  -> "ON"

    let internal parseAutoManualState str =
        match upperCase str with
        | "AUTO"           -> Auto
        | "MAN" | "MANUAL" -> Manual
        | _                -> failwithf "Unexpected auto-manual string: %s." str

    let internal autoManualStateString =
        function
        | Auto   -> "AUTO"
        | Manual -> "MANUAL"

    let internal parsePolarity str =
        match upperCase str with
        | "POS" | "POSITIVE" -> Positive
        | "NEG" | "NEGATIVE" -> Negative
        | _                  -> failwithf "Unexpected trigger polarity string: %s." str

    let internal polarityString =
        function
        | Positive -> "POS"
        | Negative -> "NEG"

    let internal functionShapeString =
        function
        | Sine     -> "SINE"
        | Triangle -> "TRI"
        | Square   -> "SQU"
        | Ramp _   -> "RAMP"
