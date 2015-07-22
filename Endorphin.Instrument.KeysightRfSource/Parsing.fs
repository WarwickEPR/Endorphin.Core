namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// functions to translate quantities used in the model
[<AutoOpen>]
module internal Parsing =
    let parseAmplitudeInDbm (str : string) = PowerInDbm (float str * 1.0<dBm>)
    let amplitudeString (PowerInDbm amplitude) = sprintf "%e dBm" (float amplitude)
    let parseFrequencyInHz (str : string) = FrequencyInHz (float str * 1.0<Hz>)
    let frequencyString (FrequencyInHz frequency) = sprintf "%e Hz" (float frequency)        
    let parsePhaseInRad (str : string) = PhaseInRad (float str * 1.0<rad>)
    let phaseString =
        function
        | PhaseInRad phase -> sprintf "%e RAD" (float phase)
        | PhaseInDeg phase -> sprintf "%e DEG" (float phase)
    let parseDurationInSec (str : string) = DurationInSec (float str * 1.0<s>)
    let durationString (DurationInSec duration) = sprintf "%e s" (float duration)
    let parsePercentage (str : string) = Percentage (float str * 1.0<pct>)
    let percentageString (Percentage percentage) = sprintf "%e PCT" (float percentage)
    let parseDecibelRatio (str : string) = DecibelRatio (float str * 1.0<dB>)
    let decibelRatioString (DecibelRatio ratio) = sprintf "%e dB" (float ratio)

    let parseImpedance =
        function
        | "50"      -> Impedance_50Ohm
        | "600"     -> Impedance_600Ohm
        | "1000000" -> Impedance_1MOhm
        | str       -> failwithf "Unexpected impedance string: %s." str

    let impedanceString =
        function
        | Impedance_50Ohm  -> "50"
        | Impedance_600Ohm -> "600"
        | Impedance_1MOhm  -> "1000000"
    
    let parseDirection str =
        match String.toUpper str with
        | "UP"   -> Up
        | "DOWN" -> Down
        | _      -> failwithf "Unexpected direction string: %s." str

    let directionString =
        function
        | Up   -> "UP"
        | Down -> "DOWN"

    let parseCoupling str =
        match String.toUpper str with
        | "AC" -> AC
        | "DC" -> DC
        | _    -> failwithf "Unexpected coupling string: %s." str

    let couplingString =
        function
        | AC -> "AC"
        | DC -> "DC"

    let parseOnOffState str =
        match String.toUpper str with
        | "0" | "OFF" -> Off
        | "1" | "ON"  -> On
        | str         -> failwithf "Unexpected on-off string: %s." str

    let onOffStateString =
        function
        | Off -> "OFF"
        | On  -> "ON"

    let parseAutoManualState str =
        match String.toUpper str with
        | "AUTO"           -> Auto
        | "MAN" | "MANUAL" -> Manual
        | _                -> failwithf "Unexpected auto-manual string: %s." str

    let autoManualStateString =
        function
        | Auto   -> "AUTO"
        | Manual -> "MANUAL"

    let parsePolarity str =
        match String.toUpper str with
        | "POS" | "POSITIVE" -> Positive
        | "NEG" | "NEGATIVE" -> Negative
        | _                  -> failwithf "Unexpected trigger polarity string: %s." str

    let polarityString =
        function
        | Positive -> "POS"
        | Negative -> "NEG"

    let functionShapeString =
        function
        | Sine     -> "SINE"
        | Triangle -> "TRI"
        | Square   -> "SQU"
        | Ramp _   -> "RAMP"

    /// Make an ASCII string out of an object's ToString() method
    let asciiString obj =
        obj.ToString() |> System.Text.Encoding.ASCII.GetBytes

    /// String of the folder location for waveforms
    let private waveformFolder = "WFM1:"
    /// String of the folder location for markers
    let private markerFolder   = "MKR1:"
    /// String of the folder location for headers
    let private headerFolder   = "HDR1:"
    /// String of the folder location for sequences
    let private sequenceFolder = "SEQ:"

    /// Build up a full file name string for storing a file
    let private fileNameString folder name = String.concat "" ["\""; folder; name; "\""]

    /// Total filename string for a waveform file
    let waveformFileString name = fileNameString waveformFolder name
    /// Total filename string for a markers file
    let markerFileString name = fileNameString markerFolder name
    /// Total filename string for a header file
    let headerFileString name = fileNameString headerFolder name
    /// Total filename string for a sequence file
    let sequenceFileString name = fileNameString sequenceFolder name

    /// Get the string representation of a SegmentId
    let extractSegmentId (SegmentId id) = id
    /// Get the string representation of a SequenceId
    let extractSequenceId (SequenceId id) = id
    /// Get the string representation of a StoredSegment
    let extractStoredSegmentId (StoredSegment id) = id |> extractSegmentId
    /// Get the string representation of a StoredSequence
    let extractStoredSequenceId (StoredSequence id) = id |> extractSequenceId

    /// Get the full file name of a waveform file from the short name stored in the
    /// StoredSegment.  For example, if the StoredSegment name is "test", then this
    /// function returns "\"WFM1:test\""B
    let storedSegmentFilename (segment : StoredSegment) =
        segment
        |> extractStoredSegmentId
        |> waveformFileString

    /// Get the full file name of a sequence file from the short name stored in the
    /// StoredSequence.  For example, if the StoredSequence name is "test", then this
    /// function returns "\"SEQ:test\""B
    let storedSequenceFilename (sequence : StoredSequence) =
        sequence
        |> extractStoredSequenceId
        |> sequenceFileString