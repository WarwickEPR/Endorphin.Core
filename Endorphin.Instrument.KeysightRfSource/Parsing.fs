namespace Endorphin.Instrument.Keysight

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Functions to translate between internal and machine representations.
[<AutoOpen>]
module internal Parsing =
    /// Convert a string containing a number into a power in dBm.
    let parseAmplitudeInDbm (str : string) = PowerInDbm (float str * 1.0<dBm>)
    /// Convert an amplitude in dBm into a string representation.
    let amplitudeString (PowerInDbm amplitude) = sprintf "%e dBm" (float amplitude)

    /// Convert a string containing a number into a frequence in Hz.
    let parseFrequencyInHz (str : string) = FrequencyInHz (float str * 1.0<Hz>)
    /// Convert a frequency in Hz into a string representation.
    let frequencyString (FrequencyInHz frequency) = sprintf "%e Hz" (float frequency)        

    /// Convert a string containing a number into a phase in radians.
    let parsePhaseInRad (str : string) = PhaseInRad (float str * 1.0<rad>)
    /// Convert a phase into a string representation of that number and unit.
    let phaseString = function
        | PhaseInRad phase -> sprintf "%e RAD" (float phase)
        | PhaseInDeg phase -> sprintf "%e DEG" (float phase)

    /// Convert a string containing a number into a duration in seconds.
    let parseDurationInSec (str : string) = DurationInSec (float str * 1.0<s>)
    /// Convert a duration in seconds into a string representation.
    let durationString (DurationInSec duration) = sprintf "%e s" (float duration)

    /// Convert a string containing a number into a percentage.
    let parsePercentage (str : string) = Percentage (float str * 1.0<pct>)
    /// Convert a percentage into a string representation.
    let percentageString (Percentage percentage) = sprintf "%e PCT" (float percentage)

    /// Convert a string containing a number into a decibel ratio in dB.
    let parseDecibelRatio (str : string) = DecibelRatio (float str * 1.0<dB>)
    /// Convert a decibel ratio into a string representation.
    let decibelRatioString (DecibelRatio ratio) = sprintf "%e dB" (float ratio)

    /// Convert a machine representation of an impedance into an internal representation.
    let parseImpedance = function
        | "50"      -> Impedance_50Ohm
        | "600"     -> Impedance_600Ohm
        | "1000000" -> Impedance_1MOhm
        | str       -> failwithf "Unexpected impedance string: %s." str

    /// Convert an internal representation of an impedance into a machine representation.
    let impedanceString = function
        | Impedance_50Ohm  -> "50"
        | Impedance_600Ohm -> "600"
        | Impedance_1MOhm  -> "1000000"
    
    /// Convert a machine representation of a direction into an internal representation.
    let parseDirection str =
        match String.toUpper str with
        | "UP"   -> Up
        | "DOWN" -> Down
        | _      -> failwithf "Unexpected direction string: %s." str

    /// Convert an internal representation of a direction into a machine representation.
    let directionString = function
        | Up   -> "UP"
        | Down -> "DOWN"

    /// Convert a machine representation of a coupling into an internal representation.
    let parseCoupling str =
        match String.toUpper str with
        | "AC" -> AC
        | "DC" -> DC
        | _    -> failwithf "Unexpected coupling string: %s." str

    /// Convert an internal representation of a coupling into a machine representation.
    let couplingString = function
        | AC -> "AC"
        | DC -> "DC"

    /// Convert a machine representation of an on/off state into an internal representation.
    let parseOnOffState str =
        match String.toUpper str with
        | "0" | "OFF" -> Off
        | "1" | "ON"  -> On
        | str         -> failwithf "Unexpected on-off string: %s." str

    /// Convert an internal representation of an on/off state into a machine representation.
    let onOffStateString = function
        | Off -> "OFF"
        | On  -> "ON"

    /// Convert a machine representation of an automatic/manual state into an
    /// internal representation.
    let parseAutoManualState str =
        match String.toUpper str with
        | "AUTO"           -> Auto
        | "MAN" | "MANUAL" -> Manual
        | _                -> failwithf "Unexpected auto-manual string: %s." str

    /// Convert an internal representation of an automatic/manual state into a
    /// machine representation.
    let autoManualStateString = function
        | Auto   -> "AUTO"
        | Manual -> "MANUAL"

    /// Convert a machine representation of a polarity into an internal representation.
    let parsePolarity str =
        match String.toUpper str with
        | "POS" | "POSITIVE" -> Positive
        | "NEG" | "NEGATIVE" -> Negative
        | _                  -> failwithf "Unexpected trigger polarity string: %s." str

    /// Convert an internal representation of a polarity into a machine representation.
    let polarityString = function
        | Positive -> "POS"
        | Negative -> "NEG"

    /// Convert an internal representation of a function shape into a machine
    /// representation.
    let functionShapeString = function
        | Sine     -> "SINE"
        | Triangle -> "TRI"
        | Square   -> "SQU"
        | Ramp _   -> "RAMP"

    /// Convert an internal representation of a low/high state into a machine
    /// representation.
    let lowHighStateString = function
        | Low -> "LOW"
        | High -> "HIGH"

    /// Convert a machine representation of a low/high state into an internal
    /// representation.
    let parseLowHighState str =
        match String.toUpper str with
        | "LOW" -> Low
        | "HIGH" -> High
        | _ -> failwithf "Unexpected low/high state string: %s" str

    /// Make an ASCII string out of an object's ToString() method.
    let asciiString obj =
        obj.ToString() |> System.Text.Encoding.ASCII.GetBytes

    /// String of the folder location for waveforms.
    let waveformFolder = Some "WFM1:"
    /// String of the folder location for markers.
    let markerFolder   = Some "MKR1:"
    /// String of the folder location for sequences.
    let sequenceFolder = Some "SEQ:"
    /// String of the folder location for list files.
    let listFolder = None

    /// Build up a full file name string for storing a file.
    let fileNameString folder name =
        match folder with
        | Some f -> String.concat "" ["\""; f; name; "\""]
        | None -> String.concat "" ["\""; name; "\""]

    /// Total filename string for a waveform file.
    let waveformFileString name = fileNameString waveformFolder name
    /// Total filename string for a markers file.
    let markerFileString name = fileNameString markerFolder name
    /// Total filename string for a sequence file.
    let sequenceFileString name = fileNameString sequenceFolder name

    /// Get the string representation of a SegmentId.
    let extractSegmentId (SegmentId id) = id
    /// Get the string representation of a SequenceId.
    let extractSequenceId (SequenceId id) = id

    /// Get the string representations of the relevant folders and the ids of a StoredWaveform.
    let extractStoredWaveformFolderAndId = function
        | StoredSegment  s -> (waveformFolder, extractSegmentId s)
        | StoredSequence s -> (sequenceFolder, extractSequenceId s)

    /// Get the string representation of a StoredWaveform.
    let extractStoredWaveformId = function
        | StoredSegment  s -> extractSegmentId s
        | StoredSequence s -> extractSequenceId s

    /// Get the full file name of a waveform file from the short name stored in the
    /// StoredWaveform.  For example, if it is a segment, and the name is "test", then this
    /// function returns "\"WFM1:test\""B.
    let storedWaveformFilename = function
        | StoredSegment  s -> waveformFileString <| extractSegmentId s
        | StoredSequence s -> sequenceFileString <| extractSequenceId s
