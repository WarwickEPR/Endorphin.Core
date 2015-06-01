namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// Functions to tranlate between values in the model and the strings
// used on Keysight devices.

module Translate =

    [<AutoOpen>]
    module Instrument =
        
        let internal tryParseDeviceId (str : string) =
            let trimWhiteSpace (str : string) = str.TrimStart([|' '|]).TrimEnd([|' '|])
            let parts = str.Split [|','|]
            if Array.length parts <> 4 then fail <| sprintf "Unexpected device ID string: %s." str
            else succeed <| { Manufacturer = parts.[0] |> trimWhiteSpace
                              ModelNumber = parts.[1] |> trimWhiteSpace
                              SerialNumber = parts.[2] |> trimWhiteSpace
                              Version = parts.[3] |> trimWhiteSpace }
          
        let internal parseDeviceId (str : string) =
            match tryParseDeviceId str with
            | Success id    -> id
            | Failure error -> failwith error

        let internal parseError (str : string) =
            let parts = str.Split [|','|]
            if Array.length parts <> 2 then failwithf "Unexpected error string: %s." str
        
            match parts.[0] with
            | ParseInteger code -> { Code = code ; Message = parts.[1] }
            | _                 -> failwithf "Unexpected error code string: %s." parts.[0]

        let internal errorString error = sprintf "%d: %s" error.Code error.Message

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

        type internal FunctionShapeType =
            | SineType
            | TriangleType
            | SquareType
            | RampType

        let internal functionShapeString =
            function
            | Sine     -> "SINE"
            | Triangle -> "TRI"
            | Square   -> "SQU"
            | Ramp _   -> "RAMP"
        
        let internal parseFunctionShapeType str =
            match upperCase str with
            | "SINE"             -> SineType
            | "TRI" | "TRIANGLE" -> TriangleType
            | "SQU" | "SQUARE"   -> SquareType
            | "RAMP"             -> RampType
            | _                  -> failwithf "Unexpected function shape type string: %s" str

    [<AutoOpen>]
    module Triggering =

        let internal parseExternalTriggerSource str =
            match upperCase str with
            | "TRIG1" | "TRIGGER1" -> Trigger1
            | "TRIG2" | "TRIGGER2" -> Trigger2
            | "PULS" | "PULSE"     -> Pulse
            | _                    -> failwithf "Unexpected external trigger source string: %s" str

        let internal externalTriggerSourceString =
            function
            | Trigger1 -> "TRIG1"
            | Trigger2 -> "TRIG2"
            | Pulse    -> "PULS"

  
        let internal parseInternalTriggerSource str =
            match upperCase str with
            | "PVID" | "PVIDEO" -> PulseVideo
            | "PSYN" | "PSYNC"  -> PulseSync
            | _                 -> failwithf "Unexpected internal trigger source string: %s" str

        let internal internalTriggerSourceString =
            function
            | PulseVideo -> "PVID"
            | PulseSync  -> "PSYN"

        let internal parseTriggerSourceType str =
            match upperCase str with
            | "IMM" | "IMMEDIATE" -> ImmediateType
            | "KEY"               -> TriggerKeyType
            | "BUS"               -> BusType
            | "EXT" | "EXTERNAL"  -> ExternalType
            | "INT" | "INTERNAL"  -> InternalType
            | "TIM" | "TIMER"     -> TimerType
            | _                   -> failwithf "Unexpected trigger source type string: %s." str

        let internal triggerSourceTypeString =
            function
            | ImmediateType  -> "IMM"
            | TriggerKeyType -> "KEY"
            | BusType        -> "BUS"
            | ExternalType   -> "EXT"
            | InternalType   -> "INT"
            | TimerType      -> "TIM"

        let triggerTypePrefix = function
            | StepTrigger -> ""
            | ListTrigger -> ":LIST"

    [<AutoOpen>]
    module Modulation =

        let ``AM Path String`` =
            function
            | AM1 -> "AM1"
            | AM2 -> "AM2"

        let ``FM Path String`` =
            function
            | FM1 -> "FM1"
            | FM2 -> "FM2"

        // Modulation Settings 
        type internal DepthType = LinearType | ExponentialType

        let internal parseDepthType str =
            match upperCase str with
            | "LIN" | "LINEAR" -> LinearType
            | "EXP" | "EXPONENTIAL" -> ExponentialType
            | str -> failwithf "Unexpected depth type: %s" str

        let internal depthTypeString =
            function
            | Linear _ -> "LIN"
            | Exponential _ -> "EXP"

        let sourceString =
            function
            | ExternalPort EXT1 -> "EXT1"
            | ExternalPort EXT2 -> "EXT2"
            | InternalGenerator Function1 -> "FUNCTION1"

        let parseSource str =
            match upperCase str with
            | "EXT1" -> ExternalPort EXT1
            | "EXT2" -> ExternalPort EXT2
            | "FUNCTION1" -> InternalGenerator Function1
            | str -> failwithf "Unexpected source: %s" str

    module Sweep =
    
        let internal parseSweepMode str =
            match upperCase str with
            | "CW"
            | "FIX"
            | "FIXED" -> Fixed
            | "LIST"  -> Swept
            | str     -> failwithf "Unexpected sweep mode string: %s." str

        let internal sweepModeString =
            function
            | Fixed -> "FIX"
            | Swept -> "LIST"

        let internal parseStepSpacing str =
            match upperCase str with
            | "LIN" | "LINEAR"      -> LinearStepSpacing
            | "LOG" | "LOGARITHMIC" -> LogarithmicStepSpacing
            | _                     -> failwithf "Unexpected step spacing string: %s." str
    
        let internal stepSpacingString =
            function
            | LinearStepSpacing      -> "LIN"
            | LogarithmicStepSpacing -> "LOG"

        let internal parseSweepType str =
            match upperCase str with
            | "LIST" -> List
            | "STEP" -> Step
            | _      -> failwithf "Unexpected sweep type string: %s." str

        let internal sweepTypeString =
            function
            | List -> "LIST"
            | Step -> "STEP"

