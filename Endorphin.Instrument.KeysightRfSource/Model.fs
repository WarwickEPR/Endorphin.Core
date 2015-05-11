namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Endorphin.Core.NationalInstruments
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Instrument =
        type RfSource = internal RfSource of Visa.Instrument

        type DeviceId =
            { Manufacturer : string
              ModelNumber : string
              SerialNumber : string
              Version : string }

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

        type Error = { Code : int ; Message : string }
   
        let internal parseError (str : string) =
            let parts = str.Split [|','|]
            if Array.length parts <> 2 then failwithf "Unexpected error string: %s." str
        
            match parts.[0] with
            | ParseInteger code -> { Code = code ; Message = parts.[1] }
            | _                 -> failwithf "Unexpected error code string: %s." parts.[0]

        let internal errorString error = sprintf "%d: %s" error.Code error.Message

    [<AutoOpen>]
    module Quantities =
        type Amplitude = PowerInDbm of float<dBm>
        let internal parseAmplitudeInDbm (str : string) = PowerInDbm (float str * 1.0<dBm>)
        let internal amplitudeString (PowerInDbm amplitude) = sprintf "%e dBm" (float amplitude)

        type Frequency = FrequencyInHz of float<Hz>
        let internal parseFrequencyInHz (str : string) = FrequencyInHz (float str * 1.0<Hz>)
        let internal frequencyString (FrequencyInHz frequency) = sprintf "%e Hz" (float frequency)

        type Phase =
            | PhaseInRad of float<rad>
            | PhaseInDeg of float<deg>
        
        let internal parsePhaseInRad (str : string) = PhaseInRad (float str * 1.0<rad>)
    
        let internal phaseString =
            function
            | PhaseInRad phase -> sprintf "%e RAD" (float phase)
            | PhaseInDeg phase -> sprintf "%e DEG" (float phase)
    
        type Duration = DurationInSec of float<s>
        let internal parseDurationInSec (str : string) = DurationInSec (float str * 1.0<s>)
        let internal durationString (DurationInSec duration) = sprintf "%e s" (float duration)

        type Percentage = Percentage of float<pct>
        let internal parsePercentage (str : string) = Percentage (float str * 1.0<pct>)
        let internal percentageString (Percentage percentage) = sprintf "%e PCT" (float percentage)

        type DecibelRatio = DecibelRatio of float<dB>
        let internal parseDecibelRatio (str : string) = DecibelRatio (float str * 1.0<dB>)
        let internal decibelRatioString (DecibelRatio ratio) = sprintf "%e dB" (float ratio)
        
        type Impedance =
            | Impedance_50Ohm
            | Impedance_600Ohm
            | Impedance_1MOhm

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

    [<AutoOpen>]
    module General = 
        type Direction = Up | Down
    
        let internal parseDirection str =
            match upperCase str with
            | "UP"   -> Up
            | "DOWN" -> Down
            | _      -> failwithf "Unexpected direction string: %s." str

        let internal directionString =
            function
            | Up   -> "UP"
            | Down -> "DOWN"

        type Coupling = AC | DC

        let internal parseCoupling str =
            match upperCase str with
            | "AC" -> AC
            | "DC" -> DC
            | _    -> failwithf "Unexpected coupling string: %s." str

        let internal couplingString =
            function
            | AC -> "AC"
            | DC -> "DC"

        type OnOffState = On | Off

        let internal parseOnOffState str =
            match upperCase str with
            | "0" | "OFF" -> Off
            | "1" | "ON"  -> On
            | str         -> failwithf "Unexpected on-off string: %s." str

        let internal onOffStateString =
            function
            | Off -> "OFF"
            | On  -> "ON"

        type AutoManualState = Auto | Manual

        let internal parseAutoManualState str =
            match upperCase str with
            | "AUTO"           -> Auto
            | "MAN" | "MANUAL" -> Manual
            | _                -> failwithf "Unexpected auto-manual string: %s." str

        let internal autoManualStateString =
            function
            | Auto   -> "AUTO"
            | Manual -> "MANUAL"

        type Polarity = Positive | Negative

        let internal parsePolarity str =
            match upperCase str with
            | "POS" | "POSITIVE" -> Positive
            | "NEG" | "NEGATIVE" -> Negative
            | _                  -> failwithf "Unexpected trigger polarity string: %s." str

        let internal polarityString =
            function
            | Positive -> "POS"
            | Negative -> "NEG"

    [<AutoOpen>]
    module Triggering =
        type ExternalTriggerSource =
            | Trigger1
            | Trigger2
            | Pulse

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

        type InternalTriggerSource =
            | PulseVideo
            | PulseSync

        let internal parseInternalTriggerSource str =
            match upperCase str with
            | "PVID" | "PVIDEO" -> PulseVideo
            | "PSYN" | "PSYNC"  -> PulseSync
            | _                 -> failwithf "Unexpected internal trigger source string: %s" str

        let internal internalTriggerSourceString =
            function
            | PulseVideo -> "PVID"
            | PulseSync  -> "PSYN"

        type TriggerSourceType =
            | ImmediateType
            | TriggerKeyType
            | BusType
            | ExternalType
            | InternalType
            | TimerType

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

        type TriggerSource =
            | Immediate
            | TriggerKey
            | Bus
            | External of source : ExternalTriggerSource * polarity : Polarity
            | Internal of source : InternalTriggerSource
            | Timer of period : Duration

    [<AutoOpen>]
    module Modulation =
        type FunctionShapeType =
            | SineType
            | TriangleType
            | SquareType
            | RampType
        
        let internal parseFunctionShapeType str =
            match upperCase str with
            | "SINE"             -> SineType
            | "TRI" | "TRIANGLE" -> TriangleType
            | "SQU" | "SQUARE"   -> SquareType
            | "RAMP"             -> RampType
            | _                  -> failwithf "Unexpected function shape type string: %s" str

        let internal functionShapeString =
            function
            | SineType     -> "SINE"
            | TriangleType -> "TRI"
            | SquareType   -> "SQU"
            | RampType     -> "RAMP"

        type ModulationSource = FunctionGenerator | External1 | External2
    
        let internal parseModulationSource str =
            match upperCase str with
            | "FUNCTION1" | "FUNCTION" -> FunctionGenerator
            | "EXT" | "EXT1"           -> External1
            | "EXT2"                   -> External2
            | str                      -> failwithf "Unexpected modulation source string: %s." str

        let internal modulationSourceString =
            function
            | FunctionGenerator  -> "FUNCTION1"
            | External1 -> "EXT1"
            | External2 -> "EXT2"

        type AmplitudeModulationType = LinearType | ExponentialType

        let internal parseAmplitudeModulationType str =
            match upperCase str with
            | "LIN" | "LINEAR"      -> LinearType
            | "EXP" | "EXPONENTIAL" -> ExponentialType
            | _                     -> failwithf "Unexpected amplitude modulation type string: %s" str

        let internal amplitudeModulationTypeString =
            function
            | LinearType      -> "LIN"
            | ExponentialType -> "EXP"

        type ModulationPath = Path1 | Path2

        let internal modulationPathString =
            function
            | Path1 -> "1"
            | Path2 -> "2"

        type FunctionShape =
            | Sine
            | Triangle
            | Square
            | Ramp of polarity : Polarity

        type FunctionGeneratorWaveform =
            { Shape : FunctionShape
              Frequency : Frequency }

        type AmplitudeModulationDepth =
            | Linear of depth : Percentage
            | Exponential of depth : DecibelRatio

        type AmplitudeModulation =
            { Depth : AmplitudeModulationDepth
              Source : ModulationSource }

        type FrequencyModulation =
            { Deviation : Frequency
              Source : ModulationSource }

        type Modulation =
            { FrequencyModulation1 : FrequencyModulation option
              FrequencyModulation2 : FrequencyModulation option
              AmplitudeModulation1 : AmplitudeModulation option
              AmplitudeModulation2 : AmplitudeModulation option }

    [<AutoOpen>]
    module Waveforms =
        type WaveformId = WaveformId of string 

        let internal waveformIdString (WaveformId name) = name
        let internal parseWaveformId = WaveformId

    [<AutoOpen>]
    module Sweep =
        type SweepMode = Fixed | Swept

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

        type StepSpacing = LinearStep | LogarithmicStep

        let internal parseStepSpacing str =
            match upperCase str with
            | "LIN" | "LINEAR"      -> LinearStep
            | "LOG" | "LOGARITHMIC" -> LogarithmicStep
            | _                     -> failwithf "Unexpected step spacing string: %s." str
    
        let internal stepSpacingString =
            function
            | LinearStep      -> "LIN"
            | LogarithmicStep -> "LOG"

        type SweepType = List | Step

        let internal parseSweepType str =
            match upperCase str with
            | "LIST" -> List
            | "STEP" -> Step
            | _      -> failwithf "Unexpected sweep type string: %s." str

        let internal sweepTypeString =
            function
            | List -> "LIST"
            | Step -> "STEP"