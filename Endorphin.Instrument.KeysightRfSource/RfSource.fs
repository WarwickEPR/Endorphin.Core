namespace Endorphin.Instrument.Keysight

open System
open log4net

open Endorphin.Core
open Endorphin.Core.Units

module RfSource =

    type VI = NationalInstruments.VisaInstrument
    let log = LogManager.GetLogger (sprintf "Keysight RF Source")

    type Power = private PowerInDbm of float<dBm>
    let powerInDbm power = PowerInDbm (power * 1.0<dBm>)
    let private parsePowerInDbm str = powerInDbm <| float str 

    type Frequency = private FrequencyInHz of float<Hz>
    let frequencyInHz frequency = FrequencyInHz (frequency * 1.0<Hz>)
    let private parseFrequencyInHz str = frequencyInHz <| float str

    [<Measure>] type pct
    let floatToPercentage (f : float) = f * 100.0<pct>
    let percentageToFloat (p : float<pct>) = p / 100.0<pct>

    let private constrain (lower : float<_>) (upper : float<_>) (x : float<_>) =
        if x < lower then lower
        elif x > upper then upper
        else x

    type SweepMode =
        private
        | FIXED
        | LIST

    let private parseSweepMode str =
        match str with
          | "CW" | "FIX" | "FIXED" -> SweepMode.FIXED
          | "LIST" -> SweepMode.LIST

    let private sweepModeString (sm : SweepMode) =
        match sm with
           | FIXED -> "FIX"
           | LIST -> "LIST"


    type ModulationSource =
        | FUNCTION
        | FUNCTION2
        | SWEEP
        | DUAL
        | NOISE
        | NOISE2
        | EXT
        | EXT2

    let private parseModulationSource str =
        match str with
            | "FUNCTION1" | "FUNCTION" -> ModulationSource.FUNCTION
            | "FUNCTION2"              -> ModulationSource.FUNCTION2
            | "SWEEP"                  -> ModulationSource.SWEEP
            | "DUAL"                   -> ModulationSource.DUAL
            | "NOISE" |"NOISE1"        -> ModulationSource.NOISE
            | "NOISE2"                 -> ModulationSource.NOISE2
            | "EXT" | "EXT1"           -> ModulationSource.EXT
            | "EXT2"                   -> ModulationSource.EXT2
                
    let private modulationSourceString(s : ModulationSource) =
        match s with
            | FUNCTION   -> "FUNCTION"
            | FUNCTION2  -> "FUNCTION2"
            | SWEEP      -> "SWEEP"
            | DUAL       -> "DUAL"
            | NOISE      -> "NOISE"
            | NOISE2     -> "NOISE2"
            | EXT        -> "EXT"
            | EXT2       -> "EXT2"

    type FunctionShape = SINE|TRIANGLE|SQUARE|RAMP|PULSE
    let private parseFunctionShape str =
        match str with
            | "SINE"              -> FunctionShape.SINE
            | "TRIANGLE" | "TRI"  -> FunctionShape.TRIANGLE
            | "SQUARE" | "SQU"    -> FunctionShape.SQUARE
            | "RAMP" | "RAMP"     -> FunctionShape.RAMP
            | "PULSE" | "PULS"    -> FunctionShape.PULSE
    let private functionShapeString (s:FunctionShape) =
        match s with
            | SINE       -> "SINE"
            | TRIANGLE   -> "TRI"
            | SQUARE     -> "SQU"
            | RAMP       -> "RAMP"
            | PULSE      -> "PULSE"

    let private parseBoolean str =
        // TODO: Handle error case with Result<>
        match str with
            | "0" | "OFF" -> false
            | "1" | "ON" -> true
            | _ -> false

    type RfInstrument =
        /// close connection to device, once pending commands complete
        abstract member close : unit -> Async<unit>
        /// get *IDN? identification string
        abstract member identify : unit -> Async<string>
        /// turn on RF output
        abstract member switchRfOn : unit -> unit
        /// turn off RF output
        abstract member switchRfOff : unit -> unit
        /// test if RF output is on
        abstract member isRfOn : unit -> Async<bool>
        /// test if RF output is off
        abstract member isRfOff : unit -> Async<bool>
        /// set output power
        abstract member setPower : Power -> unit
        /// get output power
        abstract member getPower : unit -> Async<Power>
        /// set output frequency
        abstract member setFrequency : Frequency -> unit
        /// get output frequency
        abstract member getFrequency : unit -> Async<Frequency>
        /// turn output modulation on

        abstract member switchModulationOn : unit -> unit
        /// turn output modulation off
        abstract member switchModulationOff : unit -> unit
        /// test if output modulation is on
        abstract member isModulationOn : unit -> Async<bool>
        /// test if output modulation is off
        abstract member isModulationOff : unit -> Async<bool>
        /// turn amplitude modulation on
        abstract member switchAmplitudeModulationOn : unit -> unit
        /// turn amplitude modulation off
        abstract member switchAmplitudeModulationOff : unit -> unit
        /// test if amplitude modulation if on
        abstract member isAmplitudeModulationOn : unit -> Async<bool>
        /// test if amplitude modulation if off
        abstract member isAmplitudeModulationOff : unit -> Async<bool>
        /// turn frequency modulation on
        abstract member switchFrequencyModulationOn : unit -> unit
        /// turn frequency modulation off
        abstract member switchFrequencyModulationOff : unit -> unit
        /// test if frequency modulation is on
        abstract member isFrequencyModulationOn : unit -> Async<bool>
        /// test if frequency modulation is off
        abstract member isFrequencyModulationOff : unit -> Async<bool>
        // set AM source oscillator
        abstract member setAmplitudeModulationSource : ModulationSource -> unit
        // get AM source oscillator
        abstract member getAmplitudeModulationSource : unit -> Async<ModulationSource>
        // set FM source oscillator
        abstract member setFrequencyModulationSource : ModulationSource -> unit
        // get FM source oscillator
        abstract member getFrequencyModulationSource : unit -> Async<ModulationSource>
        /// Set frequency modulation deviation
        abstract member setFrequencyDeviation : Frequency -> unit
        /// Get frequency modulation deviation
        abstract member getFrequencyDeviation : unit -> Async<Frequency>
        abstract member setAmplitudeDepth : float<pct> -> unit
        /// get depth of amplitude as a percentage
        abstract member getAmplitudeDepth : unit -> Async<float<pct>>
        /// set depth of amplitude in decibels 0-40dB
        abstract member setAmplitudeDepthDb : float<dB> -> unit
        /// get depth of amplitude as decibels
        abstract member getAmplitudeDepthDb : unit -> Async<float<dB>>
        /// set frequency of amplitude modulation
        abstract member setAmplitudeModulationFrequency : Frequency -> unit
        /// get frequency of amplitude modulation
        abstract member getAmplitudeModulationFrequency : unit -> Async<Frequency>
        /// set frequency of amplitude modulation
        abstract member setFrequencyModulationFrequency : Frequency -> unit
        /// get frequency of amplitude modulation
        abstract member getFrequencyModulationFrequency : unit -> Async<Frequency>
        /// set shape of amplitude modulation
        abstract member setAmplitudeModulationShape : FunctionShape -> unit
        /// get shape of amplitude modulation
        abstract member getAmplitudeModulationShape : unit -> Async<FunctionShape>
        /// set shape of frequency modulation
        abstract member setFrequencyModulationShape : FunctionShape -> unit
        /// get shape of frequency modulation
        abstract member getFrequencyModulationShape : unit -> Async<FunctionShape>

        // set start frequency for sweeps
        abstract member setStartFrequency : Frequency -> unit
        // get start frequency for sweeps
        abstract member getStartFrequency : unit -> Async<Frequency>
        // set end frequency for sweeps
        abstract member setEndFrequency : Frequency -> unit
        // get end frequency for sweeps
        abstract member getEndFrequency : unit -> Async<Frequency>
        // set amplitude sweep mode (fixed/list). Use list for sweep
        abstract member setAmplitudeSweepMode : SweepMode -> unit
        // get amplitude sweep mode (fixed/list). List is used for sweep
        abstract member getAmplitudeSweepMode : unit -> Async<SweepMode>
        // set frequency sweep mode (fixed/list). Use list for sweep
        abstract member setFrequencySweepMode : SweepMode -> unit
        // get frequency sweep mode (fixed/list). List is used for sweep
        abstract member getFrequencySweepMode : unit -> Async<SweepMode>
        /// abort current sweep (in continuous sweep, starts new sweep)
        abstract member abort : unit -> unit
        /// set continous sweep mode (as opposed to single sweep)
        abstract member setContinuousSweep : bool -> unit
        /// get continuous sweep mode
        abstract member getContinuousSweep : unit -> Async<bool>
        /// initiate sweep (immediate or arm for trigger)
        abstract member initiateSweep : unit -> unit
        /// set depth of amplitude as a percentage (0-100%)

    let private setFloat (rfSource : VI) (key : string) (v : float<_>) =
        float v |> sprintf "%s %e" key |> rfSource.Write

    let private getFloat (rfSource : VI) (key : string) = async {
        let! response = key |> rfSource.Query 
        return float response }

    let private getFloatWithUnit (rfSource : VI) (unit : float<_>) (key : string) = async {
        let! response = key |> rfSource.Query 
        return float response * unit}


    // Check if parameter is on
    let private isOn (rfSource : VI ) (key : string) = async {
        let! response = key |> rfSource.Query
        return parseBoolean response }

    // Check if parameter is off
    let private isOff (rfSource : VI) (key : string) = async {
        let! onState = key |> isOn rfSource
        return not onState }

    let private boolToString (state: bool) =
        match state with
            | true -> "ON"
            | false -> "OFF"

    let private setBoolean (rfSource: VI) (key: string) (value: bool) =
        boolToString value |> sprintf "%s %s" key |> rfSource.Write

    let private setPercentage (rfSource: VI) (key: string) (value: float<pct>) =
        float value |> sprintf "%s %ePCT" key |> rfSource.Write

    // Set power in dBm
    // TODO: Add other units
    let private setPower (rfSource : VI) (key : string) (PowerInDbm power : Power) =
        sprintf "%s %edBm" key (float power) |> rfSource.Write

    // Get current power setting
    // TODO: Handle other units?
    let private getPower (rfSource : VI) (key : string) = async {
        // Leaves units in original state
        let! powerunit = rfSource.Query ":UNIT:POW?"
        let! response = sprintf ":UNIT:POW DBM; %s; :UNIT:POW %s" key powerunit |> rfSource.Query 
        return parsePowerInDbm response }

    // Set frequency in Hz
    // TODO: Add other units
    let private setFrequency (rfSource : VI) (key : string) (FrequencyInHz frequency : Frequency) =
        sprintf "%s %eHz" key (float frequency) |> rfSource.Write

    // Get current frequency setting
    // TODO: Handle other units/errors
    let private getFrequency (rfSource : VI) (key : string) = async {
        let! response = key |> rfSource.Query 
        return parseFrequencyInHz response }

    let private setSweepMode (rfSource : VI) (key : string) (sm : SweepMode) =
        let modestr = sweepModeString sm
        sprintf "%s %s" key modestr |> rfSource.Write

    let private getSweepMode (rfSource : VI) (key : string) = async {
        let! response = key |> rfSource.Query
        // TODO: cover error case
        // LIST seems to cover sweep also
        return parseSweepMode response }

    let private setModulationSource (rfSource : VI) (key : string) (s : ModulationSource) =
        let str = modulationSourceString s
        sprintf "%s %s" key str |> rfSource.Write

    let private getModulationSource (rfSource : VI) (key : string) = async {
        let! response = key |> rfSource.Query
        return parseModulationSource response }

    let private setModulationShape (rfSource : VI) (key : string) (s : FunctionShape) =
        let str = functionShapeString s
        sprintf "%s %s" key str |> rfSource.Write

    let private getModulationShape (rfSource : VI) (key : string) = async {
        let! response = key |> rfSource.Query
        return parseFunctionShape response }

    let visaInstrument visaAddress =
        NationalInstruments.openInstrument visaAddress

    /// Create a handle to a Keysight RF source
    // TODO Handle failure to connect gracefully
    // Happens even when connected on occasion
    let openRfInstrument (visaInstrument : NationalInstruments.VisaInstrument) =
        let vi = visaInstrument
        { new RfInstrument with
            member rfi.close() = vi.Close()
            /// Get identifier string
            member rfi.identify() = vi.Query <| "*IDN?"
            member rfi.abort() = vi.Write <| "ABORT"
            member rfi.switchRfOn() = vi.Write <| ":OUTPUT:STATE ON"
            member rfi.switchRfOff() = vi.Write <| ":OUTPUT:STATE OFF"
            member rfi.isRfOn() = isOn vi <| ":OUTPUT:STATE?"
            member rfi.isRfOff() = isOff vi <| ":OUTPUT:STATE?"
            member rfi.setPower value = setPower vi <| ":POW" <| value
            member rfi.getPower() = getPower vi <| ":POW?"
            member rfi.setFrequency value = setFrequency vi <| ":FREQ" <| value
            member rfi.getFrequency() = getFrequency vi <| ":FREQ?"
            member rfi.switchModulationOn() = vi.Write <| ":OUTPUT:MOD ON"
            member rfi.switchModulationOff() = vi.Write <| ":OUTPUT:MOD OFF"
            member rfi.isModulationOn() = isOn vi <| ":OUTPUT:MOD?"
            member rfi.isModulationOff() = isOff vi <| ":OUTPUT:MOD?"
            member rfi.switchAmplitudeModulationOn() = vi.Write <| ":AM:STATE ON"
            member rfi.switchAmplitudeModulationOff() = vi.Write <| ":AM:STATE OFF"
            member rfi.switchFrequencyModulationOn() = vi.Write <| ":FM:STATE ON"
            member rfi.switchFrequencyModulationOff() = vi.Write <| ":FM:STATE OFF"
            member rfi.isAmplitudeModulationOn() = isOn vi <| ":AM:STATE?"
            member rfi.isAmplitudeModulationOff() = isOff vi <| ":AM:STATE?"
            member rfi.isFrequencyModulationOn() = isOn vi <| ":FM:STATE?"
            member rfi.isFrequencyModulationOff() = isOff vi <| ":FM:STATE?"
            member rfi.setAmplitudeDepth value = setPercentage vi <| ":AM:DEPTH" <| constrain 0.0<pct> 100.0<pct> value
            member rfi.getAmplitudeDepth() = getFloatWithUnit vi 100.0<pct> <| ":AM:DEPTH?"
            member rfi.setAmplitudeDepthDb value = setFloat vi <| ":AM:DEPTH:EXP" <| constrain 0.0<dB> 40.0<dB> value
            member rfi.getAmplitudeDepthDb() = getFloatWithUnit vi 1.0<dB> <| ":AM:DEPTH:EXP?"
            member rfi.setAmplitudeModulationSource value = setModulationSource vi <| ":AM:SOURCE" <| value
            member rfi.getAmplitudeModulationSource() = getModulationSource vi <| ":AM:SOURCE?"
            member rfi.setFrequencyModulationSource value = setModulationSource vi <| ":FM:SOURCE" <| value
            member rfi.getFrequencyModulationSource() = getModulationSource vi <| ":FM:SOURCE?"
            member rfi.setAmplitudeModulationFrequency value = setFrequency vi <| ":AM:INT:FUNC:FREQ" <| value
            member rfi.getAmplitudeModulationFrequency() = getFrequency vi <| ":AM:INT:FUNC:FREQ?"
            member rfi.setFrequencyModulationFrequency value = setFrequency vi <| ":FM:INT:FUNC:FREQ" <| value
            member rfi.getFrequencyModulationFrequency() = getFrequency vi <| ":FM:INT:FUNC:FREQ?"
            member rfi.setAmplitudeModulationShape value = setModulationShape vi <| ":AM:INT:FUNC:SHAPE" <| value
            member rfi.getAmplitudeModulationShape() = getModulationShape vi <| ":AM:INT:FUNC:SHAPE?"
            member rfi.setFrequencyModulationShape value = setModulationShape vi <| ":FM:INT:FUNC:SHAPE" <| value
            member rfi.getFrequencyModulationShape() = getModulationShape vi <| ":FM:INT:FUNC:SHAPE?"
            member rfi.setStartFrequency value = setFrequency vi <| ":FREQ:START" <| value
            member rfi.getStartFrequency() = getFrequency vi <| ":FREQ:START?"
            member rfi.setEndFrequency value = setFrequency vi <| ":FREQ:END" <| value 
            member rfi.getEndFrequency() = getFrequency vi <| ":FREQ:END?"
            member rfi.setAmplitudeSweepMode value = setSweepMode vi <| ":MODE" <| value
            member rfi.getAmplitudeSweepMode() = getSweepMode vi <| ":MODE?"
            member rfi.setFrequencySweepMode value = setSweepMode vi <| ":FREQ:MODE" <| value
            member rfi.getFrequencySweepMode() = getSweepMode vi <| ":FREQ:MODE?"
            member rfi.setContinuousSweep value = setBoolean vi <| ":INIT:CONT" <| value
            member rfi.getContinuousSweep() = isOn vi <| ":INIT:CONT?"
            member rfi.initiateSweep() = vi.Write <| ":INIT"
            member rfi.setFrequencyDeviation value = setFrequency vi <| ":FM:DEV" <| value
            member rfi.getFrequencyDeviation() =  getFrequency vi <| ":FM:DEV?"
        }

// Commands used in Python implementation
//LIST:DWEL:TYPE
//LIST:FREQ
//LIST:TYPE
//SWE:DWEL
//SWE:GEN
//SWE:POIN
//SWE:TIME
//SWE:TIME:AUTO


// LIST:TRIGGER:SOURCE