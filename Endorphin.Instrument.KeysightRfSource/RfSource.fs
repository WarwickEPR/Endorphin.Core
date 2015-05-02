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

    type FrequencyMode =
        private
        | FIX
        | LIST
    let private parseFrequencyMode str =
        match str with
          | "CW" | "FIX" -> FIX
          | "LIST" -> LIST

    type PowerMode =
        private
        | FIX
        | LIST
    let private parseFrequencyMode str =
        match str with
          | "CW" | "FIX" -> FIX
          | "LIST" -> LIST

    let private parseBoolean str =
        // TODO: Handle error case with Result<>
        match str with
            | "0" | "OFF" -> false
            | "1" | "ON" -> true
            | _ -> false

    type RfInstrument =
        abstract member close : unit -> Async<unit>
        abstract member identify : unit -> Async<string>
        abstract member switchRfOn : unit -> unit
        abstract member switchRfOff : unit -> unit
        abstract member isRfOn : unit -> Async<bool>
        abstract member isRfOff : unit -> Async<bool>
        abstract member switchModulationOn : unit -> unit
        abstract member switchModulationOff : unit -> unit
        abstract member isModulationOn : unit -> Async<bool>
        abstract member isModulationOff : unit -> Async<bool>
        abstract member abort : unit -> unit
        abstract member setPower : Power -> unit
        abstract member getPower : unit -> Async<Power>
        abstract member setFrequency : Frequency -> unit
        abstract member getFrequency : unit -> Async<Frequency>
        abstract member setStartFrequency : Frequency -> unit
        abstract member getStartFrequency : unit -> Async<Frequency>
        abstract member setEndFrequency : Frequency -> unit
        abstract member getEndFrequency : unit -> Async<Frequency>
        abstract member setFrequencyMode : FrequencyMode -> unit
        abstract member getFrequencyMode : unit -> Async<FrequencyMode>

    let private identify (rfSource : VI) =
        rfSource.Query "*IDN?"

    let private abort (rfSource : VI) =
        rfSource.Write("ABORT")

    // Turn RF output on
    let private switchRfOn  (rfSource : VI) = ":OUTPUT:STATE ON"  |> rfSource.Write
    // Turn RF output off
    let private switchRfOff (rfSource : VI) = ":OUTPUT:STATE OFF" |> rfSource.Write
    // Check if RF output is on
    let private isRfOn (rfSource : VI ) = async {
        let! response = ":OUTPUT:STATE?" |> rfSource.Query
        return parseBoolean response }
    // Check if RF output is off
    let private isRfOff (rfSource : VI) = async {
        let! onState = isRfOn rfSource
        return not onState }

    // Turn RF output on
    let private switchModulationOn  (rfSource : VI) = ":OUTPUT:STATE ON"  |> rfSource.Write
    // Turn RF output off
    let private switchModulationOff (rfSource : VI) = ":OUTPUT:STATE OFF" |> rfSource.Write
    // Check if RF output is on
    let private isModulationOn (rfSource : VI ) = async {
        let! response = ":OUTPUT:STATE?" |> rfSource.Query
        return parseBoolean response }
    // Check if RF output is off
    let private isModulationOff (rfSource : VI) = async {
        let! onState = isModulationOn rfSource
        return not onState }

    // Set power in dB
    // TODO: Add other units
    let private setPower (rfSource : VI) (PowerInDbm power : Power) =
        sprintf ":POW %edBm" (float power) |> rfSource.Write

    // Get current power setting
    // TODO: Handle other units?
    let private getPower (rfSource : VI) = async {
        // Leaves units in original state
        let! powerunit = rfSource.Query ":UNIT:POW?"
        let! response = sprintf ":UNIT:POW DBM; :POW?; :UNIT:POW %s" powerunit |> rfSource.Query 
        return parsePowerInDbm response }

    // Set frequency in Hz
    // TODO: Add other units
    let private setFrequency (rfSource : VI) (FrequencyInHz frequency : Frequency) =
        sprintf ":FREQ %eHz" (float frequency) |> rfSource.Write

    // Get current frequency setting
    // TODO: Handle other units/errors
    let private getFrequency (rfSource : VI) = async {
        let! response = ":FREQ?" |> rfSource.Query 
        return parseFrequencyInHz <| response }

    // Set start frequency in Hz
    // TODO: Add other units
    let private setStartFrequency (rfSource : VI) (FrequencyInHz frequency : Frequency) =
        sprintf ":FREQ:START %eHz" (float frequency) |> rfSource.Write

    // Get current start frequency
    // TODO: Handle other units/errors
    let private getStartFrequency (rfSource : VI) = async {
        let! response = ":FREQ:START?" |> rfSource.Query 
        return parseFrequencyInHz <| response }

    // Set end frequency in Hz
    // TODO: Add other units
    let private setEndFrequency (rfSource : VI) (FrequencyInHz frequency : Frequency) =
        sprintf ":FREQ:END %eHz" (float frequency) |> rfSource.Write

    // Get current end frequency
    // TODO: Handle other units/errors
    let private getEndFrequency (rfSource : VI) = async {
        let! response = ":FREQ:END?" |> rfSource.Query 
        return parseFrequencyInHz <| response }

    let private setFrequencyMode (rfSource : VI) (fm : FrequencyMode) =
        let modestr = match fm with
                        | FrequencyMode.FIX -> "FIX"
                        | FrequencyMode.LIST -> "LIST"
        sprintf ":FREQ:MODE %s" modestr |> rfSource.Write

    let private getFrequencyMode (rfSource : VI) = async {
        let! response = ":FREQ:MODE?" |> rfSource.Query
        // TODO: cover error case
        // LIST seems to cover sweep also
        return parseFrequencyMode response
        }


    // TODO Handle failure to connect gracefully
    // Happens even when connected on occasion
    let openRfInstrument visaAddress =
        let rfSource = NationalInstruments.openInstrument visaAddress
        { new RfInstrument with
            member instrument.close() = rfSource.Close()
            member instrument.identify() = identify rfSource
            member instrument.abort() = abort rfSource
            member instrument.switchRfOn() = switchRfOn rfSource
            member instrument.switchRfOff() = switchRfOff rfSource
            member instrument.isRfOn() = isRfOn rfSource
            member instrument.isRfOff() = isRfOff rfSource
            member instrument.switchModulationOn() = switchModulationOn rfSource
            member instrument.switchModulationOff() = switchModulationOff rfSource
            member instrument.isModulationOn() = isModulationOn rfSource
            member instrument.isModulationOff() = isModulationOff rfSource
            member instrument.setPower value = setPower rfSource value
            member instrument.getPower() = getPower rfSource
            member instrument.setFrequency value = setFrequency rfSource value
            member instrument.getFrequency() = getFrequency rfSource
            member instrument.setStartFrequency value = setStartFrequency rfSource value
            member instrument.getStartFrequency() = getStartFrequency rfSource
            member instrument.setEndFrequency value = setEndFrequency rfSource value
            member instrument.getEndFrequency() = getEndFrequency rfSource
            member instrument.setFrequencyMode value = setFrequencyMode rfSource value
            member instrument.getFrequencyMode() = getFrequencyMode rfSource
        }

// Commands used in Python implementation
//AM:DEPT
//AM:INT:FREQ
//AM:INT:FUNC:SHAP
//AM:SOUR
//AM:STAT
//FM:DEV
//FM:INT:FREQ
//FM:INT:FUNC:SHAP
//FM:SOUR
//FM:STAT
//FREQ:MODE
//INIT:CONT
//INIT
//LIST:DWEL:TYPE
//LIST:FREQ
//LIST:TYPE
//SWE:DWEL
//SWE:GEN
//SWE:POIN
//SWE:TIME
//SWE:TIME:AUTO
