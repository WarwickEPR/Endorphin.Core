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

    let parsePowerInDbm str =
        PowerInDbm ((float str) * 1.0<dBm>)

    type RfInstrument =
        abstract member close : unit -> Async<unit>
        abstract member identify : unit -> Async<string>
        abstract member abort : unit -> unit
        abstract member setPower : Power -> unit
        abstract member getPower : unit -> Async<Power>

    let identify (rfSource : VI) =
        rfSource.Query "*IDN?"

    let abort (rfSource : VI) =
        rfSource.Write("ABORT")

    // Set power in dB
    // TODO: Add other units
    let setPower (rfSource : VI) (PowerInDbm power : Power) =
        sprintf ":POW %edBm" (float power) |> rfSource.Write

    // Get current power setting
    // TODO: Handle other units?
    let getPower (rfSource : VI) = async {
        // Leaves units in original state
        let! powerunit = rfSource.Query ":UNIT:POW?"
        let! response = sprintf ":UNIT:POW DBM; :POW?; :UNIT:POW %s" powerunit |> rfSource.Query 
        return parsePowerInDbm response }

    // TODO Handle failure to connect gracefully
    // Happens even when connected on occasion
    let openRfInstrument visaAddress =
        let rfSource = NationalInstruments.openInstrument visaAddress
        { new RfInstrument with
            member instrument.close() = rfSource.Close()
            member instrument.identify() = identify rfSource
            member instrument.abort() = abort rfSource
            member instrument.setPower value = setPower rfSource value
            member instrument.getPower() = getPower rfSource
        }

// Commands used in Python implementation
//ABOR
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
//FREQ:CW
//FREQ:MODE
//FREQ:STAR
//FREQ:STOP
//INIT:CONT
//INIT;OUTP:STAT
//LIST:DWEL:TYPE
//LIST:FREQ
//LIST:TYPE
//OUTP:MOD
//OUTP:STAT
//POW
//SWE:DWEL
//SWE:GEN
//SWE:POIN
//SWE:TIME
//SWE:TIME:AUTO
//UNIT:POW
