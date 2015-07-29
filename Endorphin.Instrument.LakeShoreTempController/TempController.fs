namespace Endorphin.Instrument.LakeShoreTempController

open Endorphin.Core.NationalInstruments
open ExtCore.Control

module TempController =
    let queryIdentity = IO.queryIdentity Keys.identity

    let openInstrument visaAddress timeout = asyncChoice {
        let visaInstrument = Visa.openInstrument visaAddress timeout
        let tempController = TempController <| visaInstrument
        let! _ = queryIdentity tempController
        return tempController }
    
    let queryCurrentTemperature = IO.queryTemperatureForLoop Keys.temperature

    let querySetPoint = IO.queryTemperatureForLoop Keys.setPoint
    let setSetPoint = IO.setTemperatureForLoop Keys.setPoint

    let queryControlMode = IO.queryControlModeForLoop Keys.controlMode
    let setControlMode = IO.setControlModeForLoop Keys.controlMode

    let queryCurrentHeatOutput = IO.queryHeaterOutputForLoop Keys.heaterOutput
    
    let queryManualHeaterOutput = IO.queryHeaterOutputForLoop Keys.manualHeaterOuptut
    let setManualHeaterOutput = IO.setHeaterOutputForLoop Keys.manualHeaterOuptut

    let queryPidSettings = IO.queryPidSettingsForLoop Keys.pidSettings
    let setPidSettings = IO.setPidSettingsForLoop Keys.pidSettings