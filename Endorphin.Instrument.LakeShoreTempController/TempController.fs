namespace Endorphin.Instrument.LakeShoreTempController

open Endorphin.Core

/// Functions posting commands and performs queries to a LakeShore model 325 temperature
/// controller. 
module TempController =
    
    /// Query the device identity information.
    let queryIdentity = IO.queryIdentity Keys.identity

    /// Open the temperature controller at the specified VISA address with the specified 
    /// timeout for commands.
    let openInstrument visaAddress timeout = async {
        let visaInstrument = Visa.openInstrument visaAddress timeout None
        let tempController = TempController <| visaInstrument
        let! _ = queryIdentity tempController
        return tempController }

    /// Asynchronously close the connection to the given temperature controller.
    let closeInstrument (TempController tempController) =
        Visa.closeInstrument tempController
    
    /// Querry the current temperature readout for the specified control loop on the
    /// temperature controller.
    let queryCurrentTemperature = IO.queryTemperatureForLoop Keys.temperature

    /// Query the set point (target temperature) for the specified control loop on the
    /// temperature controller.
    let querySetPoint = IO.queryTemperatureForLoop Keys.setPoint

    /// Set the set point (target temperature) for the specified control loop on the
    /// temperature controller.
    let setSetPoint = IO.setTemperatureForLoop Keys.setPoint

    /// Query the control mode for the specified control loop on the temperature
    /// controller.
    let queryControlMode = IO.queryControlModeForLoop Keys.controlMode

    /// Set the control mode for the specified control loop on the temperature controller.
    let setControlMode = IO.setControlModeForLoop Keys.controlMode

    /// Query the current heater output for the specified control loop on the temperature
    /// controller.
    let queryCurrentHeatOutput = IO.queryHeaterOutputForLoop Keys.heaterOutput
    
    /// Query the manual heater output for the specified control loop on the temperature
    /// controller. This value only has effect when the control loop is in manual control
    /// mode (open loop).
    let queryManualHeaterOutput = IO.queryHeaterOutputForLoop Keys.manualHeaterOuptut
    
    /// Set the manual heater output for the specified control loop on the temperature
    /// controller. This value only has effect when the control loop is in manual control
    /// mode (open loop).
    let setManualHeaterOutput = IO.setHeaterOutputForLoop Keys.manualHeaterOuptut

    /// Query the PID settings for the specified control loop on the temperature
    /// controller. These settings only have effect when the control loop is in manual PID
    /// mode.
    let queryPidSettings = IO.queryPidSettingsForLoop Keys.pidSettings

    /// Set the PID settings for the specified control loop on the temperature controller.
    /// These settings only have effect when the control loop is in manual PID mode.
    let setPidSettings = IO.setPidSettingsForLoop Keys.pidSettings