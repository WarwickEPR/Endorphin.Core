namespace Endorphin.Instrument.Keysight

[<AutoOpen>]
module Control =
    /// Key for the overall RF output state. Must be On if anything is to play.
    /// Command reference p.157.
    let private outputStateKey = ":OUTPUT:STATE"
    /// Set the overall output state of the machine.
    let setOutputState = IO.setOnOffState outputStateKey
    /// Query the overall output state of the machine.
    let queryOutputState = IO.queryOnOffState outputStateKey

    /// Key for the continuous wave freqeuncy of the device.
    /// Command reference p.44.
    let private cwFrequencyKey = ":FREQUENCY"
    /// Set the continuous wave frequency of the device.
    let setCwFrequency = IO.setFrequency cwFrequencyKey
    /// Query the continuous wave frequency of the device.
    let queryCwFrequency = IO.queryFrequency cwFrequencyKey

    /// Key for the RF amplitude of the machine.
    /// Command reference p.83.
    let private cwAmplitudeKey = ":POWER"
    /// Set the RF amplitude of the machine.
    let setCwAmplitude = IO.setAmplitude cwAmplitudeKey
    /// Query the RF amplitude of the machine.
    let queryCwAmplitude = IO.queryAmplitude cwAmplitudeKey

    /// Key for the phase of the modulating signal.
    /// Command reference p.49.
    let private phaseKey = ":PHASE"
    /// Set the phase of the modulation signal on the machine.
    let setPhase = IO.setPhase phaseKey
    /// Query the phase of the modulation signal of the machine.
    let queryPhase = IO.queryPhase phaseKey