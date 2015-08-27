namespace Endorphin.Instrument.Keysight

open ExtCore.Control

[<AutoOpen>]
module Basic =
    /// Key for the carrier wave freqeuncy of the device.
    /// Command reference p.44.
    let private carrierFrequencyKey = ":FREQUENCY"
    /// Set the carrier wave frequency of the device.
    let setCarrierFrequency = IO.setFrequency carrierFrequencyKey
    /// Query the carrier wave frequency of the device.
    let queryCarrierFrequency = IO.queryFrequency carrierFrequencyKey

    /// Key for the RF amplitude of the machine.
    /// Command reference p.83.
    let private carrierAmplitudeKey = ":POWER"
    /// Set the RF amplitude of the carrier wave.
    let setCarrierAmplitude = IO.setAmplitude carrierAmplitudeKey
    /// Query the RF amplitude of the carrier wave.
    let queryCarrierAmplitude = IO.queryAmplitude carrierAmplitudeKey

    /// Key for the phase of the modulating signal.
    /// Command reference p.49.
    let private phaseKey = ":PHASE"
    /// Set the phase of the modulation signal on the machine.
    let setPhase = IO.setPhase phaseKey
    /// Query the phase of the modulation signal of the machine.
    let queryPhase = IO.queryPhase phaseKey

    /// Key to send a trigger on the bus.
    /// Command reference p.124.
    let private triggerKey = "*TRG"
    /// Send a trigger on the bus.
    let trigger = IO.writeKey triggerKey