namespace Endorphin.Instrument.Keysight

[<AutoOpen>]
module Control =

    let private outputStateKey = ":OUTPUT:STATE"
    let setOutputState = IO.setOnOffState outputStateKey
    let queryOutputState = IO.queryOnOffState outputStateKey

    let private cwFrequencyKey = ":FREQUENCY"
    let setCwFrequency = IO.setFrequency cwFrequencyKey
    let queryCwFrequency = IO.queryFrequency cwFrequencyKey

    let private cwAmplitudeKey = ":POWER"
    let setCwAmplitude = IO.setAmplitude cwAmplitudeKey
    let queryCwAmplitude = IO.queryAmplitude cwAmplitudeKey

    let private phaseKey = ":PHASE"
    let setPhase = IO.setPhase phaseKey
    let queryPhase = IO.queryPhase phaseKey

