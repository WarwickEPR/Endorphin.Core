namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.NationalInstruments

// Command set of the Keysight RF instrument
// Implements functions to modify & query configuration
// Organised by subsystem mirroring the Keysight configuration

[<RequireQualifiedAccess>]
module RfSource =
    let queryIdentity = IO.Identify.queryIdentity

    let private outputStateKey = ":OUTPUT:STATE"
    let setOutputState = IO.setOnOffState outputStateKey
    let queryOutputState = IO.queryOnOffState outputStateKey
        
    let private modulationStateKey = ":OUTPUT:MODULATION"
    let setModulationState = IO.setOnOffState modulationStateKey
    let queryModulationState = IO.queryOnOffState modulationStateKey

module Frequency =
    let private cwFrequencyKey = ":FREQUENCY"
    let setCwFrequency = IO.setFrequency cwFrequencyKey
    let queryCwFrequency = IO.queryFrequency cwFrequencyKey

    let private startFrequencyKey = ":FREQUENCY:START"
    let setStartFrequency = IO.setFrequency startFrequencyKey
    let queryStartFrequncy = IO.queryFrequency startFrequencyKey

    let private stopFrequencyKey = ":FREQUENCY:STOP"
    let setStopFrequency = IO.setFrequency stopFrequencyKey
    let queryStopFrequency = IO.queryFrequency stopFrequencyKey

    let private frequencySpanKey = ":FREQUENCY:SPAN"
    let setFrequencySpan = IO.setFrequency frequencySpanKey
    let queryFrequencySpan = IO.queryFrequency frequencySpanKey

    let private phaseKey = ":PHASE"
    let setPhase = IO.setPhase phaseKey
    let queryPhase = IO.queryPhase phaseKey

module Amplitude =
    let private cwAmplitudeKey = ":POWER"
    let setCwAmplitude = IO.setAmplitude cwAmplitudeKey
    let queryCwAmplitude = IO.queryAmplitude cwAmplitudeKey

    let private startAmplitudeKey = ":POWER:START"
    let setStartAmplitude = IO.setAmplitude startAmplitudeKey
    let queryStartAmplitude = IO.queryAmplitude startAmplitudeKey

    let private stopAmplitudeKey = ":POWER:STOP"
    let setStopAmplitude = IO.setAmplitude stopAmplitudeKey
    let queryStopAmplitude = IO.queryAmplitude stopAmplitudeKey

