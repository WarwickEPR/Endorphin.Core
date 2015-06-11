namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.NationalInstruments

// Command set of the Keysight RF instrument
// Implements functions to modify & query configuration
// Organised by subsystem mirroring the Keysight configuration

[<RequireQualifiedAccess>]
module RfSource =
    let openInstrument  = IO.Connect.openInstrument
    let closeInstrument = IO.Connect.closeInstrument
    let queryIdentity = IO.Identify.queryIdentity
    let setOutputOn  rfSource = setOutputState rfSource On
    let setOutputOff rfSource = setOutputState rfSource Off

    let applySettings rfSource settings = asyncChoice {
        match settings.Sweep with
        | NoSweep (frequency,amplitude)
            -> do! setCwFrequency rfSource frequency
               do! setCwAmplitude rfSource amplitude
        | StepSweep sweep
            -> do! Sweep.Apply.stepSweep rfSource sweep
        
        do! Modulation.Apply.modulationSettings rfSource settings.Modulation }


