namespace Endorphin.Instrument.Keysight

/// Command set of the Keysight RF instrument.
/// Implements functions to modify & query configuration.
/// Organised by subsystem mirroring the Keysight configuration.
[<RequireQualifiedAccess>]
module RfSource =
    /// Open an RfSource given a VISA address and timeout in milliseconds.
    let openInstrument  = IO.Connect.openInstrument
    /// Close an RfSource instrument.
    let closeInstrument = IO.Connect.closeInstrument
    /// Query the identity of an RfSource, and match the returned string against ones known
    /// to the program.
    let queryIdentity = IO.Identify.queryIdentity

    /// Apply a set of settings to the given RfSource machine.
    let applySettings rfSource settings = asyncChoice {
        match settings.Sweep with
        | NoSweep (frequency,amplitude)
            -> do! setCarrierFrequency rfSource frequency
               do! setCarrierAmplitude rfSource amplitude
        | StepSweep sweep
            -> do! Sweep.Apply.stepSweep rfSource sweep
        do! Modulation.Apply.modulationSettings rfSource settings.Modulation }