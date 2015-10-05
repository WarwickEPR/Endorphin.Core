#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"
#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"


// I don't open any modules throughout to demonstrate exactly where each function comes from.
// Most (if not all) modules may be opened to simplify this, but in general I suggest you don't
// do that due to naming clashes.

// It's ok to open the "Control" module, though - due to the nature of the module compared to
// other modules, it's unlikely to have any naming collisions.

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight

// log4net.Config.BasicConfigurator.Configure ()

// a phase for use with RF pulses - this sets I and Q to have the same
// relative power
let equalIQ = Phase.empty |> Phase.add (Phase_rad 0.0<rad>)

// some defined marker types for easy access
let marker1 = Markers.empty |> Markers.withMarker1 true
let marker2 = Markers.empty |> Markers.withMarker2 true
let marker3 = Markers.empty |> Markers.withMarker3 true

// Define the pulse sequence.  We can use up to three marker channels (even though there are four), because
// Endorphin needs one internally for the RF blanking pulse.

// At the default (currently unchangeable in experiment mode) ARB clock rate, one sample is equal to
// 1/(150Mhz), or 6.67ns.
let pulses = seq {
    // pi/2 pulse
    yield Pulse.rf equalIQ 60u
    // tau pulse
    yield Pulse.delayWithIncrement 6000u 100u
    // pi pulse
    yield Pulse.rf equalIQ 120u
    // tau pulse
    yield Pulse.delayWithIncrement 6000u 100u
    // trigger to begin acquisition
    yield Pulse.trigger marker1 }

// There is currently a very large problem with the repetitions / shots per point which has manifested
// itself.  The number of phases * repetitions * shots per point must be less than (very approximately)
// 128 or the machine might return the error code "630 - too many segments" when you try to play back
// the experiment :(

// define parameters to do with the experiment
let experiment =
    Experiment.empty
    |> Experiment.withPulseSeq pulses
    |> Experiment.withRepetitions 128
    |> Experiment.withShotRepetitionTime 10e-6<s>

// define the routing of the marker channels
let routing =
    Routing.empty
    |> Routing.withBasebandTrigger1 RouteMarker1
    |> Routing.withBasebandTrigger2 RouteMarker2
    |> Routing.withEvent1           RouteMarker3
    // the markers default to positive polarity, but let's be on the safe side
    |> Routing.withMarker1Polarity  Positive
    |> Routing.withMarker2Polarity  Positive
    |> Routing.withMarker3Polarity  Positive

try
    async {
        // open the keysight box - set the VISA access string you need here and timeout
        let! keysight = RfSource.openInstrument "USB0::0x0957::0x1F01::MY53051252::INSTR" 100000<ms>

        // set the routing correctly
        do! Routing.set keysight routing
        // set the triggering of the ARB
        do! ARB.Trigger.set keysight <| ARB.Trigger.continuous FreeRun

        // set the frequency of the carrier wave
        do! setCarrierFrequency keysight <| Frequency_Hz 150e6<Hz>
        // set the power of the carrier wave
        do! setCarrierAmplitude keysight <| Power_dBm 4.0<dBm>

        // store the experiment on the machine
        let! storedExperiment = Control.Experiment.store keysight experiment

        // if we want to change anything before we begin playback, this tells us which file
        // holds the experiment - only exists when the library is compiled in debug mode
        do Control.Print.experimentFile storedExperiment

        // play the experiment
        do! Control.Experiment.playStored keysight storedExperiment

        // tidy up and close
        do! RfSource.closeInstrument keysight }
    |> Async.RunSynchronously
with
    | :? InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0