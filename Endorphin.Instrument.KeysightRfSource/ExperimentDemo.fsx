#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight
open Experiment
open Control

// a phase for use with RF pulses - this sets I and Q to have the same
// relative power
let equalIQ = Phase.empty |> Phase.add (Phase_rad 0.0<rad>)

// some defined marker types for easy access
let marker1 = Markers.empty |> Markers.withMarker1 true
let marker2 = Markers.empty |> Markers.withMarker2 true
let marker3 = Markers.empty |> Markers.withMarker3 true

// define the pulse sequence
let pulses = seq {
    // pi/2 pulse
    yield rf equalIQ 60u
    // tau pulse
    yield delayWithIncrement 60u 10u
    // pi pulse
    yield rf equalIQ 120u
    // tau pulse
    yield delayWithIncrement 60u 10u
    // trigger to begin acquisition
    yield trigger marker1 }

// define parameters to do with the experiment
let experiment =
    Experiment.empty
    |> withPulseSeq pulses
    |> withRepetitions 128
    |> withShotRepetitionTime 10e-6<s>

// define the routing of the marker channels
let routing =
    Routing.empty
    |> Routing.withBasebandTrigger1 RouteMarker1
    |> Routing.withBasebandTrigger2 RouteMarker2
    |> Routing.withEvent1           RouteMarker3
    |> Routing.withMarker1Polarity  Positive
    |> Routing.withMarker2Polarity  Positive
    |> Routing.withMarker3Polarity  Positive

async {
    // open the keysight box - set the VISA access string you need here and timeout
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000<ms>

    // set the routing correctly
    do! Routing.set keysight routing
    // set the triggering of the ARB
    do! ARB.Trigger.set keysight <| ARB.Trigger.continuous FreeRun

    // store the experiment on the machine
    let! storedExperiment = storeExperiment keysight experiment

    // play the experiment
    do! playStoredWaveform keysight storedExperiment.StoredExperiment

    // tidy up and close
    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously