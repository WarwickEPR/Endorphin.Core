#load "Common.fsx"
open Common

open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.PicoScope5000

//log4net.Config.BasicConfigurator.Configure()

let blockParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromNanoseconds 2<ns>) Resolution_12bit (1<<<10)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_50mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Acquisition.withTrigger (Trigger.auto 50s<ms>)
    |> Parameters.Block.create
    |> Parameters.Block.withPostTriggerSamples 10000000
    |> Parameters.Block.withBuffering SingleCapture
    |> Parameters.Block.blockCapture

let noDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope blockParametersNoDownsampling
    let input = (ChannelA, NoDownsamplingBuffer)
    printStatusUpdates acquisition
//    printSampled input acquisition
    printRate input acquisition
    printTotalCount input acquisition
    return acquisition
}

let cts = new CancellationTokenSource()

let experiment picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let! acquisition = noDownsampling picoScope
    let acquisitionHandle = Acquisition.startWithCancellationToken acquisition cts.Token

    // wait for the acquisition to finish automatically or by cancellation
    let! result = Acquisition.waitToFinish acquisitionHandle
    match result with
    | AcquisitionCompleted -> printfn "Stream completed successfully."
    | AcquisitionError exn -> printfn "Stream failed: %s" exn.Message
    | AcquisitionCancelled -> printfn "Stream cancelled successuflly." }

Async.Start (async {
    try
        let! picoScope = PicoScope.openFirst()
        try
            do! experiment picoScope
        finally
            Async.StartWithContinuations(
                PicoScope.close picoScope,
                (fun ()  -> printfn "Successfully closed connection to PicoScope."),
                (fun exn -> printfn "Failed to close connection to PicoScope: %s" exn.Message),
                ignore)
    with exn -> printfn "Experiment failed: %s" exn.Message })
