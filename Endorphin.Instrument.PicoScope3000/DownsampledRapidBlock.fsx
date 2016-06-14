#load "Common.fsx"
open Common

open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.PicoScope3000

//log4net.Config.BasicConfigurator.Configure()

let rapidBlockParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromNanoseconds 12<ns>) (1<<<10)
    |> Parameters.Acquisition.enableChannel ChannelB DC Range_50mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelB Averaged
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_100mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA Aggregate
    |> Parameters.Acquisition.withDownsamplingRatio 3u
    |> Parameters.Acquisition.withTrigger (Trigger.auto 50s<ms>)
    |> Parameters.Block.create
    |> Parameters.Block.withPreTriggerSamples 10000
    |> Parameters.Block.withPostTriggerSamples 90000
    |> Parameters.Block.withBuffering (MultipleCapture 4u) 
//    |> Parameters.Block.withBuffering SingleCapture
    |> Parameters.Block.rapidBlockCapture 50u
let inputA = (Analogue ChannelA, AggregateBuffer Maximum)
let inputB = (Analogue ChannelB, AveragedBuffer)


let noDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope rapidBlockParametersNoDownsampling
    printStatusUpdates acquisition
//    printSampled input acquisition
    printRate inputB acquisition
    printTotalCount inputB acquisition
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
