#load "Common.fsx"
open Common

open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.PicoScope5000

open System.Windows.Forms
open FSharp.Charting
open FSharp.Control.Reactive

//log4net.Config.BasicConfigurator.Configure()

let form = new Form(Visible = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current

let cts = new CancellationTokenSource()
form.Closed |> Observable.add (fun _ -> cts.Cancel())

let rapidBlockParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromNanoseconds 12<ns>) Resolution_14bit (1<<<10)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_50mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Acquisition.withTrigger (Trigger.auto 50s<ms>)
    |> Parameters.Block.create
    |> Parameters.Block.withPreTriggerSamples 10000
    |> Parameters.Block.withPostTriggerSamples 90000
    |> Parameters.Block.withBuffering (MultipleCapture 4u) 
//    |> Parameters.Block.withBuffering SingleCapture
    |> Parameters.Block.rapidBlockCapture 50u
let inputA = (ChannelA, NoDownsamplingBuffer)

let showTimeChart inputs acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Signal.voltageByTime inputs acquisition
        |> Observable.observeOnContext uiContext
        |> LiveChart.LineIncremental
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let noDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope rapidBlockParametersNoDownsampling
    printStatusUpdates acquisition
//    printSampled input acquisition
    printRate inputA acquisition
    printTotalCount inputA acquisition
    return acquisition
}

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

