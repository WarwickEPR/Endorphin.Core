#load "Common.fsx"
open Common

open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.PicoScope3000

open System.Windows.Forms
open FSharp.Charting
open FSharp.Control.Reactive

//log4net.Config.BasicConfigurator.Configure()

let form = new Form(Visible = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current

let cts = new CancellationTokenSource()
form.Closed |> Observable.add (fun _ -> cts.Cancel())


let streamingParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromMicroseconds 5<us>) (1<<<18)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_50mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Streaming.create
    |> Parameters.Streaming.withAutoStop 0u 1000000u
    |> Parameters.Streaming.streamingCapture

let showTimeChart inputs acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Signal.voltageByTime inputs acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 2.0)
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
    let acquisition = Acquisition.prepare picoScope streamingParametersNoDownsampling
    let input = (Analogue ChannelA, NoDownsamplingBuffer)
    do! showTimeChart input acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSampled acquisition
    printRate input acquisition
    printTotalCount input acquisition

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
