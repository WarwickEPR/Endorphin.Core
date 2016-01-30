// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/FSharp.Charting.0.90.13/lib/net40/FSharp.Charting.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope3000.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Windows.Forms

open FSharp.Charting
open FSharp.Control.Reactive

open Endorphin.Instrument.PicoScope3000

// log4net.Config.BasicConfigurator.Configure()

let form = new Form(Visible = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current
let cts = new CancellationTokenSource()

form.Closed |> Observable.add (fun _ -> cts.Cancel())

 // define the streaming parameters: 64 kSample buffer seems to give good rate with low loss for fast sample rates
let streamingParametersAveraged = 
    Streaming.Parameters.create (Interval.fromMicroseconds 10<us>) (1u<<<15) // 64ks
    |> Streaming.Parameters.enableChannel ChannelA DC Range_200mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.sampleChannel ChannelA Averaged
    |> Streaming.Parameters.withDownsamplingRatio 100u
    |> Streaming.Parameters.withAutoStop 0u 1000000u
let inputAveraged = (ChannelA, AveragedBuffer)

let streamingParametersNoDownsampling =
    Streaming.Parameters.create (Interval.fromMicroseconds 10<us>) (1u<<<15)
    |> Streaming.Parameters.enableChannel ChannelA DC Range_200mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.sampleChannel ChannelA NoDownsampling
    |> Streaming.Parameters.withAutoStop 0u 1000000u
let inputNoDownsampling = (ChannelA, NoDownsamplingBuffer)

let streamingParameters2 =
    Streaming.Parameters.create (Interval.fromNanoseconds 20<ns>) 50000u
    |> Streaming.Parameters.enableChannel ChannelA DC Range_200mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.enableChannel ChannelB DC Range_20mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.sampleChannels [ ChannelA ; ChannelB ] Aggregate
    |> Streaming.Parameters.withDownsamplingRatio 1000u
    |> Streaming.Parameters.withAutoStop 0u 100000000u
let inputs2 = [| (ChannelA, AggregateBuffer Maximum);
                 (ChannelB, AggregateBuffer Maximum) |]
let inputA = inputs2.[0]
let inputB = inputs2.[0]

let showTimeChart inputs acquisition max = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Streaming.Signal.voltageByTime inputs acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 5.0)
        |> Observable.observeOnContext uiContext
        |> LiveChart.LineIncremental
        |> Chart.WithXAxis(Title = "Time", Max = max)
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let showDualAggregateChart inputA inputB acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Chart.Combine [
            Streaming.Signal.voltageByTime inputA acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 20.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental

            Streaming.Signal.voltageByTime inputB acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 20.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental ]

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let showChartXY inputA inputB acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chartXY =
        Streaming.Signal.voltageXY inputA inputB acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 20.0)
        |> Observable.observeOnContext uiContext
        |> LiveChart.FastLineIncremental
        |> Chart.WithXAxis(Title = "Channel A voltage")
        |> Chart.WithYAxis(Title = "Channel B voltage")

    new ChartTypes.ChartControl(chartXY, Dock = DockStyle.Fill)
    |> form.Controls.Add

    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let printStatusUpdates acquisition =
    Streaming.Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

let printSampledInput inputs acquisition =
    Streaming.Signal.voltageByTime inputs acquisition
    |> Observable.sample (TimeSpan.FromMilliseconds 50.0)
    |> Observable.add (printfn "Sample: %A")

let printRate inputs acquisition =
    Streaming.Signal.voltageByTime inputs acquisition
    |> Observable.bufferSpan (TimeSpan.FromSeconds 0.5)
    |> Observable.add (fun x -> (printfn "Rate: %.1f ks/s" (float x.Count * 0.002)))

let printTotalCount inputs acquisition =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    Streaming.Signal.voltagesByTime inputs acquisition
    |> Observable.count
    |> Observable.add (fun x -> let t = timer.ElapsedMilliseconds;
                                printfn "Received %d samples in %.1f s. Approx rate: %d ks/s" x (float t*0.001) (x/int t))

let noDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParametersNoDownsampling
    let input = inputNoDownsampling
    do! showTimeChart input acquisition 12.0 // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSampled acquisition
    printRate input acquisition
    printTotalCount [| input |] acquisition

    return acquisition
}

let averaging picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParametersAveraged
    let input = inputAveraged
    do! showTimeChart input acquisition 12.0 // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSampled acquisition
    printRate input acquisition
    printTotalCount [| input |] acquisition

    return acquisition
}

let aggregates picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters2
    do! showDualAggregateChart inputA inputB acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    return acquisition
}

let plot2d picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters2
    do! showChartXY inputA inputB acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    return acquisition
}

let rate picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParametersNoDownsampling
    printStatusUpdates acquisition
    printTotalCount [|inputNoDownsampling|] acquisition
    return acquisition
}

let rate2 picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters2
    printStatusUpdates acquisition
    printTotalCount inputs2 acquisition
    return acquisition
}



let experiment picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    // let! acquisition = noDownsampling picoScope
    // let! acquisition = averaging picoScope
    let! acquisition = plot2d picoScope
    // let! acquisition = aggregates picoScope

    let acquisitionHandle = Streaming.Acquisition.startWithCancellationToken acquisition cts.Token
    
    // wait for the acquisition to finish automatically or by cancellation
    let! result = Streaming.Acquisition.waitToFinish acquisitionHandle
    match result with
    | Streaming.StreamCompleted -> printfn "Stream completed successfully."
    | Streaming.StreamError exn -> printfn "Stream failed: %s" exn.Message
    | Streaming.StreamCancelled -> printfn "Stream cancelled successuflly." }

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