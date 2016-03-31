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

//log4net.Config.BasicConfigurator.Configure()

let form = new Form(Visible = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current
let cts = new CancellationTokenSource()

form.Closed |> Observable.add (fun _ -> cts.Cancel())

 // define the streaming parameters: 64 kSample buffer seems to give good rate with low loss for fast sample rates
let streamingParametersAveraged = 
    Parameters.Acquisition.create (Interval.fromMicroseconds 10<us>) (1<<<17) // 64ks
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_200mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA Averaged
    |> Parameters.Acquisition.withDownsamplingRatio 100u
    |> Parameters.Streaming.create
    |> Parameters.Streaming.withAutoStop 0u 1000000u
    |> Parameters.Streaming.streamingCapture
let inputAveraged = (Analogue ChannelA, AveragedBuffer)

let streamingParametersDigital =
    Parameters.Acquisition.create (Interval.fromNanoseconds 60<ns>) (1<<<15) // 64ks
    |> Parameters.Acquisition.enableDigitalPort Port0 1.0f<V>
    |> Parameters.Acquisition.sampleDigitalPort Port0 NoDownsampling
    |> Parameters.Streaming.create
    |> Parameters.Streaming.withAutoStop 0u 10000000u
    |> Parameters.Streaming.streamingCapture
let inputDigital = (Digital Port0, NoDownsamplingBuffer)

let streamingParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromNanoseconds 800<ns>) (1<<<18)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_20mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Acquisition.withTrigger (Trigger.auto 10s<ms>)
    |> Parameters.Streaming.create
    |> Parameters.Streaming.withAutoStop 0u 100000000u
    |> Parameters.Streaming.streamingCapture

let inputNoDownsampling = (Analogue ChannelA, NoDownsamplingBuffer)

let simpleTrigger = Trigger.General.simple Range_1V Simple.Rising 0.0f<V> (AnalogueTrigger ChannelA)
let advTrigger = Trigger.General.complex 

let blockParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromPicoseconds 1200<ps>) (1<<<17)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_20mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Acquisition.withTrigger (Trigger.auto 50s<ms>)
//    |> Parameters.Acquisition.withTrigger simpleTrigger
    |> Parameters.Block.create
    |> Parameters.Block.withPostTriggerSamples 10000000
    |> Parameters.Block.blockCapture
let inputBlockNoDownsampling = (Analogue ChannelA, NoDownsamplingBuffer)

let rapidBlockParametersNoDownsampling =
    Parameters.Acquisition.create (Interval.fromPicoseconds 1200<ps>) (1<<<17)
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_20mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannel ChannelA NoDownsampling
    |> Parameters.Acquisition.withTrigger (Trigger.auto 50s<ms>)
//    |> Parameters.Acquisition.withTrigger simpleTrigger
    |> Parameters.Block.create
    |> Parameters.Block.withPostTriggerSamples 10000000
    |> Parameters.Block.rapidBlockCapture 100u
let rapidBBlockNoDownsampling = (Analogue ChannelA, NoDownsamplingBuffer)


let streamingParameters2 =
    Parameters.Acquisition.create (Interval.fromNanoseconds 20<ns>) 50000
    |> Parameters.Acquisition.enableChannel ChannelA DC Range_200mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.enableChannel ChannelB DC Range_20mV 0.0f<V> FullBandwidth
    |> Parameters.Acquisition.sampleChannels [ ChannelA ; ChannelB ] Aggregate
    |> Parameters.Acquisition.withDownsamplingRatio 1000u
    |> Parameters.Streaming.create
    |> Parameters.Streaming.withAutoStop 0u 100000000u
    |> Parameters.Streaming.streamingCapture
let inputs2 = [| (Analogue ChannelA, AggregateBuffer Maximum);
                 (Analogue ChannelB, AggregateBuffer Maximum) |]
let inputA = inputs2.[0]
let inputB = inputs2.[0]

let showTimeChart inputs acquisition max = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Signal.voltageByTime inputs acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 10.0)
        |> Observable.observeOnContext uiContext
        |> LiveChart.LineIncremental
//        |> Chart.WithXAxis(Title = "Time", Max = max)
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let showDualAggregateChart inputA inputB acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
        Chart.Combine [
            Signal.voltageByTime inputA acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 20.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental

            Signal.voltageByTime inputB acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 20.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental ]

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let showDigitalTimeChart input acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chart =
//        Streaming.Signal.digitalByteByTime input acquisition
        Signal.digitalBitByTime 2 input acquisition
//        |> Observable.sample (TimeSpan.FromMilliseconds 1.0)
        |> Observable.observeOnContext uiContext
        |> LiveChart.LineIncremental
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Value")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add

    // return to the thread pool context
    do! Async.SwitchToThreadPool() }


let showChartXY inputA inputB acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chartXY =
        Signal.voltageXY inputA inputB acquisition
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
    Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

let printSamples inputs acquisition =
    Signal.voltageByTime inputs acquisition
    |> Observable.add (printfn "Sample: %A")

let printAdc inputs acquisition =
    Signal.adcCount inputs acquisition
    |> Observable.add (printfn "Sample Adc: %A")

let printBlockCount inputs acquisition =
    Signal.blockSampleCount inputs acquisition
    |> Observable.add (printfn "Block count: %A")

let printSampledInput inputs acquisition =
    Signal.voltageByTime inputs acquisition
    |> Observable.sample (TimeSpan.FromMilliseconds 50.0)
    |> Observable.add (printfn "Sample: %A")

let printDigitalSamples input acquisition =
    Signal.digitalByteByTime input acquisition
//    |> Observable.sample (TimeSpan.FromMilliseconds 50.0)
    |> Observable.add (printfn "Sample: %A")

let printDigitalTags bit input acquisition =
    Signal.digitalBitByTime bit input acquisition
    |> Signal.digitalEdge Signal.RisingEdge
    |> Observable.add (printfn "Edge: %A")

let printPulseRate bit inputs acquisition =
    Signal.digitalBitByTime bit inputs acquisition
    |> Signal.digitalEdge Signal.RisingEdge
    |> Observable.bufferSpan (TimeSpan.FromSeconds 0.5)
    |> Observable.add (fun x -> (printfn "Pulse rate: %d /s" (x.Count * 2)))

let printPulseTotalCount bit input acquisition =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    Signal.digitalBitByTime bit input acquisition
    |> Signal.digitalEdge Signal.RisingEdge
    |> Observable.count
    |> Observable.add (fun x -> let t = timer.ElapsedMilliseconds;
                                printfn "Received %d pulses in %.1f s. Approx rate: %d ks/s" x (float t*0.001) (x/int t))

let printRate inputs acquisition =
    Signal.adcCountByTime inputs acquisition
    |> Observable.bufferSpan (TimeSpan.FromSeconds 0.5)
    |> Observable.add (fun x -> (printfn "Rate: %.1f ks/s" (float x.Count * 0.002)))

let printTotalCount inputs acquisition =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    Signal.adcCountByTime inputs acquisition
    |> Observable.count
    |> Observable.add (fun x -> let t = timer.ElapsedMilliseconds;
                                printfn "Received %d samples in %.1f s. Approx rate: %d ks/s" x (float t*0.001) (x/int t))

let printWhenFinished inputs acquisition =
    Signal.adcCount inputs acquisition
    |> Observable.last
    |> Observable.add (fun x -> printfn "That's all the samples folks")

let noDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParametersNoDownsampling
    let input = inputNoDownsampling
//    do! showTimeChart input acquisition 12.0 // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSampled acquisition
    printRate input acquisition
    printTotalCount input acquisition

    return acquisition
}

let blockNoDownsampling picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope blockParametersNoDownsampling
    let input = inputBlockNoDownsampling
//    do! showTimeChart input acquisition 0.001 // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSamples input acquisition
    printBlockCount input acquisition
//    printAdc input acquisition
    printRate input acquisition
    printTotalCount input acquisition
    printWhenFinished input acquisition

    return acquisition
}


let averaging picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParametersAveraged
    let input = inputAveraged
    do! showTimeChart input acquisition 12.0 // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 

    printStatusUpdates acquisition
//    printSampled acquisition
    printRate input acquisition
    printTotalCount input acquisition

    return acquisition
}

let aggregates picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParameters2
    do! showDualAggregateChart inputA inputB acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    return acquisition
}

let plot2d picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParameters2
    do! showChartXY inputA inputB acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    return acquisition
}

let digital picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParametersDigital
    let input = inputDigital
    do! showDigitalTimeChart input acquisition

    printStatusUpdates acquisition
    printRate input acquisition
    printTotalCount input acquisition
    printDigitalSamples input acquisition
    return acquisition
}

let digitalTag picoScope = async {
    let acquisition = Acquisition.prepare picoScope streamingParametersDigital
    let input = inputDigital

    printStatusUpdates acquisition
    printRate input acquisition
    printTotalCount input acquisition
    printPulseRate 3 input acquisition
    printPulseTotalCount 3 input acquisition
//    printDigitalTags 3 input acquisition

    return acquisition
}


let rate picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParametersNoDownsampling
    printStatusUpdates acquisition
    printTotalCount inputNoDownsampling acquisition
    return acquisition
}

let rate2 picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Acquisition.prepare picoScope streamingParameters2
    printStatusUpdates acquisition
    printTotalCount inputs2.[0] acquisition
    return acquisition
}



let experiment picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let! acquisition = noDownsampling picoScope
//    let! acquisition = averaging picoScope
//    let! acquisition = blockNoDownsampling picoScope
//    let! acquisition = plot2d picoScope
    // let! acquisition = digitalTag picoScope
    // let! acquisition = aggregates picoScope

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