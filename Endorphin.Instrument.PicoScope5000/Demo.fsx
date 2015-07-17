#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope5000.dll"
#r "bin/Debug/ExtCore.dll"
#r "../packages/FSharp.Charting.0.90.9/lib/net40/FSharp.Charting.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open Endorphin.Instrument.PicoScope5000
open ExtCore.Control
open System
open System.Threading
open System.Windows.Forms
open FSharp.Charting

let form = new Form(Visible = true, TopMost = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current

let streamingParameters = 
    let inputs =
        Inputs.none // define acquisition inputs by adding the required channels and specifying downsampling
        |> Inputs.enableChannel ChannelA DC Range_500mV Voltage.zero FullBandwidth
        |> Inputs.enableChannel ChannelB DC Range_2V Voltage.zero Bandwidth_20MHz
        |> Inputs.sampleChannels [ ChannelA ; ChannelB ] NoDownsampling

    // define the streaming parameters: 14 bit resolution, 20 ms sample interval, 64 kSample bufffer
    Streaming.Parameters.create Resolution_14bit (Interval.fromMilliseconds 20<ms>) (64u * 1024u)
    |> Streaming.Parameters.withNoDownsampling inputs // use the previously defined inputs

let showTimeChart acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context
        
    let chart = 
        Chart.Combine [ 
            Streaming.Signals.sampleByTime (ChannelA, NoDownsamplingBuffer) acquisition
            |> Observable.observeOn uiContext
            |> LiveChart.FastLineIncremental

            Streaming.Signals.sampleByTime (ChannelB, NoDownsamplingBuffer) acquisition
            |> Observable.observeOn uiContext
            |> LiveChart.FastLineIncremental ]
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "ADC coounts")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() } |> AsyncChoice.liftAsync

let showChartXY acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chartXY =
        Streaming.Signals.sampleXY (ChannelA, NoDownsamplingBuffer) (ChannelB, NoDownsamplingBuffer) acquisition
        |> Observable.observeOn uiContext
        |> LiveChart.FastLineIncremental
        |> Chart.WithXAxis(Title = "Channel A ADC counts")
        |> Chart.WithYAxis(Title = "Channel B ADC counts")

    new ChartTypes.ChartControl(chartXY, Dock = DockStyle.Fill)
    |> form.Controls.Add

    // return to the thread pool context
    do! Async.SwitchToThreadPool () } |> AsyncChoice.liftAsync

let printStatusUpdates acquisition =
    Streaming.Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...)

let experiment = asyncChoice {
    let! picoScope = PicoScope.openFirst() // open the first avaiable PicoScope
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters
    do! showTimeChart acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    let! acquisitionHandle = Streaming.Acquisition.start acquisition
    
    // run the acquisition for 10s and then stop manually
    do! Async.Sleep 10000 |> AsyncChoice.liftAsync
    do! Streaming.Acquisition.stopAndFinish acquisitionHandle }

Async.StartWithContinuations(experiment, printfn "%A", ignore, ignore)
