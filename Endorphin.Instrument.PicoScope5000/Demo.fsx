#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/ExtCore.0.8.45/lib/net45/ExtCore.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/FSharp.Charting.0.90.12/lib/net40/FSharp.Charting.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope5000.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Windows.Forms

open ExtCore.Control
open FSharp.Charting
open FSharp.Control.Reactive

open Endorphin.Core
open Endorphin.Instrument.PicoScope5000

let form = new Form(Visible = true, TopMost = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current
let cts = new CancellationTokenSource()

form.Closed |> Observable.add (fun _ -> cts.Cancel())

let streamingParameters = 
    let inputs =
        Inputs.none // define acquisition inputs by adding the required channels and specifying downsampling
        |> Inputs.enableChannel ChannelA DC Range_500mV Voltage.zero FullBandwidth
        |> Inputs.enableChannel ChannelB DC Range_2V Voltage.zero Bandwidth_20MHz
        |> Inputs.sampleChannels [ ChannelA ; ChannelB ] NoDownsampling

    // define the streaming parameters: 14 bit resolution, 20 ms sample interval, 64 kSample bufffer
    Streaming.Parameters.create Resolution_14bit (Interval.fromMilliseconds 20<ms>) (64u * 1024u)
    |> Streaming.Parameters.withNoDownsampling inputs // use the previously defined inputs
    |> Streaming.Parameters.withAutoStop 0u 500u // acquire 500 samples after the trigger (approximately 10s)

let showTimeChart acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context
        
    let chart = 
        Chart.Combine [ 
            Streaming.Signal.voltageByTime (ChannelA, NoDownsamplingBuffer) acquisition
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental

            Streaming.Signal.voltageByTime (ChannelB, NoDownsamplingBuffer) acquisition
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental ]
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() } |> AsyncChoice.liftAsync

let showChartXY acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context

    let chartXY =
        Streaming.Signal.voltageXY (ChannelA, NoDownsamplingBuffer) (ChannelB, NoDownsamplingBuffer) acquisition
        |> Observable.observeOnContext uiContext
        |> LiveChart.FastLineIncremental
        |> Chart.WithXAxis(Title = "Channel A voltage")
        |> Chart.WithYAxis(Title = "Channel B voltage")

    new ChartTypes.ChartControl(chartXY, Dock = DockStyle.Fill)
    |> form.Controls.Add

    // return to the thread pool context
    do! Async.SwitchToThreadPool () } |> AsyncChoice.liftAsync

let printStatusUpdates acquisition =
    Streaming.Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

let experiment = asyncChoice {
    let! picoScope = PicoScope.openFirst() 

    try
        // create an acquisition with the previously defined parameters and start it after subscribing to its events
        let acquisition = Streaming.Acquisition.create picoScope streamingParameters
        do! showTimeChart acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
        printStatusUpdates acquisition

        let acquisitionHandle = Streaming.Acquisition.startWithCancellationToken acquisition cts.Token
    
        // wait for the acquisition to finish automatically or by cancellation   
        do! Streaming.Acquisition.waitToFinish acquisitionHandle

    finally Async.StartImmediate <| async {
        let! closeResult = PicoScope.close picoScope 
        match closeResult with
        | Success () -> printfn "Successfully closed connection to PicoScope."
        | Failure f  -> printfn "Failed to close connection to PicoScope due to error: %s" f } }

Async.StartWithContinuations(experiment,
    (function
    | Success () -> printfn "Successfully completed experiment."
    | Failure f  -> printfn "Failed to complete experiment due to error: %s" f),
    ignore, ignore)