#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/FSharp.Charting.0.90.12/lib/net40/FSharp.Charting.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope3000.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Windows.Forms

open FSharp.Charting
open FSharp.Control.Reactive

open Endorphin.Core
open Endorphin.Instrument.PicoScope3000

// log4net.Config.BasicConfigurator.Configure()

let form = new Form(Visible = true, Width = 800, Height = 600)
let uiContext = SynchronizationContext.Current
let cts = new CancellationTokenSource()

form.Closed |> Observable.add (fun _ -> cts.Cancel())

let streamingParameters = 
    // define the streaming parameters: 14 bit resolution, 20 ms sample interval, 64 kSample bufffer
    Streaming.Parameters.create (Interval.fromMicroseconds 1<us>) 50000u
    |> Streaming.Parameters.enableChannel ChannelA DC Range_100mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.enableChannel ChannelB DC Range_200mV Voltage.zero FullBandwidth
    |> Streaming.Parameters.sampleChannels [ ChannelA ; ChannelB ] Aggregate
    |> Streaming.Parameters.withDownsamplingRatio 1000u
    |> Streaming.Parameters.withAutoStop 0u 1000000000u

let showTimeChart acquisition = async {
    do! Async.SwitchToContext uiContext // add the chart to the form using the UI thread context
        
    let chart = 
        Chart.Combine [ 
            Streaming.Signal.voltageByTime (ChannelA, AggregateBuffer Maximum) acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 100.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental

            Streaming.Signal.voltageByTime (ChannelB, AggregateBuffer Maximum) acquisition
            |> Observable.sample (TimeSpan.FromMilliseconds 100.0)
            |> Observable.observeOnContext uiContext
            |> LiveChart.FastLineIncremental ]
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

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
    do! Async.SwitchToThreadPool () }

let printSampleNumber acquisition =
    Streaming.Signal.voltageByBlock (ChannelA, AggregateBuffer Minimum) acquisition
    |> Observable.add (fun arr -> printfn "A min: Got %d samples %A" arr.Length arr)
    Streaming.Signal.voltageByBlock (ChannelA, AggregateBuffer Maximum) acquisition
    |> Observable.add (fun arr -> printfn "A max: Got %d samples %A" arr.Length arr)

let printStatusUpdates acquisition =
    Streaming.Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

let experiment picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters
    do! showTimeChart acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    // do printSampleNumber acquisition
    printStatusUpdates acquisition

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