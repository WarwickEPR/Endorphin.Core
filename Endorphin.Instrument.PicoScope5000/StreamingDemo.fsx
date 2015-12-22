#r "../Endorphin.Core/bin/Release/Endorphin.Core.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/FSharp.Charting.0.90.12/lib/net40/FSharp.Charting.dll"
#r "bin/Release/Endorphin.Instrument.PicoScope5000.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.IO
open System.Threading
open System.Windows.Forms

open FSharp.Charting
open FSharp.Control.Reactive

open Endorphin.Core
open Endorphin.Instrument.PicoScope5000

let form = new Form(Visible = true, TopMost = true, Width = 800, Height = 600)
let ui = SynchronizationContext.Current
let cts = new CancellationTokenSource()

let path = sprintf @"%s\StreamData.csv" (Environment.GetFolderPath <| Environment.SpecialFolder.DesktopDirectory)
let writer = new StreamWriter (path)
writer.WriteLine "Time (s), Voltage (V)"

form.Closed |> Observable.add (fun _ -> cts.Cancel() ; writer.Dispose())

let streamingParameters = 
    // define the streaming parameters: 14 bit resolution, 20 ms sample interval, 64 kSample bufffer
    Streaming.Parameters.create Resolution_14bit (Interval.fromMilliseconds 20<ms>) (64u * 1024u)
    |> Streaming.Parameters.enableChannel ChannelA DC Range_2V   0.0f<V> FullBandwidth
    |> Streaming.Parameters.enableChannel ChannelB DC Range_500mV 0.0f<V> Bandwidth_20MHz
    |> Streaming.Parameters.sampleChannels [ ChannelA ; ChannelB ] NoDownsampling
    |> Streaming.Parameters.withAutoStop 0u 500u // acquire for approximately 10s

let showTimeChart acquisition = async {
    do! Async.SwitchToContext ui // add the chart to the form using the UI sync context
        
    let chart = 
        Chart.Combine [ 
            Streaming.Signal.voltageByTime (ChannelA, NoDownsamplingBuffer) acquisition
            |> Observable.observeOnContext ui
            |> LiveChart.FastLineIncremental

            Streaming.Signal.voltageByTime (ChannelB, NoDownsamplingBuffer) acquisition
            |> Observable.observeOnContext ui
            |> LiveChart.FastLineIncremental ]
        |> Chart.WithXAxis(Title = "Time")
        |> Chart.WithYAxis(Title = "Voltage")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() }

let showChartXY acquisition = async {
    do! Async.SwitchToContext ui // add the chart to the form using the UI sync context

    let chartXY =
        Streaming.Signal.voltageXY (ChannelA, NoDownsamplingBuffer) (ChannelB, NoDownsamplingBuffer) acquisition
        |> Observable.observeOnContext ui
        |> LiveChart.FastLineIncremental
        |> Chart.WithXAxis(Title = "Channel A voltage")
        |> Chart.WithYAxis(Title = "Channel B voltage")

    new ChartTypes.ChartControl(chartXY, Dock = DockStyle.Fill)
    |> form.Controls.Add

    // return to the thread pool context
    do! Async.SwitchToThreadPool () }

let writeToCsv acquisition =
    Streaming.Signal.voltageByTime (ChannelA, NoDownsamplingBuffer) acquisition
    |> Observable.add (fun (time, voltage) -> sprintf "%.9f, %f" time voltage |> writer.WriteLine)

let printStatusUpdates acquisition =
    Streaming.Acquisition.status acquisition
    |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

let printCumulativeSampleCount acquisition =
    Streaming.Signal.voltageByBlock (ChannelA, NoDownsamplingBuffer) acquisition
    |> Observable.map Array.length
    |> Observable.scan (+)
    |> Observable.timestamp
    |> Observable.add (printfn "Total samples: %A")

let experiment picoScope = async {
    // create an acquisition with the previously defined parameters and start it after subscribing to its events
    let acquisition = Streaming.Acquisition.create picoScope streamingParameters
    do! showTimeChart acquisition // use showTimeChart to show X and Y vs T or showXYChart to to plot Y vs X 
    printStatusUpdates acquisition
    printCumulativeSampleCount acquisition
    writeToCsv acquisition

    let acquisitionHandle = Streaming.Acquisition.startWithCancellationToken acquisition cts.Token
    
    // wait for the acquisition to finish automatically or by cancellation   
    let! result = Streaming.Acquisition.waitToFinish acquisitionHandle
    match result with
    | Streaming.StreamCompleted -> printfn "Stream completed successfully."
    | Streaming.StreamError exn -> printfn "Stream failed: %s" exn.Message
    | Streaming.StreamCancelled -> printfn "Stream cancelled successuflly." }

Async.Start <| async {
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
    with exn -> printfn "Experiment failed: %s" exn.Message }