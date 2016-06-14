    // Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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

    let streamingParameters = 
        // define the streaming parameters: 14 bit resolution, 20 ms sample interval, 64 kSample bufffer
        Streaming.Parameters.create Resolution_14bit (Interval.fromMicroseconds 10<us>) (64u * 1024u)
        |> Streaming.Parameters.enableChannel ChannelA DC Range_2V -2.5f<V> FullBandwidth
        |> Streaming.Parameters.sampleChannels [ ChannelA ] NoDownsampling
        |> Streaming.Parameters.withAutoStop 0u 700000u // acquire for approximately 7s

    let vcoTune =
        SignalGenerator.createBuiltInWaveform
        <| SignalGenerator.Waveform.rampUp 0.5f<V> 0.0f<V> (SignalGenerator.fixedFrequency 0.2f<Hz>)
        <| SignalGenerator.Playback.numberOfCycles 1u
        <| SignalGenerator.Trigger.software (SignalGenerator.Trigger.rising)

    let showTimeChart acquisition = async {
        do! Async.SwitchToContext ui // add the chart to the form using the UI sync context
        
        let chart = 
            Streaming.Signal.voltageByTime (ChannelA, NoDownsamplingBuffer) acquisition
            |> Observable.scan (fun (t, x) (t', x') -> (t', 0.999f * x + 0.001f * x'))
            |> Observable.skip 500
            |> Observable.every 250
            |> Observable.scanInit [] (fun xs x -> x :: xs)
            |> Observable.sample (TimeSpan.FromMilliseconds 100.0)
            |> Observable.observeOnContext ui
            |> LiveChart.FastLine
            |> Chart.WithXAxis(Title = "Time")
            |> Chart.WithYAxis(Title = "Voltage") // , Min = 2.6, Max = 3.0) // set min and max accordingly to zoom

        new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
        |> form.Controls.Add
    
        // return to the thread pool context
        do! Async.SwitchToThreadPool() }

    let experiment picoScope = async {
        do! PicoScope.SignalGenerator.setBuiltInWaveform picoScope vcoTune

        // create an acquisition with the previously defined parameters and start it after subscribing to its events
        let acquisition = Streaming.Acquisition.create picoScope streamingParameters
        do! showTimeChart acquisition
    
        let acquisitionHandle = Streaming.Acquisition.startWithCancellationToken acquisition cts.Token
    
        do! Async.Sleep 1000
        do! PicoScope.SignalGenerator.invokeSoftwareTrigger picoScope

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