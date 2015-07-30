﻿#r @"../packages/ExtCore.0.8.45/lib/net45/ExtCore.dll"
#r @"../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r @"NationalInstruments.Common.dll"
#r @"NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.LakeShoreTempController.dll"
#r @"../packages/FSharp.Charting.0.90.9/lib/net40/FSharp.Charting.dll"
#r @"../packages/log4net.2.0.3/lib/net40-full/log4net.dll"
#r @"System.Windows.Forms.DataVisualization.dll"

open System.Threading
open System.Windows.Forms
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open Endorphin.Instrument.LakeShoreTempController
open ExtCore.Control
open FSharp.Charting
open log4net.Config

// enable logging (shows all VISA communication by default)
// BasicConfigurator.Configure()

// instrument VISA address
let visaAdddress = "GPIB0::12::INSTR"

// device contnrol loop
let controlLoop = Loop1

// PID settings which will be tested in the experiment
let pidSettings =
    { Proportional = 1.0
      Integral     = 1.0
      Differential = 1.0 }

// set point parameters: the initial set point is held for the specified delay and then changed
// to the final set point value
let initialSetPoint = TemperatureInK 80.0<K>
let finalSetPoint   = TemperatureInK 100.0<K>
let setPointDelay   = 10000 // ms

// number of measurements and interval between them
let measurementRepeats  = 1000
let measurementInterval = 3000 // ms

// measurements which will be charted: measured temperature, target temperature and heater output
let temperatureMeasurement  = new Event<Temperature>()
let targetTemperature       = new Event<Temperature>()
let heaterOutputMeasurement = new Event<HeaterOutput>()

// create a form and capture the corresponding thread synchronisation context
let form = new Form(Visible = true, TopMost = true, Width = 800, Height = 600)
let uiContext = WindowsFormsSynchronizationContext.Current

// show the chart of measurements over time
let showTimeChart () = async {
    // add the chart to the form using the UI thread context
    do! Async.SwitchToContext uiContext
        
    let chart = 
        Chart.Combine [ 
            temperatureMeasurement.Publish
            |> Event.mapi (fun i (TemperatureInK temp) -> ((i * measurementInterval / 1000), temp))
            |> Observable.observeOn uiContext
            |> LiveChart.FastLineIncremental

            targetTemperature.Publish
            |> Event.mapi (fun i (TemperatureInK temp) -> ((i * measurementInterval / 1000), temp))
            |> Observable.observeOn uiContext
            |> LiveChart.FastLineIncremental
            
            heaterOutputMeasurement.Publish
            |> Event.mapi (fun i (HeaterOutput output) -> ((i * measurementInterval / 1000), output))
            |> Observable.observeOn uiContext
            |> LiveChart.FastLineIncremental ]
        |> Chart.WithXAxis(Title = "Time (s)")
        |> Chart.WithYAxis(Title = "Temperature (K)")
        |> Chart.WithYAxis2(Title = "Heater output (%)")

    new ChartTypes.ChartControl(chart, Dock = DockStyle.Fill)
    |> form.Controls.Add
    
    // return to the thread pool context
    do! Async.SwitchToThreadPool() } |> AsyncChoice.liftAsync

// monitor the temperature controller's temperature measurements and heater output at the given
// interval for the specified number of repeats and post measurements to the events
let rec monitorController tempController repeats interval = asyncChoice {
    let! temperature = TempController.queryCurrentTemperature tempController controlLoop
    let! heaterOutput = TempController.queryCurrentHeatOutput tempController controlLoop

    temperatureMeasurement.Trigger  temperature
    targetTemperature.Trigger finalSetPoint
    heaterOutputMeasurement.Trigger heaterOutput
    if repeats <> 0 then 
        do! Async.Sleep interval |> AsyncChoice.liftAsync
        do! monitorController tempController (repeats - 1) interval }

// set the temperature set point after the specified delay
let setSetPoint tempController delay setPoint = asyncChoice {
    do! Async.Sleep delay |> AsyncChoice.liftAsync
    do! TempController.setSetPoint tempController controlLoop setPoint  }

// experiment workflow
let experiment = asyncChoice {
    // connect to the temperature controller
    let! tempController = TempController.openInstrument visaAdddress 3000

    // add the live chart to the form
    do! showTimeChart ()

    // set the initial set point and enable the specified PID settings
    do! TempController.setSetPoint tempController controlLoop initialSetPoint
    do! TempController.setPidSettings tempController controlLoop pidSettings
    do! TempController.setControlMode tempController controlLoop ManualPID

    // start a workflow which will change the set point after the specified delay and begin
    // monitoring the temperature and heater output
    Async.Start (setSetPoint tempController setPointDelay finalSetPoint |> Async.Ignore)
    do! monitorController tempController measurementRepeats measurementInterval }

// when the form is shown run the experiment workflow and print success/failure messages
form.Shown.Add(fun _ ->
    Async.StartWithContinuations (experiment,
        (function
        | Success ()    -> printfn "Successfully perfromed %d measurements." measurementRepeats
        | Failure error -> printfn "Failed to perform measurements: %s" error),
        (fun exn -> printfn "Error: %s" exn.Message),
        ignore))