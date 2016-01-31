// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Experiment.HighFieldEpr

open System
open System.IO
open System.Threading
open System.Windows
open System.Windows.Data
open System.Windows.Input
open Microsoft.Win32

open FsXaml
open FSharp.ViewModule
open Nessos.FsPickler.Json

open FSharp.Control.Reactive
open OxyPlot

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open Endorphin.Instrument.TwickenhamSmc
open Endorphin.Instrument.PicoScope5000
open CwEprExperiment

type CwEprExperimentState =
    | Waiting
    | Running  of experiment : CwEprExperiment * cts : CancellationTokenSource
    | Finished of parameters : CwEprExperimentParameters * data : CwEprData

type InstrumentConnection =
    | Connected of magnetController : MagnetController * picoScope : PicoScope5000
    | Disconnected

type CwEprViewModel() as self =
    inherit ViewModelBase()

    let pickler = FsPickler.CreateJsonSerializer (indent=true)

    let magnetControllerSettings = 
        { TwickenhamSmc.HardwareParameters =
            { MaximumCurrent = 20.0M<A>
              CalibratedRampRates =
                [ 0.00020; 0.00024; 0.00026; 0.00030; 0.00036; 0.00042; 0.00048; 0.00054; 
                  0.00064; 0.00072; 0.00084; 0.00098; 0.00110; 0.00130; 0.00150; 0.00170;
                  0.0020;  0.0024;  0.0026;  0.0030;  0.0036;  0.0042;  0.0048;  0.0054; 
                  0.0064;  0.0072;  0.0084;  0.0098;  0.0110;  0.0130;  0.0150;  0.0170;
                  0.020;   0.024;   0.026;   0.030;   0.036;   0.042;   0.048;   0.054; 
                  0.064;   0.072;   0.084;   0.098;   0.110;   0.130;   0.150;   0.170;
                  0.20;    0.24;    0.26;    0.30;    0.36;    0.42;    0.48;    0.54; 
                  0.64;    0.72;    0.84;    0.98;    1.10;    1.30;    1.50;    1.70; 
                  2.0 ]
                |> List.map (fun x -> (decimal x) * 1.0M<A/s>) }
              
          TwickenhamSmc.Limits = 
            { RampRateLimit    = 0.1M<A/s>
              TripVoltageLimit = 2.5M<V>
              CurrentLimit     = 17.5M<A> }
          
          TwickenhamSmc.FieldCalibration =
            { StaticField       = 14.14421M<T>
              LinearCoefficient = -0.002833M<T/A> }
          
          TwickenhamSmc.ShuntCalibration = 
            { VoltageOffset     = 0.00218M<V>
              LinearCoefficient = 0.3985M<V/A> 
              RmsVoltageNoise   = 0.100M<V> }
      
          TwickenhamSmc.LastUpdated = new DateTime(2016, 1, 26) }

    let defaultParameters = 
        Parameters.create (14.146M<T> + 0.005M<T>) (0.01M<T>) (0.020M<A/s> * 0.002845M<T/A>)
        <| magnetControllerSettings
    
    let reSeries = new Series.LineSeries(Color=OxyColors.Navy,      StrokeThickness=1.5)
    let imSeries = new Series.LineSeries(Color=OxyColors.IndianRed, StrokeThickness=1.5)
    
    let defaultPlotModel = 
        let model = new PlotModel()
        model.Series.Add reSeries
        model.Series.Add imSeries
        model

    let experimentParameters = self.Factory.Backing(<@ self.ExperimentParameters @>, defaultParameters, Parameters.validate)
    let experimentState = self.Factory.Backing(<@ self.ExperimentState @>, Waiting)
    let plotModel = self.Factory.Backing(<@ self.PlotModel @>, defaultPlotModel)
    let connection = self.Factory.Backing(<@ self.Connection @>, Disconnected)
    let statusMessage = self.Factory.Backing(<@ self.StatusMessage @>, "")

    let connect ui = async {
        try
            let! magnetController = MagnetController.openInstrument "GPIB0::4" 3000<ms> magnetControllerSettings
            let! picoScope = PicoScope.openFirst()

            do! Async.SwitchToContext ui
            self.Connection <- Connected (magnetController, picoScope)
        with exn ->
            do! Async.SwitchToContext ui
            sprintf "Failed to connect to instruments: %s" exn.Message
            |> MessageBox.Show |> ignore  }

    let disconnect ui = async {
        do! Async.SwitchToContext ui
        let (magnetController, picoScope) =
            match self.Connection with
            | Connected (magnetController, picoScope) -> (magnetController, picoScope)
            | Disconnected                            -> failwith "No connection." 
        
        self.Connection <- Disconnected
        
        do! Async.SwitchToThreadPool()
        do! MagnetController.closeInstrument magnetController
        do! PicoScope.close picoScope }

    let save () =
        match self.ExperimentState with
        | Finished (parameters, data) ->
            let saveFile = new SaveFileDialog (Filter="CW EPR experiment|*.cwepr")
            let result = saveFile.ShowDialog() 
            if result.HasValue && result.Value then
                use paramsFile = new StreamWriter (saveFile.FileName)
                pickler.PickleToString parameters |> paramsFile.Write

                use dataFile = new StreamWriter (Path.ChangeExtension(saveFile.FileName, "csv"))
                Data.signalRows data |> Seq.iter dataFile.WriteLine
                
                let rawPath = Path.GetDirectoryName saveFile.FileName + "\\" + Path.GetFileNameWithoutExtension saveFile.FileName + "_raw.csv"
                use rawFile = new StreamWriter (rawPath)
                Data.rawDataRows data |> Seq.iter rawFile.WriteLine

        | _ -> "No data to save." |> MessageBox.Show |> ignore 

    let loadParameters () =
        if self.IsReadyToStart then
            let openFile = new OpenFileDialog (Filter="CW EPR experiment|*.cwepr")
            let result = openFile.ShowDialog()
            if result.HasValue && result.Value then
                use paramsFile = new StreamReader (openFile.FileName)
                let parameters = paramsFile.ReadToEnd() |> pickler.UnPickleOfString
                self.ExperimentParameters <- parameters |> Parameters.withMagnetControllerSettings magnetControllerSettings
                
        else "Cannot load experiment parameters while an experiment is running." |> MessageBox.Show |> ignore
    
    let resetAxes () = self.PlotModel.ResetAllAxes()

    let performExperiment ui = async {
        do! Async.SwitchToContext ui
        let cts = new CancellationTokenSource()

        let parameters = self.ExperimentParameters |> Parameters.withDate DateTime.Now
        
        let (magnetController, picoScope) =
            match self.Connection with
            | Connected (magnetController, picoScope) -> (magnetController, picoScope)
            | Disconnected                            -> failwith "No connection."
        
        let experiment = CwEprExperiment.create parameters magnetController picoScope
        
        use __ =
            CwEprExperiment.status experiment
            |> Observable.observeOnContext ui
            |> Observable.subscribeWithError
                (fun status -> self.StatusMessage <- sprintf "%A" status)
                (fun exn    -> self.StatusMessage <- sprintf "Experiment failed: %s." exn.Message)

        CwEprExperiment.reSignal experiment
        |> Observable.sample (TimeSpan.FromMilliseconds 500.0)
        |> Observable.map (Seq.map (fun (x, y) -> new DataPoint(float (decimal x), float y)))
        |> Observable.observeOnContext ui
        |> Observable.add (fun signal ->
            self.RePoints.Clear()
            self.RePoints.AddRange signal
            self.PlotModel.InvalidatePlot true)
        
        if CwEprExperiment.Parameters.quadratureDetection parameters then
            CwEprExperiment.imSignal experiment
            |> Observable.sample (TimeSpan.FromMilliseconds 500.0)
            |> Observable.map (Seq.map (fun (x, y) -> new DataPoint(float (decimal x), float y)))
            |> Observable.observeOnContext ui
            |> Observable.add (fun signal ->
                self.ImPoints.Clear()
                self.ImPoints.AddRange signal
                self.PlotModel.InvalidatePlot true)

        self.RePoints.Clear() ; self.ImPoints.Clear()
        self.PlotModel.ResetAllAxes()
        self.PlotModel.InvalidatePlot true
        self.ExperimentState <- Running (experiment, cts)

        do! Async.SwitchToThreadPool()

        let! waitForData = 
            CwEprExperiment.data experiment 
            |> Observable.last
            |> Async.AwaitObservable
            |> Async.StartChild

        let! experimentResult = CwEprExperiment.run experiment cts.Token
        let! data = waitForData

        do! Async.SwitchToContext ui
        self.ExperimentState <- Finished (parameters, data) }
        
    let connectCommand =
        self.Factory.CommandAsyncChecked(
            connect,
            (fun () -> not self.Connected),
            [ <@ self.Connected @>])

    let disconnectCommand =
        self.Factory.CommandAsyncChecked(
            disconnect,
            (fun () -> self.Connected),
            [ <@ self.Connected @>])

    let saveCommand =
        self.Factory.CommandSyncChecked(
            save,
            (fun () -> match self.ExperimentState with Finished _ -> true | _ -> false),
            [ <@ self.ExperimentState @> ])

    let loadParametersCommand =
        self.Factory.CommandSyncChecked(
            loadParameters,
            (fun () -> self.IsReadyToStart),
            [ <@ self.IsReadyToStart @> ])

    let resetAxesCommand = self.Factory.CommandSync resetAxes

    let startExperimentCommand = self.Factory.CommandAsync performExperiment
        //
        //    (fun () -> self.IsValid && self.IsReadyToStart),
        //    [ <@ self.IsValid @> ; <@ self.IsReadyToStart @> ])

    let stopExperimentCommand =
        self.Factory.CommandSyncParamChecked(
            (function Running (_, cts) -> cts.Cancel() | _ -> ()), 
            (fun _ -> self.IsPerformingExperiment), 
            [ <@ self.IsPerformingExperiment @> ])

    let stopAfterScanCommand =
        self.Factory.CommandSyncParamChecked(
            (function Running (experiment, _) -> CwEprExperiment.stopAfterScan experiment | _ -> ()), 
            (fun _ -> self.IsPerformingExperiment), 
            [ <@ self.IsPerformingExperiment @> ])

    do 
        self.DependencyTracker.AddPropertyDependency(<@ self.Connected @>, <@ self.Connection @>)

        self.DependencyTracker.AddPropertyDependency(<@ self.CentreField @>,         <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.SweepWidth @>,          <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.FieldSweepDirection @>, <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.RampRateIndex @>,       <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.ConversionTime @>,      <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.QuadratureDetection @>, <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.NumberOfScans @>,       <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.SampleNotes @>,         <@ self.ExperimentParameters @>)
        self.DependencyTracker.AddPropertyDependency(<@ self.ExperimentNotes @>,     <@ self.ExperimentParameters @>)
        
        self.DependencyTracker.AddPropertyDependency(<@ self.IsPerformingExperiment @>, <@ self.ExperimentState @>)
        self.DependencyTracker.AddPropertyDependencies(<@@ self.IsReadyToStart @@>, 
            [ <@@ self.Connected @@> ; <@@ self.IsPerformingExperiment @@> ])

    member x.ExperimentParameters 
        with get()     = experimentParameters.Value 
        and  set value = experimentParameters.Value <- value

    member x.ExperimentState
        with get()     = experimentState.Value
        and  set value = experimentState.Value <- value

    member x.StatusMessage
        with get()     = statusMessage.Value
        and  set value = statusMessage.Value <- value

    member x.PlotModel : PlotModel = plotModel.Value
    member x.RePoints : ResizeArray<DataPoint> = reSeries.Points
    member x.ImPoints : ResizeArray<DataPoint> = imSeries.Points

    member x.IsPerformingExperiment = x.ExperimentState |> (function Running _ -> true | _ -> false)
    member x.IsReadyToStart = x.Connected && (not x.IsPerformingExperiment) 

    member x.CentreField 
        with get()     = Parameters.centreField x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withCentreField value x.ExperimentParameters

    member x.SweepWidth
        with get()     = Parameters.sweepWidth x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withSweepWidth value x.ExperimentParameters

    member x.FieldSweepDirection
        with get() = 
            Parameters.fieldSweepDirection x.ExperimentParameters
            |> (function Model.Increasing -> 0 | _ -> 1)

        and set value = 
            x.ExperimentParameters <- 
                if value = 0
                then Parameters.withFieldSweepDirection Model.Increasing x.ExperimentParameters
                else Parameters.withFieldSweepDirection Model.Decreasing x.ExperimentParameters

    member x.RampRateIndex
        with get() = 
            (Parameters.rampRate x.ExperimentParameters)
                / (abs <| MagnetController.Settings.linearFieldCoefficient magnetControllerSettings)
            |> MagnetController.Settings.RampRate.nearestIndex magnetControllerSettings

        and set value = 
            let rampRate = 
                (MagnetController.Settings.RampRate.fromIndex magnetControllerSettings value)
                * (abs <| MagnetController.Settings.linearFieldCoefficient magnetControllerSettings)
            x.ExperimentParameters <- Parameters.withRampRate rampRate x.ExperimentParameters

    member x.ConversionTime
        with get()     = Parameters.conversionTime x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withConversionTime value x.ExperimentParameters

    member x.QuadratureDetection
        with get()     = Parameters.quadratureDetection x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withQuadratureDetection value x.ExperimentParameters

    member x.NumberOfScans
        with get()     = Parameters.numberOfScans x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withNumberOfScans value x.ExperimentParameters

    member x.SampleNotes
        with get()     = Parameters.sampleNotes x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withSampleNotes value x.ExperimentParameters

    member x.ExperimentNotes
        with get()     = Parameters.experimentNotes x.ExperimentParameters
        and  set value = x.ExperimentParameters <- Parameters.withExperimentNotes value x.ExperimentParameters

    member x.MagneticFieldStep = MagnetController.Settings.fieldStep magnetControllerSettings
    member x.SweepWidthStep    = 2.0M * x.MagneticFieldStep
    member x.MinimumField      = MagnetController.Settings.Limit.lowerField magnetControllerSettings
    member x.MaximumField      = MagnetController.Settings.Limit.upperField magnetControllerSettings
    member x.MaximumFieldRange = x.MaximumField - x.MinimumField

    member x.AvailableRampRates =
        MagnetController.Settings.RampRate.availableValues magnetControllerSettings
        |> List.map ((*) (abs <| MagnetController.Settings.linearFieldCoefficient magnetControllerSettings))
        |> CollectionViewSource.GetDefaultView

    member x.StartExperiment = startExperimentCommand
    member x.StopExperiment = stopExperimentCommand
    member x.StopAfterScan = stopAfterScanCommand

    member x.Connection
        with get ()    = connection.Value
        and  set value = connection.Value <- value

    member x.Connected = match x.Connection with Connected _ -> true | Disconnected -> false

    member x.Connect = connectCommand
    member x.Disconnect = disconnectCommand
    member x.Save = saveCommand
    member x.LoadParameters = loadParametersCommand