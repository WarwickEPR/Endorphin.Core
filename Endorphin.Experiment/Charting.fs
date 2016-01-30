// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Experiment

open System
open System.Runtime.InteropServices
open System.Reactive.Linq
open System.Threading
open FSharp.Charting
open Endorphin.Core
open System.Runtime.CompilerServices

[<Extension>]
/// Provides charting extension members for experiment workflows.
module ChartingExtensions =

    [<Extension>]
    /// Creates a real-time updating chart from the CwEprData event stream with a specified refresh interval.
    let LiveChartExperiment
        (experimentWorker : CwEprExperimentWorker,
         refreshInterval : TimeSpan,
         [<Optional; DefaultParameterValue("CW EPR experiment")>] title : string,
         [<Optional; DefaultParameterValue("Magnetic field (T)")>] xTitle : string,
         [<Optional; DefaultParameterValue("")>] yTitle : string) =
        
        // create an event which will fire at the refresh interval
        let sampler = Observable.Interval refreshInterval

        // chart the real channel data
        let reData =
            (experimentWorker.DataUpdated
                .Select(fun data -> data.ReSignal()))
                .Sample(sampler)
                .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe updates on the current synchronization context so that UI can be updated 
        let reChart = LiveChart.FastLine(reData, Name="Real signal", Title=title, XTitle=xTitle, YTitle=yTitle)

        // if quadrature detection is not enabled then only plot the real part of the signal
        if not experimentWorker.Experiment.QuadratureDetection then reChart
        else
            // otherwise also plot the imaginary part
            let imData =
                (experimentWorker.DataUpdated
                    .Select(fun data -> data.ImSignal()))
                    .Sample(sampler)
                    .ObserveOn(SynchronizationContext.CaptureCurrent())
            let imChart = LiveChart.FastLine(imData, Name="Imaginary signal", Title=title, XTitle=xTitle, YTitle=yTitle)

            // and combine the plots
            Chart.Combine [ reChart ; imChart ]
