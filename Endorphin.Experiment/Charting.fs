namespace Endorphin.Experiment

open System
open System.Runtime.InteropServices
open System.Reactive.Linq
open System.Threading
open FSharp.Charting
open Endorphin.Core

[<AutoOpen>]
/// Provides charting extension members for experiment workflows.
module ChartingExtensions =

    type CwEprExperimentWorker with

        /// Creates a real-time updating chart from the CwEprData event stream with a specified refresh interval.
        member experimentWorker.LiveChart
            (refreshInterval : TimeSpan,
             [<Optional; DefaultParameterValue("CW EPR experiment")>] title : string,
             [<Optional; DefaultParameterValue("Magnetic field (T)")>] xTitle : string,
             [<Optional; DefaultParameterValue("")>] yTitle : string) =
        
            // chart the real channel data
            let reData =
                (experimentWorker.DataUpdated
                    |> Event.map (fun data -> data.ReSignal()))
                    .Sample(refreshInterval)
                    .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe updates on the current synchronization context so that UI can be updated 
            let reChart = LiveChart.FastLine(reData, Name="Real signal", Title=title, XTitle=xTitle, YTitle=yTitle)

            // if quadrature detection is not enabled then only plot the real part of the signal
            if not experimentWorker.Experiment.QuadratureDetection then reChart
            else
                // otherwise also plot the imaginary part
                let imData =
                    (experimentWorker.DataUpdated
                        |> Event.map (fun data -> data.ImSignal()))
                        .Sample(refreshInterval)
                        .ObserveOn(SynchronizationContext.CaptureCurrent())
                let imChart = LiveChart.FastLine(imData, Name="Imaginary signal", Title=title, XTitle=xTitle, YTitle=yTitle)

                // and combine the plots
                Chart.Combine [ reChart ; imChart ]
