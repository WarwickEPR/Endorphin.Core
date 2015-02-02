namespace Endorphin.Instrument.PicoScope5000

open System
open System.Runtime.InteropServices
open System.Reactive.Linq
open System.Threading
open FSharp.Charting
open Endorphin.Core
open System.Runtime.CompilerServices

[<Extension>]
/// Provides charting extension members for PicoScope 5000 series acquisition workflows.
/// TODO: convert samples to voltage vs. time and voltage vs. voltage respectively.
module ChartingExtensions =

    [<Extension>]
    /// Creates an animated chart for the specified ChannelBuffer in a StreamWorker acquisition. The chart data
    /// refreshes with the speicifed interval. Default title and axis titles are blank. The default series name is 
    /// chosen according to the ChannelBuffer by default. Use an FSharp.Charting.ChartTypes.ChartControl to display
    /// the animated chart in a user interface.
    let LiveChartStream
       (streamWorker : StreamWorker,
        channelBuffer : ChannelBuffer, 
        refreshInterval : TimeSpan,
        [<Optional; DefaultParameterValue(null)>] name : string,
        [<Optional; DefaultParameterValue("")>] title : string,
        [<Optional; DefaultParameterValue("")>] xTitle : string,
        [<Optional; DefaultParameterValue("")>] yTitle : string) =

        // create an event stream which will contain the accumulated data set
        let chartData = 
           (streamWorker.SampleObserved channelBuffer // take the SampleObserved event for the specified ChannelBuffer
            |> Observable.scan (fun chartData x -> // accumulate the samples in a list
                match chartData with
                | [] -> [(0, x)] // if the list is empty then add the first sample with index zero
                | (lastIndex, _) :: _ -> (lastIndex + 1, x) :: chartData) List.empty) // otherwise prepend the sample, incrementing the index
            .Sample(refreshInterval) // refresh with the specified interval
            .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe the events on the UI thread synchronisation context
        
        // if no name is speicified, use the default description for the ChannelBuffer and create the live chart
        let seriesName = if name = null then channelBuffer.ToString() else name
        LiveChart.FastLine(chartData, Name=seriesName, Title=title, XTitle=xTitle, YTitle=yTitle)

    [<Extension>]
    /// Creates an animated XY chart for the specfieid ChannelBuffers, representing the X and Y data respectively, in
    /// a StreamWorker acquisition. The chart data refreshes with the specified interval. Default title and axis titles
    /// are blank. The default series name is chosen according to the ChannelBuffers by default. Use an
    /// FSharp.Charting.ChartTypes.ChartControl to display the animated chart in a user interface.
    let LiveChartStreamXY
       (streamWorker : StreamWorker,
        channelBufferX : ChannelBuffer,
        channelBufferY : ChannelBuffer, 
        refreshInterval : TimeSpan,
        [<Optional; DefaultParameterValue(null)>] name : string,
        [<Optional; DefaultParameterValue("")>] title : string,
        [<Optional; DefaultParameterValue("")>] xTitle : string,
        [<Optional; DefaultParameterValue("")>] yTitle : string) =

        // create an event stream which will contain the accumulated data set
        let chartData =
           (streamWorker.SampleBlockObserved // take each SampleBlockObserved event
            |> Observable.scan (fun chartData block ->
                // take all pairs of XY values for each sample in the specified buffers in the block
                let samples = seq {
                    for index in 0 .. block.Length - 1 ->
                       (block.Samples.[channelBufferX].[index],
                        block.Samples.[channelBufferY].[index]) }
                
                // accumulate the samples in a list, starting with an empty list
                Seq.fold (fun data sample -> sample :: data) chartData samples) List.empty)
             .Sample(refreshInterval) // refresh with the specified interval
             .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe the events on the UI thread synchronisation context
        
        // if no name is specified, use the default description for the ChannelBuffers and create the live chart
        let seriesName =
            if name = null then sprintf "%s vs. %s" (channelBufferY.ToString()) (channelBufferX.ToString())
            else name
        
        LiveChart.FastLine(chartData, Name=seriesName, Title=title, XTitle=xTitle, YTitle=yTitle)
    
    [<Extension>]
    /// Creates an animated hart for all acquired ChannelBuffers in a StreamWorker acquisition. The chart data refreshes with
    /// the specified interval. Default title and axis titles are blank. By default, each series is labelled with a name
    /// chosen according to the ChannelBuffer. Use an FSharp.Charting.ChartTypes.ChartControl to display the animated chart
    /// in a user interface. 
    let LiveChartAllStreams
       (streamWorker : StreamWorker,
        refreshInterval : TimeSpan,
        [<Optional; DefaultParameterValue(true)>] showNames : bool,
        [<Optional; DefaultParameterValue("")>] title : string,
        [<Optional; DefaultParameterValue("")>] xTitle : string,
        [<Optional; DefaultParameterValue("")>] yTitle : string) =

        // create an event which will fire at the refresh interval
        let sampler = Observable.Interval refreshInterval

        seq {
            // compute the channel buffers in the stream's active channels
            for (channel, channelStream) in Map.toSeq streamWorker.Stream.ActiveChannels do
                for downsamplingMode in channelStream.DownsamplingModes do
                    match downsamplingMode.BufferFormat with
                    | Single bufferDownsampling ->
                        yield { Channel = channel; BufferDownsampling = bufferDownsampling }
                    | Pair (bufferDownsamplingMax, bufferDownsamplingMin) ->
                        yield { Channel = channel; BufferDownsampling = bufferDownsamplingMax }
                        yield { Channel = channel; BufferDownsampling = bufferDownsamplingMin } }
        |> Seq.map (fun channelBuffer -> 
            // for each ChannelBuffer in the sequence
            let chartData =
               (streamWorker.SampleObserved channelBuffer // take each SampleObserved event
                    |> Observable.scan (fun chartData x -> // accumulate the samples in a list
                        match chartData with
                        | [] -> [(0, x)] // if the list is empty then add the first sample with zero index
                        | (last, _) :: _ -> (last + 1, x) :: chartData) List.empty) // otherwise prepend the sample, incrementing the index
                .Sample(sampler) // sample using the sampler event
                .ObserveOn(SynchronizationContext.CaptureCurrent()) // observe the events on the UI thread synchronisation context
            
            // and create the corresponding live chart
            if showNames then LiveChart.FastLine(chartData, Name=channelBuffer.ToString(), Title=title, XTitle=xTitle, YTitle=yTitle)
            else LiveChart.FastLine(chartData, Title=title, XTitle=xTitle, YTitle=yTitle))
        |> Chart.Combine // then combine all the charts