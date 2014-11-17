namespace Endorphin.Test

open Endorphin.Core.Utils
open Endorphin.Instrument.PicoScope5000
open Devices
open NUnit.Framework
open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Reactive.Linq
open TestUtils
open log4net

[<TestFixture>]
type ``PicoScope 5000 series streaming tests``() = 
    static let log = LogManager.GetLogger typeof<``PicoScope 5000 series streaming tests``>

    let picoScope : PicoScope5000 option ref = ref None
    let _ = log4netConfig()

    member this.PicoScope =
        match !picoScope with
        | Some(scope) -> scope
        | None -> raise (new NullReferenceException())

    [<TestFixtureSetUp>]
    member this.``Connect to PicoScope``() =
        picoScope := Some(new PicoScope5000())
        this.PicoScope.Error.Add(fun exn ->
            picoScope := None
            Assert.Fail(sprintf "PicoScope agent crashed due to error: %A" exn))

        let isMainsPowered =
            this.PicoScope.GetUnitIsMainsPoweredAsync()
            |> Async.RunSynchronously
        Assert.IsTrue(isMainsPowered, 
            "The PicoScope is not mains-powered. These unit test assume that the device has mains-power.")

    [<TestFixtureTearDown>]
    member this.``Disconnect from PicoScope``() =
        if (!picoScope).IsSome then
            this.PicoScope.PingAsync() |> Async.RunSynchronously 
            (this.PicoScope :> IDisposable).Dispose()
            picoScope := None

    [<TearDown>]
    member this.``Check response after test``() =
        // if the agent crashes then this will fail
        this.PicoScope.PingAsync() |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member this.``Can perform 1000 short streaming acquisitions with auto stop``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 5000u)
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(1000)
        |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with auto stop``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 6000000u)
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(10000)
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
        
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously
    
    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition at 10 Msps``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 600000000u)
                  sampleInterval = 1e-6<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(1000000)
        |> Observable.add (fun _ -> "Processed 1000000 samples." |> log.Debug)
        
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member this.``Can perform 1000 short streaming acquisitions with manual stop``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s> 
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(1000)
        |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
        "Waiting for 1s." |> log.Debug
        Thread.Sleep(1000)
        "Stopping acquisition stream" |> log.Debug
        
        let waitForCompletion = Observable.waitHandleForNext streamWorker.Completed
        streamWorker.Stop()
        Async.AwaitWaitHandle(waitForCompletion) |> Async.RunSynchronously |> ignore

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with manual stop``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(10000)
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
        
        "Waiting for 10 min." |> log.Debug
        Thread.Sleep(600000)
        "Stopping acquisition stream" |> log.Debug
        
        let waitForCompletion = Observable.waitHandleForNext streamWorker.Completed
        streamWorker.Stop()
        Async.AwaitWaitHandle(waitForCompletion) |> Async.RunSynchronously |> ignore
    
    [<Test>]
    member this.``Can perform stream sampling all input channels``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full })  
              (Channel.B, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full })  
              (Channel.C, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full })  
              (Channel.D, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let w = ChannelData(Channel.A, Downsampling.None)
        let x = ChannelData(Channel.B, Downsampling.None)
        let y = ChannelData(Channel.C, Downsampling.None)
        let z = ChannelData(Channel.D, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 100000u)
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ w ; x ; y; z ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(10000)
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
        
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously
    
    [<Test>]
    member this.``Can prepare stream and cancel without starting``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        let waitForReady = 
            streamWorker.StatusChanged
            |> Observable.filter (fun status -> status = ReadyToStream)
            |> Observable.waitHandleForNext  
        streamWorker.Prepare()
        Async.AwaitWaitHandle(waitForReady) |> Async.RunSynchronously |> ignore
        
        let waitForCompletion = Observable.waitHandleForNext streamWorker.Completed
        streamWorker.Stop()
        Async.AwaitWaitHandle(waitForCompletion) |> Async.RunSynchronously |> ignore

    [<Test>]
    member this.``Can prepare stream then start``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 100000u)
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 0.2<s>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        let waitForReady = 
            streamWorker.StatusChanged
            |> Observable.filter (fun status -> status = ReadyToStream)
            |> Observable.waitHandleForNext  
        streamWorker.Prepare()
        Async.AwaitWaitHandle(waitForReady) |> Async.RunSynchronously |> ignore

        do
            use failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            Thread.Sleep(3000)
        
        streamWorker.SetReadyToStart()
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously |> ignore

    // TODO:
    // - downsampling
    // - aggregate sampling
