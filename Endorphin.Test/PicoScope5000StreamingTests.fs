namespace Endorphin.Test.PicoScope5000

open Endorphin.Core.Units
open Endorphin.Core.ObservableExtensions
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
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 5000u)
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
        
        x.Samples.Buffer(1000)
        |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild 
            |> Async.RunSynchronously

        streamWorker.PrepareAndStart()    
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns>; FinishedStream(true) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with auto stop``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 6000000u)
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
        
        x.Samples.Buffer(1000)
        |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)

        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild 
            |> Async.RunSynchronously

        streamWorker.PrepareAndStart()    
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(true) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition at 1 Msps``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 600000000u)
                  sampleInterval = 1000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
        
        x.Samples.Buffer(1000000)
        |> Observable.add (fun _ -> "Processed 1000000 samples." |> log.Debug)

        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild 
            |> Async.RunSynchronously

        streamWorker.PrepareAndStart()    
        waitForCompleted |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 1000<ns> ; FinishedStream(true) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    [<Repeat(1000)>]
    member this.``Can perform 1000 short streaming acquisitions with manual stop``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false

        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        x.Samples.Buffer(1000)
        |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)
        
        streamWorker.PrepareAndStart()
        "Waiting for 1s." |> log.Debug
        Thread.Sleep(1000)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        "Stopping acquisition stream" |> log.Debug
        streamWorker.Stop()
        waitForCompleted |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(false) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with manual stop``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        x.Samples.Buffer(10000)
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
                          
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)
        
        streamWorker.PrepareAndStart()

        "Waiting for 10 min." |> log.Debug
        Thread.Sleep(600000)
        
        let waitForCompleted = 
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        "Stopping acquisition stream" |> log.Debug
        streamWorker.Stop()
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(false) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can perform stream sampling all input channels``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
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
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 100000u)
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ w ; x ; y ; z ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
                                            
        (w.Samples, x.Samples, y.Samples, z.Samples)
        |> Observable.zip4
        |> Observable.buffer 10000
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
            
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForCompleted = 
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.PrepareAndStart()
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(true) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can prepare stream and cancel without starting``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = ManualStop
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
        
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)
        
        let waitForReady = 
            streamWorker.StatusChanged
            |> Event.filter ((=) ReadyToStream)
            |> Async.AwaitEvent
            |> Async.Ignore
            |> Async.StartChild
            |> Async.RunSynchronously
                    
        streamWorker.Prepare()
        waitForReady |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)
        
        do
            use failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            Thread.Sleep(3000)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.Stop()
        waitForCompleted |> Async.RunSynchronously |> ignore

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream |], !streamStatusArray )
        Assert.IsTrue(!cancellingDidFire)

    [<Test>]
    member this.``Can prepare stream then start``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false
        
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 100000u)
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
                  
        x.Samples.Buffer(10000)
        |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)

        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForReady = 
            streamWorker.StatusChanged
            |> Event.filter ((=) ReadyToStream)
            |> Async.AwaitEvent
            |> Async.Ignore
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.Prepare()
        waitForReady |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

        do
            use failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            Thread.Sleep(3000)
        
        let waitForCompleted = 
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.SetReadyToStart()
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(true) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    [<Test>]
    member this.``Can manually stop auto-stop stream``() =
        let streamStatusArray = ref Array.empty
        let cancellingDidFire = ref false

        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(
                this.PicoScope,
                { streamStop = AutoStop(0u, 600000u)
                  sampleInterval = 100000<ns>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger 200s<ms>
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })
                  
        streamWorker.StatusChanged.Add(fun newStatus -> 
            streamStatusArray := Array.append !streamStatusArray [| newStatus |] )

        streamWorker.Cancelling.Add(fun _ -> cancellingDidFire := true)

        let waitForStreaming = 
            streamWorker.StatusChanged
            |> Event.filter ((=) (Streaming 100000<ns>))
            |> Async.AwaitEvent
            |> Async.Ignore
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.PrepareAndStart()
        waitForStreaming |> Async.RunSynchronously
        Thread.Sleep(5000)

        let waitForCompleted =
            streamWorker.Completed
            |> Async.AwaitEvent
            |> Async.StartChild
            |> Async.RunSynchronously

        streamWorker.Stop()
        waitForCompleted |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream(false) |], !streamStatusArray )
        Assert.IsFalse(!cancellingDidFire)

    // TODO:
    // - downsampling
    // - aggregate sampling
