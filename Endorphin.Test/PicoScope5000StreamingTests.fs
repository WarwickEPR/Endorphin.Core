namespace Endorphin.Test.PicoScope5000

open Endorphin.Core.Units
open Endorphin.Core.ObservableExtensions
open Endorphin.Instrument.PicoScope5000
open NUnit.Framework
open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Reactive.Linq
open Config
open log4net

[<TestFixture>]
type ``PicoScope 5000 series streaming tests``() = 
    static let log = LogManager.GetLogger typeof<``PicoScope 5000 series streaming tests``>

    let picoScopeSession = new PicoScope5000Session(picoScope5000serial)
    let _ = log4netConfig()

    [<TestFixtureTearDown>]
    member this.``Disconnect from PicoScope``() =
        picoScopeSession.CloseSessionAsync() |> Async.RunSynchronously

    [<TearDown>]
    member this.``Check response after test``() =
        // if the agent crashes then this will fail
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            do! pico.PingAsync() } 
        |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member this.``Can perform 1000 short streaming acquisitions with auto stop``() =
        async { 
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let x = (Channel.A, { 
                inputSettings =  { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 5000u)
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
        
            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild 

            streamWorker.PrepareAndStart()    
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns>; FinishedStream true ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with auto stop``() =
        async { 
            use! pico = picoScopeSession.RequestControlAsync()
             
            let streamStatusList = ref List.empty
            let cancellingDidFire = ref false
        
            let x = (Channel.A, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 6000000u)
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
        
            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)

            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> cancellingDidFire := true)

            let! waitForSuccess =
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild 

            streamWorker.PrepareAndStart()    
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                List.rev !streamStatusList )
            Assert.IsFalse !cancellingDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition at 1 Msps``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let x = (Channel.A, { 
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 600000000u)
                      sampleInterval = 1000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
        
            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 1000000
            |> Observable.add (fun _ -> "Processed 1000000 samples." |> log.Debug)

            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild 

            streamWorker.PrepareAndStart()    
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 1000<ns> ; FinishedStream true ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member this.``Can perform 1000 short streaming acquisitions with manual stop``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false

            let x = (Channel.A, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full}
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = ManualStop
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })

            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            streamWorker.PrepareAndStart()
            "Waiting for 1s." |> log.Debug
            do! Async.Sleep 1000

            let! waitForSuccess =
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild

            "Stopping acquisition stream" |> log.Debug
            streamWorker.Stop()
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can perform long-running (10min) streaming acquisition with manual stop``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let x = (Channel.A, { 
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList }) 
        
            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = ManualStop
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })

            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
                          
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            streamWorker.PrepareAndStart()

            "Waiting for 10 min." |> log.Debug
            do! Async.Sleep 600000
        
            let! waitForSuccess = 
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild

            "Stopping acquisition stream" |> log.Debug
            streamWorker.Stop()
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can perform stream sampling all input channels``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let w = (Channel.A, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let x = (Channel.B, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let y = (Channel.C, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let z = (Channel.D, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 100000u)
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ w ; x ; y ; z ] |> Map.ofList
                      memorySegment = 0u })
                                            
            streamWorker.SampleSlice
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 sample slices." |> log.Debug)
            
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess = 
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild

            streamWorker.PrepareAndStart()
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can prepare stream and cancel without starting``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let x = 
                (Channel.A, { 
                    inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                    downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = ManualStop
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
        
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReady = 
                streamWorker.StatusChanged
                |> Event.filter ((=) ReadyToStream)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
                    
            streamWorker.Prepare()
            do! waitForReady

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire
        
            let failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            do! Async.Sleep 1000
            failIfStatusChanged.Dispose()
            
            let! waitForCanceled =
                streamWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            streamWorker.Stop()
            do! waitForCanceled

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; CanceledStream ],
                List.rev !streamStatusList)
            Assert.IsTrue !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can prepare stream then start``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false
        
            let x = (Channel.A, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 100000u)
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
                  
            streamWorker.Sample(Channel.A, AllSamples)
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)

            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForReady = 
                streamWorker.StatusChanged
                |> Event.filter ((=) ReadyToStream)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            streamWorker.Prepare()
            do! waitForReady

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire

            let failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            do! Async.Sleep 3000
            failIfStatusChanged.Dispose()

            let! waitForSuccess = 
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild

            streamWorker.SetReadyToStart()
            do! waitForSuccess

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can manually stop auto-stop stream``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let streamStatusList = ref List.empty
            let canceledDidFire = ref false

            let x = (Channel.A, {
                inputSettings = { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }
                downsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { streamStop = AutoStop(0u, 600000u)
                      sampleInterval = 100000<ns>
                      downsampling = None
                      triggerSettings = AutoTrigger 200s<ms>
                      activeChannels = [ x ] |> Map.ofList
                      memorySegment = 0u })
                  
            streamWorker.StatusChanged.Add(fun newStatus -> streamStatusList := newStatus :: !streamStatusList)
            streamWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForStreaming = 
                streamWorker.StatusChanged
                |> Event.filter ((=) (Streaming 100000<ns>))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            streamWorker.PrepareAndStart()
            do! waitForStreaming
            do! Async.Sleep 5000

            let! waitForSuccess =
                streamWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild

            streamWorker.Stop()
            waitForSuccess |> Async.RunSynchronously

            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                List.rev !streamStatusList)
            Assert.IsFalse !canceledDidFire }
        |> Async.RunSynchronously

    // TODO:
    // - downsampling
    // - aggregate sampling
