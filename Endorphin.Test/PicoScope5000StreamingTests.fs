namespace Endorphin.Test.PicoScope5000

open Config
open Endorphin.Core.Units
open Endorphin.Core.ObservableExtensions
open Endorphin.Instrument.PicoScope5000
open log4net
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System.Reactive.Linq

[<TestFixture>]
type ``PicoScope 5000 series streaming tests``() = 
    static let log = LogManager.GetLogger typeof<``PicoScope 5000 series streaming tests``>

    let picoScopeSession = new PicoScope5000Session(picoScope5000serial)
    let _ = log4netConfig()
        
    [<TestFixtureSetUp>]
    member __.``Connect to PicoScope``() =
        picoScopeSession.ConnectAsync() |> Async.RunSynchronously

    [<TestFixtureTearDown>]
    member __.``Disconnect from PicoScope``() =
        picoScopeSession.CloseSessionAsync() |> Async.RunSynchronously

    [<TearDown>]
    member __.``Check response after test``() =
        // if the agent crashes then this will fail
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            do! pico.PingAsync() } 
        |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member __.``Can perform 1000 short streaming acquisitions with auto stop``() =
        async { 
            use! pico = picoScopeSession.RequestControlAsync()

            let x = (Channel.A, { 
                InputSettings =  { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 5000u)
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()

            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
            streamWorker.PrepareAndStart()    
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns>; FinishedStream true ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform long-running (10min) streaming acquisition with auto stop``() =
        async { 
            use! pico = picoScopeSession.RequestControlAsync()
             
            let x = (Channel.A, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 6000000u)
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()

            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)

            streamWorker.PrepareAndStart()    
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                statusReplay.ToEnumerable() |> Seq.toList) } 
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform long-running (10min) streaming acquisition at 1 Msps``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let x = (Channel.A, { 
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 600000000u)
                      SampleInterval = 1000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()
        
            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 1000000
            |> Observable.add (fun _ -> "Processed 1000000 samples." |> log.Debug)

            streamWorker.PrepareAndStart()    
            do! statusReplay.AwaitLastAsync() |> Async.Ignore
            
            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 1000<ns> ; FinishedStream true ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    [<Repeat(1000)>]
    member __.``Can perform 1000 short streaming acquisitions with manual stop``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let x = (Channel.A, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full}
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = ManualStop
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()

            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 1000
            |> Observable.add (fun _ -> "Processed 1000 samples." |> log.Debug)
        
            streamWorker.PrepareAndStart()
            "Waiting for 1s." |> log.Debug
            do! Async.Sleep 1000

            "Stopping acquisition stream" |> log.Debug
            streamWorker.Stop()
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform long-running (10min) streaming acquisition with manual stop``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
        
            let x = (Channel.A, { 
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList }) 
        
            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = ManualStop
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()

            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)
                          
            streamWorker.PrepareAndStart()

            "Waiting for 10 min." |> log.Debug
            do! Async.Sleep 600000
        
            "Stopping acquisition stream" |> log.Debug
            streamWorker.Stop()
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform stream sampling all input channels``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let w = (Channel.A, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let x = (Channel.B, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let y = (Channel.C, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
            
            let z = (Channel.D, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 100000u)
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ w ; x ; y ; z ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()
                                            
            streamWorker.SampleSliceObserved
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 sample slices." |> log.Debug)
            
            streamWorker.PrepareAndStart()
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare stream and cancel without starting``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
        
            let x = 
                (Channel.A, { 
                    InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                    DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })

            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = ManualStop
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
                
            let statusReplay = streamWorker.StatusChanged.Catch(fun _ -> Observable.Empty()).Replay()
            use _ = statusReplay.Connect()

            streamWorker.Prepare()
            do! statusReplay.AwaitFirstAsync ((=) ReadyToStream) |> Async.Ignore

            sprintf "Status list before : %A" (statusReplay.Take(2).ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ],
                statusReplay.Take(2).ToEnumerable() |> Seq.toList)
        
            let failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            do! Async.Sleep 1000
            failIfStatusChanged.Dispose()
            
            streamWorker.Stop()

            let! finalStatus = statusReplay.AwaitLastAsync()

            Assert.IsTrue(
                match finalStatus with
                | CanceledStream _ -> true
                | _ -> false)

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ],
                statusReplay.SkipLast(1).ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare stream then start``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
        
            let x = (Channel.A, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 100000u)
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()
                  
            streamWorker.SampleObserved { Channel = Channel.A; BufferDownsampling = NoDownsampling }
            |> Observable.buffer 10000
            |> Observable.add (fun _ -> "Processed 10000 samples." |> log.Debug)

            streamWorker.Prepare()
            do! statusReplay.AwaitFirstAsync ((=) ReadyToStream) |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.Take(2).ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ],
                statusReplay.Take(2).ToEnumerable() |> Seq.toList)

            let failIfStatusChanged =
                streamWorker.StatusChanged
                |> Observable.subscribe (fun status -> 
                    (sprintf "Stream status changed to %A after ReadyToStream but before call to SetReadyToStart." status) |> Assert.Fail)

            do! Async.Sleep 3000
            failIfStatusChanged.Dispose()

            streamWorker.SetReadyToStart()
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream true ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can manually stop auto-stop stream``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let x = (Channel.A, {
                InputSettings = { Coupling = Coupling.DC; Range = Range._5V; AnalogueOffset = 0.0<V>; BandwidthLimit = BandwidthLimit.Full }
                DownsamplingModes = [ DownsamplingMode.None ] |> Set.ofList })
        
            let streamWorker = 
                new StreamWorker(pico,
                    { StreamStop = AutoStop(0u, 600000u)
                      SampleInterval = 100000<ns>
                      DownsamplingRatio = None
                      TriggerSettings = AutoTrigger 200s<ms>
                      ActiveChannels = [ x ] |> Map.ofList
                      MemorySegment = 0u })
        
            let statusReplay = streamWorker.StatusChanged.Replay()
            use _ = statusReplay.Connect()

            streamWorker.PrepareAndStart()
            do! statusReplay.AwaitFirstAsync ((=) (Streaming 100000<ns>)) |> Async.Ignore
            do! Async.Sleep 5000

            streamWorker.Stop()
            do! statusReplay.AwaitLastAsync() |> Async.Ignore

            sprintf "Status list: %A" (statusReplay.ToEnumerable() |> Seq.toList) |> log.Debug
            Assert.AreEqual(
                [ PreparingStream ; ReadyToStream ; Streaming 100000<ns> ; FinishedStream false ],
                statusReplay.ToEnumerable() |> Seq.toList) }
        |> Async.RunSynchronously

    // TODO:
    // - downsampling
    // - aggregate sampling
