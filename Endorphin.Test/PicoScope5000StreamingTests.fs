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

[<TestFixture>]
type ``PicoScope 5000 series streaming tests``() = 
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
    member this.``Can run streaming acquisition``() =
        let streamChannels =  
            [ (Channel.A, { coupling = Coupling.DC; range = Range._5V; analogueOffset = 0.0<V>; bandwidthLimit = BandwidthLimit.Full }) ] 
        
        let x = ChannelData(Channel.A, Downsampling.None)

        let streamWorker = 
            new StreamWorker(this.PicoScope,
                { streamStop = AutoStop(0u, 5000000u)
                  sampleInterval = 1e-4<s>
                  downsamplingRatio = 1u
                  triggerSettings = AutoTrigger(0.2<s>) 
                  activeChannels = streamChannels
                  channelStreams = [ x ]
                  channelAggregateStreams = []
                  memorySegment = 0u })

        streamWorker.PrepareAndStart()

        x.Samples.Buffer(100000)
        |> Observable.add (fun _ -> printfn "Processed 100000 samples.")
        
        Async.AwaitEvent(streamWorker.Completed) |> Async.RunSynchronously

    // TODO:
    // - many repeats of short stream with manual and auto stop
    // - long running stream with manual and auto stop
    // - multiple channels
    // - downsampling
    // - aggregate sampling