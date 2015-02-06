namespace Endorphin.Test.PicoScope5000

open Config
open Endorphin.Instrument.PicoScope5000
open NUnit.Framework
open System
open System.Threading

[<TestFixture>]
type ``PicoScope 5000 series connection tests``() = 
    let _ = log4netConfig()
    
    [<Test>]
    member __.``Can connect to first available PicoScope``() =
        let session = new PicoScope5000Session()
        let cancellationCapability = new CancellationTokenSource()
        
        let workflow =
            async {
                do! session.ConnectAsync()
                use! pico = session.RequestControlAsync()
                do! pico.PingAsync()
                let! resolution = pico.GetDeviceResolutionAsync()
                Assert.AreEqual(Resolution._8bit, resolution) }

        Async.RunSynchronously(workflow, cancellationToken=cancellationCapability.Token)
        session.CloseSessionAsync() |> Async.RunSynchronously

    [<Test>]
    member __.``Can connect to PicoScope by serial``() =
        let session = new PicoScope5000Session(picoScope5000serial)
        let cancellationCapability = new CancellationTokenSource()
        
        let workflow =
            async {
                do! session.ConnectAsync()
                use! pico = session.RequestControlAsync()
                do! pico.PingAsync()
                let! resolution = pico.GetDeviceResolutionAsync()
                Assert.AreEqual(Resolution._8bit, resolution) }
        
        Async.RunSynchronously(workflow, cancellationToken=cancellationCapability.Token)
        session.CloseSessionAsync() |> Async.RunSynchronously

    [<Test>]
    member __.``Can connect to first available PicoScope with specified resolution``() =
        let testResolution (resolution : Resolution) =
            let session = new PicoScope5000Session(resolution)
            let cancellationCapability = new CancellationTokenSource()
            
            let workflow =
                async {
                    do! session.ConnectAsync()
                    use! pico = session.RequestControlAsync()
                    do! pico.PingAsync()
                    let! sessionResolution = pico.GetDeviceResolutionAsync()
                    Assert.AreEqual(resolution, sessionResolution) }
            
            Async.RunSynchronously(workflow, cancellationToken=cancellationCapability.Token)
            session.CloseSessionAsync() |> Async.RunSynchronously

        testResolution Resolution._12bit
        testResolution Resolution._15bit

    [<Test>]
    member __.``Can connect to PicoScope by serial with specified resolution``() =
        let testResolution (resolution : Resolution) =
            let session = new PicoScope5000Session(picoScope5000serial, resolution)
            let cancellationCapability = new CancellationTokenSource()
            
            let workflow =
                async {
                    do! session.ConnectAsync()
                    use! pico = session.RequestControlAsync()
                    do! pico.PingAsync()
                    let! sessionResolution = pico.GetDeviceResolutionAsync()
                    Assert.AreEqual(resolution, sessionResolution) }

            Async.RunSynchronously(workflow, cancellationToken=cancellationCapability.Token)
            session.CloseSessionAsync() |> Async.RunSynchronously
            
        testResolution Resolution._8bit
        testResolution Resolution._14bit