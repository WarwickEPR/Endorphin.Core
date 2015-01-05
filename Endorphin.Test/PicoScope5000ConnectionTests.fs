namespace Endorphin.Test.PicoScope5000

open Endorphin.Instrument.PicoScope5000
open Devices
open NUnit.Framework
open TestUtils

[<TestFixture>]
type ``PicoScope 5000 series connection tests``() = 
    let _ = log4netConfig()
    
    [<Test>]
    member this.``Can connect to first available PicoScope``() =
        let session = new PicoScope5000Session()
        
        async {
            use! pico = session.RequestControlAsync()
            do! pico.PingAsync()
            let! resolution = pico.GetDeviceResolutionAsync()
            Assert.AreEqual(Resolution._8bit, resolution) }
        |> Async.RunSynchronously

        session.CloseSessionAsync() |> Async.RunSynchronously

    [<Test>]
    member this.``Can connect to PicoScope by serial``() =
        let session = new PicoScope5000Session(picoScope5000serial)

        async {
            use! pico = session.RequestControlAsync()
            do! pico.PingAsync()
            let! resolution = pico.GetDeviceResolutionAsync()
            Assert.AreEqual(Resolution._8bit, resolution) }
        |> Async.RunSynchronously

        session.CloseSessionAsync() |> Async.RunSynchronously

    [<Test>]
    member this.``Can connect to first available PicoScope with specified resolution``() =
        let testResolution (resolution : Resolution) =
            let session = new PicoScope5000Session(resolution)

            async {
                use! pico = session.RequestControlAsync()
                do! pico.PingAsync()
                let! sessionResolution = pico.GetDeviceResolutionAsync()
                Assert.AreEqual(resolution, sessionResolution) }
            |> Async.RunSynchronously

            session.CloseSessionAsync() |> Async.RunSynchronously

        testResolution Resolution._12bit
        testResolution Resolution._15bit

    [<Test>]
    member this.``Can connect to PicoScope by serial with specified resolution``() =
        let testResolution (resolution : Resolution) =
            let session = new PicoScope5000Session(picoScope5000serial, resolution)

            async {
                use! pico = session.RequestControlAsync()
                do! pico.PingAsync()
                let! sessionResolution = pico.GetDeviceResolutionAsync()
                Assert.AreEqual(resolution, sessionResolution) }
            |> Async.RunSynchronously

            session.CloseSessionAsync() |> Async.RunSynchronously
        
        testResolution Resolution._8bit
        testResolution Resolution._14bit