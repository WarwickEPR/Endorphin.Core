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
        use pico = new PicoScope5000()
        pico.PingAsync() |> Async.RunSynchronously

        let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
        Assert.AreEqual(Resolution._8bit, resolution)
        let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
        Assert.IsTrue(isMainsPowered, 
            "The PicoScope is not mains-powered. These unit test assume that the device has mains-power.")

    [<Test>]
    member this.``Can connect to PicoScope by serial``() =
        use pico = new PicoScope5000(picoScope5000serial)
        pico.PingAsync() |> Async.RunSynchronously

        let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
        Assert.AreEqual(Resolution._8bit, resolution)
        let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
        Assert.IsTrue(isMainsPowered, 
            "The PicoScope is not mains-powered. These unit test assume that the device has mains-power.")

    [<Test>]
    member this.``Can connect to first available PicoScope with specified resolution``() =
        using (new PicoScope5000(Resolution._12bit)) (fun pico ->
            pico.PingAsync() |> Async.RunSynchronously

            let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
            Assert.AreEqual(Resolution._12bit, resolution)            
            let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
            Assert.IsTrue(isMainsPowered, 
                "The PicoScope is not mains-powered. These unit test assume that the device has mains-power."))

        using (new PicoScope5000(Resolution._15bit)) (fun pico ->
            pico.PingAsync() |> Async.RunSynchronously
            
            let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
            Assert.AreEqual(Resolution._15bit, resolution)            
            let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
            Assert.IsTrue(isMainsPowered, 
                "The PicoScope is not mains-powered. These unit test assume that the device has mains-power."))

    [<Test>]
    member this.``Can connect to PicoScope by serial with specified resolution``() =
        using (new PicoScope5000(picoScope5000serial, Resolution._8bit)) (fun pico ->
            pico.PingAsync() |> Async.RunSynchronously

            let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
            Assert.AreEqual(Resolution._8bit, resolution)            
            let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
            Assert.IsTrue(isMainsPowered, 
                "The PicoScope is not mains-powered. These unit test assume that the device has mains-power."))

        using (new PicoScope5000(picoScope5000serial, Resolution._14bit)) (fun pico ->
            pico.PingAsync() |> Async.RunSynchronously
            
            let resolution = pico.GetDeviceResolutionAsync() |> Async.RunSynchronously
            Assert.AreEqual(Resolution._14bit, resolution)            
            let isMainsPowered = pico.GetUnitIsMainsPoweredAsync() |> Async.RunSynchronously
            Assert.IsTrue(isMainsPowered, 
                "The PicoScope is not mains-powered. These unit test assume that the device has mains-power."))