namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open TestUtils

[<TestFixture>]
type ``Magnet controller tests with prepared state``() = 
    let magnetController : MagnetController option ref = ref None
    let _ = log4netConfig()

    member this.MagnetController =
        match !magnetController with
        | Some(controller) -> controller
        | None -> raise (new NullReferenceException())

    [<SetUp>]
    member this.``Connect to magnet controller and initialise``() =
        magnetController := Some(new MagnetController(magnetControllerVisaAddress, magnetControllerParameters))
        initialiseDefaultMagnetControllerState this.MagnetController
        
    [<TearDown>]
    member this.``Disccnnect from magnet controller``() =
        (this.MagnetController :> IDisposable).Dispose()
        magnetController := None

    [<TestFixtureTearDown>]
    member this.``Connect return magnet controller to initial state after tests``() =
        magnetController := Some(new MagnetController(magnetControllerVisaAddress, magnetControllerParameters))
        initialiseDefaultMagnetControllerState this.MagnetController
        (this.MagnetController :> IDisposable).Dispose()
        magnetController := None
        
    [<Test>]
    member this.``Can change current direction``() =        
        this.MagnetController.SetCurrentDirection Reverse
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)

        this.MagnetController.SetCurrentDirection Forward
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)

    [<Test>]
    member this.``Can set lower set point``() =
        this.MagnetController.SetLowerSetPoint 2.0<A>
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(2.0<A>, setPointParams.lowerLimit)

        this.MagnetController.SetLowerSetPoint 4.0<A>
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(4.0<A>, setPointParams.lowerLimit)

    [<Test>]
    member this.``Can set upper set point``() =
        this.MagnetController.SetUpperSetPoint 1.0<A>
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(1.0<A>, setPointParams.upperLimit)

        this.MagnetController.SetUpperSetPoint 3.0<A>
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(3.0<A>, setPointParams.upperLimit)

    [<Test>]
    member this.``Can wait to reach target``() =
        this.MagnetController.SetLowerSetPoint 0.5<A>
        
        this.MagnetController.SetRampTarget Lower
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Lower, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

        this.MagnetController.SetRampTarget Zero
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

    [<Test>]
    member this.``Can ramp to zero``() =
        this.MagnetController.SetLowerSetPoint 0.5<A>
        
        this.MagnetController.SetRampTarget Lower
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Lower, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

        this.MagnetController.RampToZeroAsync() |> Async.RunSynchronously
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

    [<Test>]
    member this.``Can ramp to zero at ramp rate limit and change current direction``() =
        this.MagnetController.SetLowerSetPoint 0.5<A>
        
        this.MagnetController.SetRampTarget Lower
        this.MagnetController.SetRampRate 0.048<A/s>
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        Async.RunSynchronously(this.MagnetController.RampToZeroAndSetCurrentDirectionAsync Reverse)
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)
        Assert.AreEqual(0.098<A/s>, operatingParams.rampRate)

        this.MagnetController.SetRampRate 0.048<A/s>
        this.MagnetController.SetRampTarget Lower
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        Async.RunSynchronously(this.MagnetController.RampToZeroAndSetCurrentDirectionAsync Forward)
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)
        Assert.AreEqual(0.098<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Can wait to reach zero and change current direction``() =
        this.MagnetController.SetLowerSetPoint 0.5<A>
        this.MagnetController.SetRampRate 0.048<A/s>

        this.MagnetController.SetRampTarget Lower
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        this.MagnetController.SetRampTarget Zero
        Async.RunSynchronously(this.MagnetController.WaitToReachZeroAndSetCurrentDirectionAsync Reverse)
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)
        Assert.AreEqual(0.048<A/s>, operatingParams.rampRate)

        this.MagnetController.SetRampTarget Lower
        Async.RunSynchronously(this.MagnetController.WaitToReachTargetAsync())
        this.MagnetController.SetRampTarget Zero
        Async.RunSynchronously(this.MagnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward)
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)
        Assert.AreEqual(0.048<A/s>, operatingParams.rampRate)


