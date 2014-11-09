namespace Endorphin.Test

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System

[<TestFixture>]
type ``Magnet controller tests with prepared state``() = 
    let mutable magnetControllerOpt : MagnetController option = None
    member this.magnetController 
        with get() : MagnetController = magnetControllerOpt.Value
        and set(value : MagnetController) = magnetControllerOpt <- Some(value)

    [<SetUp>]
    member this.``Connect to magnet controller and initialise``() =
        this.magnetController <-
            new MagnetController(magnetControllerVisaAddress, magnetControllerParameters)
        initialiseDefaultMagnetControllerState this.magnetController
        
    [<TearDown>]
    member this.``Disccnnect from magnet controller``() =
        (this.magnetController :> IDisposable).Dispose()

    [<TestFixtureTearDown>]
    member this.``Connect return magnet controller to initial state after tests``() =
        this.magnetController <-
            new MagnetController(magnetControllerVisaAddress, magnetControllerParameters)
        initialiseDefaultMagnetControllerState this.magnetController
        (this.magnetController :> IDisposable).Dispose()

    [<Test>]
    member this.``Can change current direction``() =        
        this.magnetController.SetCurrentDirection Reverse
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)

        this.magnetController.SetCurrentDirection Forward
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)

    [<Test>]
    member this.``Can set lower set point``() =
        this.magnetController.SetLowerSetPoint 2.0<A>
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(2.0<A>, setPointParams.lowerLimit)

        this.magnetController.SetLowerSetPoint 4.0<A>
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(4.0<A>, setPointParams.lowerLimit)

    [<Test>]
    member this.``Can set upper set point``() =
        this.magnetController.SetUpperSetPoint 1.0<A>
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(1.0<A>, setPointParams.upperLimit)

        this.magnetController.SetUpperSetPoint 3.0<A>
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(3.0<A>, setPointParams.upperLimit)

    [<Test>]
    member this.``Can wait to reach target``() =
        this.magnetController.SetLowerSetPoint 0.5<A>
        
        this.magnetController.SetRampTarget Lower
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Lower, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

        this.magnetController.SetRampTarget Zero
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

    [<Test>]
    member this.``Can ramp to zero``() =
        this.magnetController.SetLowerSetPoint 0.5<A>
        
        this.magnetController.SetRampTarget Lower
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Lower, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

        this.magnetController.RampToZeroAsync() |> Async.RunSynchronously
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)
        Assert.IsTrue(currentParams.reachedTarget)

    [<Test>]
    member this.``Can ramp to zero at ramp rate limit and change current direction``() =
        this.magnetController.SetLowerSetPoint 0.5<A>
        
        this.magnetController.SetRampTarget Lower
        this.magnetController.SetRampRate 0.048<A/s>
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        Async.RunSynchronously(this.magnetController.RampToZeroAndSetCurrentDirectionAsync Reverse)
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)
        Assert.AreEqual(0.098<A/s>, operatingParams.rampRate)

        this.magnetController.SetRampRate 0.048<A/s>
        this.magnetController.SetRampTarget Lower
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        Async.RunSynchronously(this.magnetController.RampToZeroAndSetCurrentDirectionAsync Forward)
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)
        Assert.AreEqual(0.098<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Can wait to reach zero and change current direction``() =
        this.magnetController.SetLowerSetPoint 0.5<A>
        this.magnetController.SetRampRate 0.048<A/s>

        this.magnetController.SetRampTarget Lower
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        this.magnetController.SetRampTarget Zero
        Async.RunSynchronously(this.magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Reverse)
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Reverse, operatingParams.currentDirection)
        Assert.AreEqual(0.048<A/s>, operatingParams.rampRate)

        this.magnetController.SetRampTarget Lower
        Async.RunSynchronously(this.magnetController.WaitToReachTargetAsync())
        this.magnetController.SetRampTarget Zero
        Async.RunSynchronously(this.magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward)
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(Forward, operatingParams.currentDirection)
        Assert.AreEqual(0.048<A/s>, operatingParams.rampRate)


