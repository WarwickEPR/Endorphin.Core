namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open TestUtils

[<TestFixture>]
type ``Magnet controller tests with prepared state``() = 
    let magnetControllerSession = new MagnetControllerSession(magnetControllerVisaAddress, magnetControllerParameters)
    let _ = log4netConfig()

    [<SetUp>]
    member __.``Prepare magnet controller state``() = 
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously

    [<TestFixtureTearDown>]
    member __.``Close magnet controller session``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously

        magnetControllerSession.CloseSessionAsync() 
        |> Async.RunSynchronously
        
    [<Test>]
    member __.``Can change current direction``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()      
            
            magnetController.SetCurrentDirection Reverse
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Reverse, operatingParams.currentDirection)

            magnetController.SetCurrentDirection Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.currentDirection) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can set lower set point``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            
            magnetController.SetLowerSetPoint 2.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(2.0<A>, setPointParams.lowerSetPoint)

            magnetController.SetLowerSetPoint 4.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(4.0<A>, setPointParams.lowerSetPoint) } 
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can set upper set point``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 
            
            magnetController.SetUpperSetPoint 1.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(1.0<A>, setPointParams.upperSetPoint)

            magnetController.SetUpperSetPoint 3.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(3.0<A>, setPointParams.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can wait to reach target``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Lower, currentParams.rampTarget)
            Assert.IsTrue(currentParams.reachedTarget)

            magnetController.SetRampTarget Zero
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Zero, currentParams.rampTarget)
            Assert.IsTrue(currentParams.reachedTarget) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can ramp to zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Lower, currentParams.rampTarget)
            Assert.IsTrue(currentParams.reachedTarget)

            do! magnetController.RampToZeroAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Zero, currentParams.rampTarget)
            Assert.IsTrue(currentParams.reachedTarget) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can ramp to zero at ramp rate limit and change current direction``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            magnetController.SetRampRate 0.048<A/s>
            do! magnetController.WaitToReachTargetAsync()
            do! magnetController.RampToZeroAndSetCurrentDirectionAsync Reverse
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Reverse, operatingParams.currentDirection)
            Assert.AreEqual(0.098<A/s>, operatingParams.rampRate)

            magnetController.SetRampRate 0.048<A/s>
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            do! magnetController.RampToZeroAndSetCurrentDirectionAsync Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.currentDirection)
            Assert.AreEqual(0.098<A/s>, operatingParams.rampRate) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can wait to reach zero and change current direction``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            magnetController.SetLowerSetPoint 0.5<A>
            magnetController.SetRampRate 0.048<A/s>

            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            magnetController.SetRampTarget Zero
            do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Reverse
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Reverse, operatingParams.currentDirection)
            Assert.AreEqual(0.048<A/s>, operatingParams.rampRate)

            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            magnetController.SetRampTarget Zero
            do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.currentDirection)
            Assert.AreEqual(0.048<A/s>, operatingParams.rampRate) }
        |> Async.RunSynchronously


