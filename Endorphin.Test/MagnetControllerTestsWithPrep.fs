namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open Config

[<TestFixture>]
type ``Magnet controller tests with prepared state``() = 
    let magnetControllerSession = new MagnetControllerSession(magnetControllerVisaAddress, magnetControllerParameters)
    let _ = log4netConfig()

    [<TestFixtureSetUp>]
    member __.``Connect to PicoScope``() =
        magnetControllerSession.ConnectAsync() 
        |> Async.RunSynchronously
    
    [<TestFixtureTearDown>]
    member __.``Close magnet controller session``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously

        magnetControllerSession.CloseSessionAsync() 
        |> Async.RunSynchronously

    [<SetUp>]
    member __.``Prepare magnet controller state``() = 
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously
        
    [<Test>]
    member __.``Can change current direction``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()      
            
            magnetController.SetCurrentDirection Reverse
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Reverse, operatingParams.CurrentDirection)

            magnetController.SetCurrentDirection Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.CurrentDirection) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can set lower set point``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            
            magnetController.SetLowerSetPoint 2.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(2.0<A>, setPointParams.LowerSetPoint)

            magnetController.SetLowerSetPoint 4.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(4.0<A>, setPointParams.LowerSetPoint) } 
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can set upper set point``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 
            
            magnetController.SetUpperSetPoint 1.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(1.0<A>, setPointParams.UpperSetPoint)

            magnetController.SetUpperSetPoint 3.0<A>
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            Assert.AreEqual(3.0<A>, setPointParams.UpperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can wait to reach target``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Lower, currentParams.RampTarget)
            Assert.IsTrue(currentParams.ReachedTarget)

            magnetController.SetRampTarget Zero
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Zero, currentParams.RampTarget)
            Assert.IsTrue(currentParams.ReachedTarget) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can ramp to zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Lower, currentParams.RampTarget)
            Assert.IsTrue(currentParams.ReachedTarget)

            magnetController.BeginRampToZeroAtMaximumRampRate()
            do! magnetController.WaitToReachTargetAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()
            Assert.AreEqual(Zero, currentParams.RampTarget)
            Assert.IsTrue(currentParams.ReachedTarget) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can ramp to zero at ramp rate limit and change current direction``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            magnetController.SetLowerSetPoint 0.5<A>
        
            magnetController.SetRampTarget Lower
            magnetController.SetRampRate 0.048<A/s>
            do! magnetController.WaitToReachTargetAsync()
            magnetController.BeginRampToZeroAtMaximumRampRate()
            do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Reverse
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Reverse, operatingParams.CurrentDirection)
            Assert.AreEqual(0.098<A/s>, operatingParams.RampRate)

            magnetController.SetRampRate 0.048<A/s>
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            magnetController.BeginRampToZeroAtMaximumRampRate()
            do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.CurrentDirection)
            Assert.AreEqual(0.098<A/s>, operatingParams.RampRate) }
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
            Assert.AreEqual(Reverse, operatingParams.CurrentDirection)
            Assert.AreEqual(0.048<A/s>, operatingParams.RampRate)

            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
            magnetController.SetRampTarget Zero
            do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync Forward
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            Assert.AreEqual(Forward, operatingParams.CurrentDirection)
            Assert.AreEqual(0.048<A/s>, operatingParams.RampRate) }
        |> Async.RunSynchronously


