namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open Config

[<TestFixture>]
type ``Magnet controller tests``() = 
    let magnetControllerSession = new MagnetControllerSession(magnetControllerVisaAddress, magnetControllerParameters)
    let _ = log4netConfig()

    [<TestFixtureTearDown>]
    member __.``Close magnet controller session``() =
        magnetControllerSession.CloseSessionAsync() 
        |> Async.RunSynchronously
        
    [<Test>]
    member __.``Check ramp rate limit is set``() =
        let rampRateLimit = magnetControllerSession.DeviceParameters.RampRateLimit
        Assert.AreEqual(0.1<A/s>, rampRateLimit)

    [<Test>]
    member __.``Check trip voltage limit is set``() =
        let tripVoltageLimit = magnetControllerSession.DeviceParameters.TripVoltageLimit
        Assert.AreEqual(2.5<V>, tripVoltageLimit)

    [<Test>]
    member __.``Check current limit is set``() =
        let currentLimit = magnetControllerSession.DeviceParameters.CurrentLimit
        Assert.AreEqual(5.0<A>, currentLimit)

    [<Test>]
    member __.``Check static field is set``() =
        let staticField = magnetControllerSession.DeviceParameters.StaticField
        Assert.AreEqual(14.0<T>, staticField)

    [<Test>]
    member __.``Check field calibration is set``() =
        let fieldCalibration = magnetControllerSession.DeviceParameters.FieldCalibration
        Assert.AreEqual(-0.003<T/A>, fieldCalibration)

    [<Test>]
    member __.``Check maximum current is set``() =
        let maximumCurrent = magnetControllerSession.DeviceParameters.MaximumCurrent
        Assert.AreEqual(20.0<A>, maximumCurrent)

    [<Test>]
    member __.``Check shunt calibration is set``() =
        let shuntCalibration = magnetControllerSession.DeviceParameters.ShuntCalibration
        Assert.AreEqual(0.020<V/A>, shuntCalibration)

    [<Test>]
    member __.``Check shunt offset is set``() =
        let shuntOffset = magnetControllerSession.DeviceParameters.ShuntOffset
        Assert.AreEqual(2e-5<V>, shuntOffset)

    [<Test>]
    member __.``Check output resolution is set``() =
        let outputResolution = magnetControllerSession.DeviceParameters.OutputResolution
        Assert.AreEqual(16, outputResolution)

    [<Test>]
    member __.``Check output parameter response``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        magnetController.GetOutputParametersAsync()
        |> Async.RunSynchronously
        |> ignore

    [<Test>]
    member __.``Check current parameter response``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.GetCurrentParametersAsync()
        |> Async.RunSynchronously
        |> ignore

    [<Test>]
    member __.``Check operating parameter response``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        magnetController.GetOperatingParametersAsync()
        |> Async.RunSynchronously
        |> ignore

    [<Test>]
    member __.``Check set point parameter response``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        magnetController.GetSetPointParametersAsync()
        |> Async.RunSynchronously
        |> ignore

    [<Test>]
    member __.``Can set ramp rate``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        magnetController.SetRampRate 0.064<A/s> 
        let operatingParams = 
            magnetController.GetOperatingParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.064<A/s>, operatingParams.RampRate)

        magnetController.SetRampRate 0.0072<A/s>
        let operatingParams = 
            magnetController.GetOperatingParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.0072<A/s>, operatingParams.RampRate)

    [<Test>]
    member __.``Can set ramp rate by index``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        magnetController.SetRampRateByIndex 5         
        let operatingParams = 
            magnetController.GetOperatingParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.00042<A/s>, operatingParams.RampRate)

        magnetController.SetRampRateByIndex 20
        let operatingParams =
            magnetController.GetOperatingParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.00360<A/s>, operatingParams.RampRate)

    [<Test>]
    member __.``Ramp rates in amps per sec within limit are available``() =
        let testRampRates = [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(Some testRampRate,
                magnetControllerSession.DeviceParameters.AvailableCurrentRampRates
                |> Seq.tryFind ((=) testRampRate))
    
    [<Test>]
    member __.``Ramp rates in amps per sec outside limit are not available``() =
        let testRampRates = [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(None,
                magnetControllerSession.DeviceParameters.AvailableCurrentRampRates
                |> Seq.tryFind ((=) testRampRate))

    [<Test>] 
    member __.``Available ramp rates are sorted in ascending order``() =
        magnetControllerSession.DeviceParameters.AvailableCurrentRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))

    [<Test>]
    member __.``Ramp rates in tesla per sec within limit are available``() = 
        let testRampRates = 
            [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
            |> Seq.map ((*) 0.003<T/A>)
        
        for testRampRate in testRampRates do
            Assert.AreEqual(Some testRampRate, 
                magnetControllerSession.DeviceParameters.AvailableFieldRampRates
                |> Seq.tryFind ((=) testRampRate))
                
    [<Test>]
    member __.``Ramp rates in tesla per sec outside limit are not available``() = 
        let testRampRates = 
            [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
            |> Seq.map ((*) 0.003<T/A>)  
        
        for testRampRate in testRampRates do
            Assert.AreEqual(None, 
                magnetControllerSession.DeviceParameters.AvailableFieldRampRates
                |> Seq.tryFind ((=) testRampRate))
                
    [<Test>] 
    member __.``Available ramp rates in tesla per sec are sorted in ascending order``() =
        magnetControllerSession.DeviceParameters.AvailableFieldRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))
        
    [<Test>]
    member __.``Can retrieve current for index``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)

        Assert.AreEqual(
            1324.0 * currentStep,
            magnetControllerSession.DeviceParameters.CurrentForIndex 1324)

        Assert.AreEqual(
            3398.0 * currentStep,
            magnetControllerSession.DeviceParameters.CurrentForIndex 3398)

    [<Test>]
    member __.``Can retrieve ramp rate for index in amps per sec``() =
        Assert.AreEqual(
            0.00042<A/s>,
            magnetControllerSession.DeviceParameters.CurrentRampRateForIndex 5)

        Assert.AreEqual(
            0.00036<A/s> * 10.0,
            magnetControllerSession.DeviceParameters.CurrentRampRateForIndex 20)

    [<Test>]
    member __.``Can retrieve ramp rate for index in tesla per sec``() =
        Assert.AreEqual(
            0.00042<A/s> * 0.003<T/A>,
            magnetControllerSession.DeviceParameters.FieldRampRateForIndex 5)

        Assert.AreEqual(
            0.00036<A/s> * 10.0 * 0.003<T/A>,
            magnetControllerSession.DeviceParameters.FieldRampRateForIndex 20)

    [<Test>]
    member __.``Fastest available ramp rate is correct``() =
        let maximumRampRate =
            magnetControllerSession.DeviceParameters.AvailableCurrentRampRates
            |> Seq.max
        
        Assert.AreEqual(
            0.09800<A/s>, 
            maximumRampRate)

    [<Test>]
    member __.``Nearest digitised ramp rate in amps per sec is calculated correctly``() =
        let digitisedRampRate = magnetControllerSession.DeviceParameters.NearestDigitisedCurrentRampRate 0.00630<A/s>
        Assert.AreEqual(0.00640<A/s>, digitisedRampRate)
        
        let digitisedRampRate = magnetControllerSession.DeviceParameters.NearestDigitisedCurrentRampRate 0.01000<A/s>
        Assert.AreEqual(0.00980<A/s>, digitisedRampRate)

    [<Test>]
    member __.``Nearest digitised ramp rate in tesla per sec is calculated correctly``() =
        let digitisedRampRate = magnetControllerSession.DeviceParameters.NearestDigitisedFieldRampRate (0.00630<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00640<A/s> * 0.003<T/A>, digitisedRampRate)
        
        let digitisedRampRate = magnetControllerSession.DeviceParameters.NearestDigitisedFieldRampRate (0.01000<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00980<A/s> * 0.003<T/A>, digitisedRampRate)
    
    [<Test>]
    member __.``Can set lower set point``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.SetUpperSetPoint 5.0<A> 
        magnetController.SetLowerSetPoint 0.4<A>
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.4<A>, setPointParams.LowerSetPoint)

        magnetController.SetLowerSetPoint 2.2<A>
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(2.2<A>, setPointParams.LowerSetPoint)

    [<Test>]
    member __.``Can set upper set point``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.SetLowerSetPoint 0.0<A> 
        magnetController.SetUpperSetPoint 0.8<A>
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(0.8<A>, setPointParams.UpperSetPoint)

        magnetController.SetUpperSetPoint 3.7<A>
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(3.7<A>, setPointParams.UpperSetPoint)

    [<Test>]
    member __.``Can set trip voltage``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.SetTripVoltage 1.5<V> 
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(1.5<V>, setPointParams.TripVoltage)

        magnetController.SetTripVoltage 2.0<V>
        let setPointParams = 
            magnetController.GetSetPointParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(2.0<V>, setPointParams.TripVoltage)

    [<Test>]
    member __.``Can pause``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.SetPause true
        let currentParams = 
            magnetController.GetCurrentParametersAsync()
            |> Async.RunSynchronously
        Assert.IsTrue(currentParams.IsPaused)

        magnetController.SetPause false
        let currentParams = 
            magnetController.GetCurrentParametersAsync()
            |> Async.RunSynchronously
        Assert.IsFalse(currentParams.IsPaused)

    [<Test>]
    member __.``Can set ramp target``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        magnetController.SetPause true
        
        magnetController.SetRampTarget Upper
        let currentParams =
            magnetController.GetCurrentParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(Upper, currentParams.RampTarget)

        magnetController.SetRampTarget Zero
        let currentParams =
            magnetController.GetCurrentParametersAsync()
            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.RampTarget)

        magnetController.SetPause false

    [<Test>]
    member __.``Number of current steps is correct``() =
        Assert.AreEqual(
            int(2.0 ** 16.0),
            magnetControllerSession.DeviceParameters.NumberOfCurrentSteps)

    [<Test>]
    member __.``Current step in amps is correct``() =
        Assert.AreEqual(
            20.0<A> / (2.0 ** 16.0 - 1.0),
            magnetControllerSession.DeviceParameters.CurrentStep)

    [<Test>]
    member __.``Field step in tesla is correct``() =
        Assert.AreEqual(
            0.003<T/A> * 20.0<A> / (2.0 ** 16.0 - 1.0), 
            magnetControllerSession.DeviceParameters.FieldStep)

    [<Test>]
    member __.``Minimum field in tesla is correct``() =
        Assert.AreEqual(
            14.0<T> - 0.003<T/A> * 5.0<A>,
            magnetControllerSession.DeviceParameters.MinimumField)
    
    [<Test>]
    member __.``Maximum field in tesla is correct``() =
        Assert.AreEqual(
            14.0<T> + 0.003<T/A> * 5.0<A>,
            magnetControllerSession.DeviceParameters.MaximumField)
    
    [<Test>]
    member __.``Shunt step in volts is correct``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        let expectedShuntStep = 0.020<V/A> * currentStep
        Assert.AreEqual(expectedShuntStep, magnetControllerSession.DeviceParameters.ShuntStep)

    [<Test>]
    member __.``Nearest ramp rate index for amps per sec is calculated correctly``() =
        Assert.AreEqual(3, magnetControllerSession.DeviceParameters.NearestDigitisedCurrentRampRateIndex 0.00032<A/s>)
        Assert.AreEqual(33, magnetControllerSession.DeviceParameters.NearestDigitisedCurrentRampRateIndex 0.02300<A/s>)

    [<Test>]
    member __.``Nearest ramp rate index for tesla per sec is calculated correctly``() =
        Assert.AreEqual(3, magnetControllerSession.DeviceParameters.NearestDigitisedFieldRampRateIndex (0.00032<A/s> * 0.003<T/A>))
        Assert.AreEqual(33, magnetControllerSession.DeviceParameters.NearestDigitisedFieldRampRateIndex (0.02300<A/s> * 0.003<T/A>))