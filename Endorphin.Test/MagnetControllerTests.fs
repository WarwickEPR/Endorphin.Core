namespace Endorphin.Test

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System

[<TestFixture>]
type ``Magnet controller tests``() = 
    let mutable magnetControllerOpt : MagnetController option = None
    member this.magnetController 
        with get() : MagnetController = magnetControllerOpt.Value
        and set(value : MagnetController) = magnetControllerOpt <- Some(value)

    [<SetUp>]
    member this.``Connect to magnet controller``() =
        this.magnetController <-
            new MagnetController(magnetControllerVisaAddress, magnetControllerParameters)

    [<TearDown>]
    member this.``Disccnnect from magnet controller``() =
        (this.magnetController :> IDisposable).Dispose()

    [<Test>]
    member this.``Check ramp rate limit is set``() =
        let rampRateLimit = this.magnetController.RampRateLimit
        Assert.AreEqual(0.1<A/s>, rampRateLimit)

    [<Test>]
    member this.``Check trip voltage limit is set``() =
        let tripVoltageLimit = this.magnetController.TripVoltageLimit
        Assert.AreEqual(2.5<V>, tripVoltageLimit)

    [<Test>]
    member this.``Check current limit is set``() =
        let currentLimit = this.magnetController.CurrentLimit
        Assert.AreEqual(5.0<A>, currentLimit)

    [<Test>]
    member this.``Check zero current field is set``() =
        let zeroCurrentField = this.magnetController.StaticField
        Assert.AreEqual(14.0<T>, zeroCurrentField)

    [<Test>]
    member this.``Check field calibration is set``() =
        let fieldCalibration = this.magnetController.FieldCalibration
        Assert.AreEqual(-0.003<T/A>, fieldCalibration)

    [<Test>]
    member this.``Check maximum current is set``() =
        let maximumCurrent = this.magnetController.MaximumCurrent
        Assert.AreEqual(20.0<A>, maximumCurrent)

    [<Test>]
    member this.``Check shunt calibration is set``() =
        let shuntCalibration = this.magnetController.ShuntCalibration
        Assert.AreEqual(0.020<V/A>, shuntCalibration)

    [<Test>]
    member this.``Check output resolution is set``() =
        let outputResolution = this.magnetController.OutputResolutionInBits
        Assert.AreEqual(16, outputResolution)

    [<Test>]
    member this.``Check output parameter response``() =
        let response = this.magnetController.GetOutputParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check current parameter response``() =
        let response = this.magnetController.GetCurrentParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check operating parameter response``() =
        let response = this.magnetController.GetOperatingParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check set point parameter response``() =
        let response = this.magnetController.GetSetPointParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Can set ramp rate``() =
        this.magnetController.SetRampRate 0.064<A/s> 
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.064<A/s>, operatingParams.rampRate)

        this.magnetController.SetRampRate 0.0072<A/s>
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.0072<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Can set ramp rate by index``() =
        this.magnetController.SetRampRateByIndex 5         
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.00042<A/s>, operatingParams.rampRate)

        this.magnetController.SetRampRateByIndex 20
        let operatingParams = this.magnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.00360<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Ramp rates in amps per sec within limit are available``() =
        let testRampRates = [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(1,
                this.magnetController.AvailableCurrentRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
    
    [<Test>]
    member this.``Ramp rates in amps per sec outside limit are not available``() =
        let testRampRates = [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(0,
                this.magnetController.AvailableCurrentRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)

    [<Test>] 
    member this.``Available ramp rates are sorted in ascending order``() =
        this.magnetController.AvailableCurrentRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))

    [<Test>]
    member this.``Ramp rates in tesla per sec within limit are available``() = 
        let testRampRates = 
            [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
            |> Seq.map (fun rampRate -> rampRate * 0.003<T/A>)  
        
        for testRampRate in testRampRates do
            Assert.AreEqual(1, 
                this.magnetController.AvailableFieldRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
                
    [<Test>]
    member this.``Ramp rates in tesla per sec outside limit are not available``() = 
        let testRampRates = 
            [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
            |> Seq.map (fun rampRate -> rampRate * 0.003<T/A>)  
        
        for testRampRate in testRampRates do
            Assert.AreEqual(0, 
                this.magnetController.AvailableFieldRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
                
    [<Test>] 
    member this.``Available ramp rates in tesla per sec are sorted in ascending order``() =
        this.magnetController.AvailableFieldRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))
        
    [<Test>]
    member this.``Can retrieve current for index``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)

        Assert.AreEqual(
            1324.0 * currentStep,
            this.magnetController.CurrentForIndex(1324))

        Assert.AreEqual(
            3398.0 * currentStep,
            this.magnetController.CurrentForIndex(3398))

    [<Test>]
    member this.``Can retrieve ramp rate for index in amps per sec``() =
        Assert.AreEqual(
            0.00042<A/s>,
            this.magnetController.CurrentRampRateForIndex(5))

        Assert.AreEqual(
            0.00036<A/s> * 10.0,
            this.magnetController.CurrentRampRateForIndex(20))

    [<Test>]
    member this.``Can retrieve ramp rate for index in tesla per sec``() =
        Assert.AreEqual(
            0.00042<A/s> * 0.003<T/A>,
            this.magnetController.FieldRampRateForIndex(5))

        Assert.AreEqual(
            0.00036<A/s> * 10.0 * 0.003<T/A>,
            this.magnetController.FieldRampRateForIndex(20))

    [<Test>]
    member this.``Fastest available ramp rate is correct``() =
        let maximumRampRate =
            this.magnetController.AvailableCurrentRampRates
            |> Seq.max
        Assert.AreEqual(
            0.09800<A/s>, 
            maximumRampRate)

    [<Test>]
    member this.``Nearest digitised ramp rate in amps per sec is calculated correctly``() =
        let digitisedRampRate = this.magnetController.NearestDigitisedCurrentRampRate(0.00630<A/s>)
        Assert.AreEqual(0.00640<A/s>, digitisedRampRate)
        
        let digitisedRampRate = this.magnetController.NearestDigitisedCurrentRampRate(0.01000<A/s>)
        Assert.AreEqual(0.00980<A/s>, digitisedRampRate)

    [<Test>]
    member this.``Nearest digitised ramp rate in tesla per sec is calculated correctly``() =
        let digitisedRampRate = this.magnetController.NearestDigitisedFieldRampRate(0.00630<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00640<A/s> * 0.003<T/A>, digitisedRampRate)
        
        let digitisedRampRate = this.magnetController.NearestDigitisedFieldRampRate(0.01000<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00980<A/s> * 0.003<T/A>, digitisedRampRate)
    
    [<Test>]
    member this.``Can set trip voltage``() =
        this.magnetController.SetTripVoltage 1.5<V> 
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(1.5<V>, setPointParams.tripVoltage)

        this.magnetController.SetTripVoltage 2.0<V>
        let setPointParams = this.magnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(2.0<V>, setPointParams.tripVoltage)

    [<Test>]
    member this.``Can pause``() =
        this.magnetController.SetPause true
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.IsTrue(currentParams.isPaused)

        this.magnetController.SetPause false
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.IsFalse(currentParams.isPaused)

    [<Test>]
    member this.``Can set ramp target``() =
        this.magnetController.SetPause true
        
        this.magnetController.SetRampTarget Upper
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Upper, currentParams.rampTarget)

        this.magnetController.SetRampTarget Zero
        let currentParams = this.magnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)

        this.magnetController.SetPause false

    [<Test>]
    member this.``Number of current steps is correct``() =
        let expectedNumberOfCurrentSteps = int(2.0 ** 16.0)
        Assert.AreEqual(
            expectedNumberOfCurrentSteps,
            this.magnetController.NumberOfCurrentSteps)

    [<Test>]
    member this.``Current step in amps is correct``() =
        let expectedCurrentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        Assert.AreEqual(
            expectedCurrentStep,
            this.magnetController.CurrentStep)

    [<Test>]
    member this.``Field step in tesla is correct``() =
        let expectedFieldStep = 0.003<T/A> * 20.0<A> / (2.0 ** 16.0 - 1.0)
        Assert.AreEqual(
            expectedFieldStep, 
            this.magnetController.FieldStep)

    [<Test>]
    member this.``Minimum field in tesla is correct``() =
        let expectedMinimumField = 14.0<T> - 0.003<T/A> * 5.0<A>
        Assert.AreEqual(
            expectedMinimumField,
            this.magnetController.MinimumField)
    
    [<Test>]
    member this.``Maximum field in tesla is correct``() =
        let expectedMaximumField = 14.0<T> + 0.003<T/A> * 5.0<A>
        Assert.AreEqual(
            expectedMaximumField,
            this.magnetController.MaximumField)
    
    [<Test>]
    member this.``Shunt step in volts is correct``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        let expectedShuntStep = 0.020<V/A> * currentStep
        Assert.AreEqual(expectedShuntStep, this.magnetController.ShuntStep)

    [<Test>]
    member this.``Nearest ramp rate index for amps per sec is calculated correctly``() =
        Assert.AreEqual(3, this.magnetController.NearestDigitisedCurrentRampRateIndex(0.00032<A/s>))
        Assert.AreEqual(33, this.magnetController.NearestDigitisedCurrentRampRateIndex(0.02300<A/s>))

    [<Test>]
    member this.``Nearest ramp rate index for tesla per sec is calculated correctly``() =
        Assert.AreEqual(3, this.magnetController.NearestDigitisedFieldRampRateIndex(0.00032<A/s> * 0.003<T/A>))
        Assert.AreEqual(33, this.magnetController.NearestDigitisedFieldRampRateIndex(0.02300<A/s> * 0.003<T/A>))

    [<Test>]
    member this.``Nearest digitised current output index for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734,
            this.magnetController.NearestDigitisedOutputIndexForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341,
            this.magnetController.NearestDigitisedOutputIndexForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))

    [<Test>]
    member this.``Nearest digitised current for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734.0 * currentStep,
            this.magnetController.NearestDigitisedCurrentForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341.0 * currentStep,
            this.magnetController.NearestDigitisedCurrentForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))

    
    [<Test>]
    member this.``Nearest digitised field for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734.0 * currentStep * -0.003<T/A>,
            this.magnetController.NearestDigitisedFieldForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341.0 * currentStep * -0.003<T/A>,
            this.magnetController.NearestDigitisedFieldForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))