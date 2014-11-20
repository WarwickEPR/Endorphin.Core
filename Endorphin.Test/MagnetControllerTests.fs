namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open TestUtils

[<TestFixture>]
type ``Magnet controller tests``() = 
    let magnetController : MagnetController option ref = ref None
    let _ = log4netConfig()

    member this.MagnetController =
        match !magnetController with
        | Some(controller) -> controller
        | None -> raise (new NullReferenceException())
        
    [<SetUp>]
    member this.``Connect to magnet controller``() =
        magnetController := Some(new MagnetController(magnetControllerVisaAddress, magnetControllerParameters))

    [<TearDown>]
    member this.``Disccnnect from magnet controller``() =
        (this.MagnetController :> IDisposable).Dispose()
        magnetController := None
        
    [<Test>]
    member this.``Check ramp rate limit is set``() =
        let rampRateLimit = this.MagnetController.RampRateLimit
        Assert.AreEqual(0.1<A/s>, rampRateLimit)

    [<Test>]
    member this.``Check trip voltage limit is set``() =
        let tripVoltageLimit = this.MagnetController.TripVoltageLimit
        Assert.AreEqual(2.5<V>, tripVoltageLimit)

    [<Test>]
    member this.``Check current limit is set``() =
        let currentLimit = this.MagnetController.CurrentLimit
        Assert.AreEqual(5.0<A>, currentLimit)

    [<Test>]
    member this.``Check zero current field is set``() =
        let zeroCurrentField = this.MagnetController.StaticField
        Assert.AreEqual(14.0<T>, zeroCurrentField)

    [<Test>]
    member this.``Check field calibration is set``() =
        let fieldCalibration = this.MagnetController.FieldCalibration
        Assert.AreEqual(-0.003<T/A>, fieldCalibration)

    [<Test>]
    member this.``Check maximum current is set``() =
        let maximumCurrent = this.MagnetController.MaximumCurrent
        Assert.AreEqual(20.0<A>, maximumCurrent)

    [<Test>]
    member this.``Check shunt calibration is set``() =
        let shuntCalibration = this.MagnetController.ShuntCalibration
        Assert.AreEqual(0.020<V/A>, shuntCalibration)

    [<Test>]
    member this.``Check output resolution is set``() =
        let outputResolution = this.MagnetController.OutputResolutionInBits
        Assert.AreEqual(16, outputResolution)

    [<Test>]
    member this.``Check output parameter response``() =
        let response = this.MagnetController.GetOutputParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check current parameter response``() =
        let response = this.MagnetController.GetCurrentParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check operating parameter response``() =
        let response = this.MagnetController.GetOperatingParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Check set point parameter response``() =
        let response = this.MagnetController.GetSetPointParametersAsync()
                       |> Async.RunSynchronously
        response |> ignore

    [<Test>]
    member this.``Can set ramp rate``() =
        this.MagnetController.SetRampRate 0.064<A/s> 
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.064<A/s>, operatingParams.rampRate)

        this.MagnetController.SetRampRate 0.0072<A/s>
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.0072<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Can set ramp rate by index``() =
        this.MagnetController.SetRampRateByIndex 5         
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.00042<A/s>, operatingParams.rampRate)

        this.MagnetController.SetRampRateByIndex 20
        let operatingParams = this.MagnetController.GetOperatingParametersAsync()
                              |> Async.RunSynchronously
        Assert.AreEqual(0.00360<A/s>, operatingParams.rampRate)

    [<Test>]
    member this.``Ramp rates in amps per sec within limit are available``() =
        let testRampRates = [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(1,
                this.MagnetController.AvailableCurrentRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
    
    [<Test>]
    member this.``Ramp rates in amps per sec outside limit are not available``() =
        let testRampRates = [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
        for testRampRate in testRampRates do
            Assert.AreEqual(0,
                this.MagnetController.AvailableCurrentRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)

    [<Test>] 
    member this.``Available ramp rates are sorted in ascending order``() =
        this.MagnetController.AvailableCurrentRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))

    [<Test>]
    member this.``Ramp rates in tesla per sec within limit are available``() = 
        let testRampRates = 
            [ 0.00020<A/s>; 0.00150<A/s>; 0.00036<A/s> * 10.0; 0.00098<A/s> * 100.0 ]
            |> Seq.map (fun rampRate -> rampRate * 0.003<T/A>)  
        
        for testRampRate in testRampRates do
            Assert.AreEqual(1, 
                this.MagnetController.AvailableFieldRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
                
    [<Test>]
    member this.``Ramp rates in tesla per sec outside limit are not available``() = 
        let testRampRates = 
            [ 0.00020<A/s> * 1000.0; 0.00150<A/s> * 1000.0; 0.00048<A/s> * 1000.0 ]
            |> Seq.map (fun rampRate -> rampRate * 0.003<T/A>)  
        
        for testRampRate in testRampRates do
            Assert.AreEqual(0, 
                this.MagnetController.AvailableFieldRampRates
                |> Seq.filter (fun rampRate -> rampRate = testRampRate)
                |> Seq.length)
                
    [<Test>] 
    member this.``Available ramp rates in tesla per sec are sorted in ascending order``() =
        this.MagnetController.AvailableFieldRampRates
        |> Seq.pairwise
        |> Seq.iter (fun (first, second) -> Assert.IsTrue(first < second))
        
    [<Test>]
    member this.``Can retrieve current for index``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)

        Assert.AreEqual(
            1324.0 * currentStep,
            this.MagnetController.CurrentForIndex(1324))

        Assert.AreEqual(
            3398.0 * currentStep,
            this.MagnetController.CurrentForIndex(3398))

    [<Test>]
    member this.``Can retrieve ramp rate for index in amps per sec``() =
        Assert.AreEqual(
            0.00042<A/s>,
            this.MagnetController.CurrentRampRateForIndex(5))

        Assert.AreEqual(
            0.00036<A/s> * 10.0,
            this.MagnetController.CurrentRampRateForIndex(20))

    [<Test>]
    member this.``Can retrieve ramp rate for index in tesla per sec``() =
        Assert.AreEqual(
            0.00042<A/s> * 0.003<T/A>,
            this.MagnetController.FieldRampRateForIndex(5))

        Assert.AreEqual(
            0.00036<A/s> * 10.0 * 0.003<T/A>,
            this.MagnetController.FieldRampRateForIndex(20))

    [<Test>]
    member this.``Fastest available ramp rate is correct``() =
        let maximumRampRate =
            this.MagnetController.AvailableCurrentRampRates
            |> Seq.max
        Assert.AreEqual(
            0.09800<A/s>, 
            maximumRampRate)

    [<Test>]
    member this.``Nearest digitised ramp rate in amps per sec is calculated correctly``() =
        let digitisedRampRate = this.MagnetController.NearestDigitisedCurrentRampRate(0.00630<A/s>)
        Assert.AreEqual(0.00640<A/s>, digitisedRampRate)
        
        let digitisedRampRate = this.MagnetController.NearestDigitisedCurrentRampRate(0.01000<A/s>)
        Assert.AreEqual(0.00980<A/s>, digitisedRampRate)

    [<Test>]
    member this.``Nearest digitised ramp rate in tesla per sec is calculated correctly``() =
        let digitisedRampRate = this.MagnetController.NearestDigitisedFieldRampRate(0.00630<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00640<A/s> * 0.003<T/A>, digitisedRampRate)
        
        let digitisedRampRate = this.MagnetController.NearestDigitisedFieldRampRate(0.01000<A/s> * 0.003<T/A>)
        Assert.AreEqual(0.00980<A/s> * 0.003<T/A>, digitisedRampRate)
    
    [<Test>]
    member this.``Can set trip voltage``() =
        this.MagnetController.SetTripVoltage 1.5<V> 
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(1.5<V>, setPointParams.tripVoltage)

        this.MagnetController.SetTripVoltage 2.0<V>
        let setPointParams = this.MagnetController.GetSetPointParametersAsync()
                             |> Async.RunSynchronously
        Assert.AreEqual(2.0<V>, setPointParams.tripVoltage)

    [<Test>]
    member this.``Can pause``() =
        this.MagnetController.SetPause true
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.IsTrue(currentParams.isPaused)

        this.MagnetController.SetPause false
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.IsFalse(currentParams.isPaused)

    [<Test>]
    member this.``Can set ramp target``() =
        this.MagnetController.SetPause true
        
        this.MagnetController.SetRampTarget Upper
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Upper, currentParams.rampTarget)

        this.MagnetController.SetRampTarget Zero
        let currentParams = this.MagnetController.GetCurrentParametersAsync()
                            |> Async.RunSynchronously
        Assert.AreEqual(Zero, currentParams.rampTarget)

        this.MagnetController.SetPause false

    [<Test>]
    member this.``Number of current steps is correct``() =
        let expectedNumberOfCurrentSteps = int(2.0 ** 16.0)
        Assert.AreEqual(
            expectedNumberOfCurrentSteps,
            this.MagnetController.NumberOfCurrentSteps)

    [<Test>]
    member this.``Current step in amps is correct``() =
        let expectedCurrentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        Assert.AreEqual(
            expectedCurrentStep,
            this.MagnetController.CurrentStep)

    [<Test>]
    member this.``Field step in tesla is correct``() =
        let expectedFieldStep = 0.003<T/A> * 20.0<A> / (2.0 ** 16.0 - 1.0)
        Assert.AreEqual(
            expectedFieldStep, 
            this.MagnetController.FieldStep)

    [<Test>]
    member this.``Minimum field in tesla is correct``() =
        let expectedMinimumField = 14.0<T> - 0.003<T/A> * 5.0<A>
        Assert.AreEqual(
            expectedMinimumField,
            this.MagnetController.MinimumField)
    
    [<Test>]
    member this.``Maximum field in tesla is correct``() =
        let expectedMaximumField = 14.0<T> + 0.003<T/A> * 5.0<A>
        Assert.AreEqual(
            expectedMaximumField,
            this.MagnetController.MaximumField)
    
    [<Test>]
    member this.``Shunt step in volts is correct``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        let expectedShuntStep = 0.020<V/A> * currentStep
        Assert.AreEqual(expectedShuntStep, this.MagnetController.ShuntStep)

    [<Test>]
    member this.``Nearest ramp rate index for amps per sec is calculated correctly``() =
        Assert.AreEqual(3, this.MagnetController.NearestDigitisedCurrentRampRateIndex(0.00032<A/s>))
        Assert.AreEqual(33, this.MagnetController.NearestDigitisedCurrentRampRateIndex(0.02300<A/s>))

    [<Test>]
    member this.``Nearest ramp rate index for tesla per sec is calculated correctly``() =
        Assert.AreEqual(3, this.MagnetController.NearestDigitisedFieldRampRateIndex(0.00032<A/s> * 0.003<T/A>))
        Assert.AreEqual(33, this.MagnetController.NearestDigitisedFieldRampRateIndex(0.02300<A/s> * 0.003<T/A>))

    [<Test>]
    member this.``Nearest digitised current output index for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734,
            this.MagnetController.NearestDigitisedOutputIndexForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341,
            this.MagnetController.NearestDigitisedOutputIndexForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))

    [<Test>]
    member this.``Nearest digitised current for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734.0 * currentStep,
            this.MagnetController.NearestDigitisedCurrentForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341.0 * currentStep,
            this.MagnetController.NearestDigitisedCurrentForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))

    
    [<Test>]
    member this.``Nearest digitised field for shunt voltage is calculated correctly``() =
        let currentStep = 20.0<A> / (2.0 ** 16.0 - 1.0)
        
        Assert.AreEqual(
            7734.0 * currentStep * -0.003<T/A>,
            this.MagnetController.NearestDigitisedFieldForShuntVoltage(7734.2 * currentStep * 0.020<V/A>))
        
        Assert.AreEqual(
            -2341.0 * currentStep * -0.003<T/A>,
            this.MagnetController.NearestDigitisedFieldForShuntVoltage(-2340.8 * currentStep * 0.020<V/A>))