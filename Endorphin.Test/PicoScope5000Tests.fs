namespace Endorphin.Test

open Endorphin.Core.Utils
open Endorphin.Instrument.PicoScope5000
open Devices
open NUnit.Framework
open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Reactive.Linq
open TestUtils

[<TestFixture>]
type ``PicoScope 5000 series tests``() = 
    let picoScope : PicoScope5000 option ref = ref None
    let _ = log4netConfig()

    member this.PicoScope =
        match !picoScope with
        | Some(scope) -> scope
        | None -> raise (new NullReferenceException())

    [<TestFixtureSetUp>]
    member this.``Connect to PicoScope``() =
        picoScope := Some(new PicoScope5000())
        this.PicoScope.Error.Add(fun exn ->
            picoScope := None
            Assert.Fail(sprintf "PicoScope agent crashed due to error: %A" exn))

        let isMainsPowered =
            this.PicoScope.GetUnitIsMainsPoweredAsync()
            |> Async.RunSynchronously
        Assert.IsTrue(isMainsPowered, 
            "The PicoScope is not mains-powered. These unit test assume that the device has mains-power.")

    [<TestFixtureTearDown>]
    member this.``Disconnect from PicoScope``() =
        if (!picoScope).IsSome then
            this.PicoScope.PingAsync() |> Async.RunSynchronously 
            (this.PicoScope :> IDisposable).Dispose()
            picoScope := None

    [<TearDown>]
    member this.``Check response after test``() =
        // if the agent crashes then this will fail
        this.PicoScope.PingAsync() |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit driver version``() =
        let driverVersion =
            this.PicoScope.GetUnitDriverVersionAsync() 
            |> Async.RunSynchronously

        let regex = @"\d\.\d\.\d\.\d"
        match driverVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." driverVersion
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit USB version``() =
        let usbVersion =
            this.PicoScope.GetUnitUsbVersionAsync() 
            |> Async.RunSynchronously

        let regex = @"\d\.\d"
        match usbVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." usbVersion
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit hardware version``() =
        let hardwareVersion =
            this.PicoScope.GetUnitDriverVersionAsync() 
            |> Async.RunSynchronously

        let regex = @"\d\.\d"
        match hardwareVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." hardwareVersion
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit model number``() =
        let modelNumber =
            this.PicoScope.GetUnitModelNumberAsync() 
            |> Async.RunSynchronously

        let regex = @"\d{4}[AB]"
        match modelNumber with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." modelNumber
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit serial number``() =
        let serial =
            this.PicoScope.GetUnitSerialAsync() 
            |> Async.RunSynchronously

        let regex = @"[A-Z&&\d]{5}/\d{3}"
        match serial with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." serial
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit calibration date``() =
        let calibrationDate =
            this.PicoScope.GetUnitCalibrationDateAsync() 
            |> Async.RunSynchronously
        
        Assert.DoesNotThrow((fun () ->
            DateTime.ParseExact(calibrationDate, "ddMMMyy", new Globalization.CultureInfo("en-gb")) |> ignore),
            sprintf "PicoScope response %s does not match expected format." calibrationDate)

    [<Test>]
    member this.``Can get unit kernel version``() =
        let kernelVersion =
            this.PicoScope.GetUnitKernelVersionAsync() 
            |> Async.RunSynchronously
        
        let regex = @"\d\.\d"
        match kernelVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." kernelVersion
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit digital hardware version``() =
        let digitalHardwareVersion =
            this.PicoScope.GetUnitKernelVersionAsync() 
            |> Async.RunSynchronously
        
        let regex = @"\d\.\d"
        match digitalHardwareVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." digitalHardwareVersion
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit analogue hardware version``() =
        let analogueHardwareVersion =
            this.PicoScope.GetUnitKernelVersionAsync() 
            |> Async.RunSynchronously
        
        let regex = @"\d\.\d"
        match analogueHardwareVersion with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." analogueHardwareVersion
            |> Assert.Fail
    
    [<Test>]
    member this.``Can get unit firmware version 1``() =
        let firmwareVersion1 =
            this.PicoScope.GetUnitFirmwareVersion1Async() 
            |> Async.RunSynchronously
        
        let regex = @"\d\.\d"
        match firmwareVersion1 with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." firmwareVersion1
            |> Assert.Fail

    [<Test>]
    member this.``Can get unit firmware version 2``() =
        let firmwareVersion2 =
            this.PicoScope.GetUnitFirmwareVersion2Async() 
            |> Async.RunSynchronously
        
        let regex = @"\d\.\d"
        match firmwareVersion2 with
        | ParseRegex regex [] -> 
            Assert.Pass()
        | _ -> 
            sprintf "PicoScope response %s does not match expected format." firmwareVersion2
            |> Assert.Fail

    [<Test>]
    member this.``Can get all unit info as list``() =
        let infoList =
            this.PicoScope.GetAllUnitInfoAsync() 
            |> Async.RunSynchronously

        let driverVersion =
            this.PicoScope.GetUnitDriverVersionAsync() 
            |> Async.RunSynchronously
        let usbVersion =
            this.PicoScope.GetUnitUsbVersionAsync() 
            |> Async.RunSynchronously
        let hardwareVersion =
            this.PicoScope.GetUnitHardwareVersionAsync() 
            |> Async.RunSynchronously
        let modelNumber =
            this.PicoScope.GetUnitModelNumberAsync() 
            |> Async.RunSynchronously
        let serial =
            this.PicoScope.GetUnitSerialAsync() 
            |> Async.RunSynchronously
        let calibrationDate =
            this.PicoScope.GetUnitCalibrationDateAsync() 
            |> Async.RunSynchronously
        let kernelVersion =
            this.PicoScope.GetUnitKernelVersionAsync() 
            |> Async.RunSynchronously
        let digitalHardwareVersion =
            this.PicoScope.GetUnitDigitalHardwareVersionAsync() 
            |> Async.RunSynchronously
        let analogueHardwareVersion =
            this.PicoScope.GetUnitAnalogueHardwareVersionAsync() 
            |> Async.RunSynchronously
        let firmwareVersion1 =
            this.PicoScope.GetUnitFirmwareVersion1Async() 
            |> Async.RunSynchronously
        let firmwareVersion2 =
            this.PicoScope.GetUnitFirmwareVersion2Async() 
            |> Async.RunSynchronously

        let expected =
            [(PicoInfo.DriverVersion, driverVersion)
             (PicoInfo.UsbVersion, usbVersion)
             (PicoInfo.HardwareVersion, hardwareVersion)
             (PicoInfo.ModelNumber, modelNumber)
             (PicoInfo.SerialNumber, serial)
             (PicoInfo.CalibrationDate, calibrationDate)
             (PicoInfo.KernelVersion, kernelVersion)
             (PicoInfo.DigitalHardwareVersion, digitalHardwareVersion)
             (PicoInfo.AnalogueHardwareVersion, analogueHardwareVersion)
             (PicoInfo.FirmwareVersion1, firmwareVersion1)
             (PicoInfo.FirmwareVersion2, firmwareVersion2)]
        
        Assert.AreEqual(expected, infoList)
        
    [<Test>]
    member this.``Can flash LED``() =
        this.PicoScope.FlashLed(3s)
        Thread.Sleep(5000)

        this.PicoScope.FlashLedIndefinitely()
        Thread.Sleep(5000)

        this.PicoScope.StopFlashingLed()

    [<Test>]
    member this.``Can get analogue offset limits``() =
        let (upper, lower) =
            this.PicoScope.GetAnalogueOffsetLimitsAsync(Range._20V, Coupling.DC) 
            |> Async.RunSynchronously
        
        Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
        Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
        Assert.AreEqual(20<V>, upper)

        let (upper, lower) =
            this.PicoScope.GetAnalogueOffsetLimitsAsync(Range._5V, Coupling.AC) 
            |> Async.RunSynchronously
        
        Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
        Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
        Assert.AreEqual(20<V>, upper)

        let (upper, lower) =
            this.PicoScope.GetAnalogueOffsetLimitsAsync(Range._200mV, Coupling.DC) 
            |> Async.RunSynchronously
        
        Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
        Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
        Assert.AreEqual(0.25<V>, upper)

    [<Test>]
    member this.``Can set channel settings``() =
        this.PicoScope.EnableChannel(Channel.A, 
            { coupling = Coupling.DC 
              range = Range._2V
              analogueOffset = 0.2<V>
              bandwidthLimit = BandwidthLimit._20MHz })

        this.PicoScope.DisableChannel(Channel.B)
        this.PicoScope.SetChannelSettings(Channel.A, Disabled)

    [<Test>]
    member this.``Can set simple or automatic trigger``() =
        let voltsToAdc =
            this.PicoScope.GetVoltageToAdcCountConversionAsync(Range._10V, 0.25<V>)
            |> Async.RunSynchronously

        this.PicoScope.SetSimpleTrigger(
            { channel = Channel.A
              adcThreshold = 5.0<V> |> voltsToAdc
              thresholdDirection = ThresholdDirection.Rising
              delaySamplesAfterTrigger = 0u
              autoTrigger = None })

        this.PicoScope.SetAutoTrigger(0.5<s>)

    [<Test>]
    member this.``Can get voltage to ADC conversion function``() =
        let voltsToAdc =
            this.PicoScope.GetVoltageToAdcCountConversionAsync(Range._10V, 0.25<V>)
            |> Async.RunSynchronously

        Assert.That(voltsToAdc 10.25<V>, Is.EqualTo(0x7F00s).Within(1s))
        Assert.That(voltsToAdc 0.25<V>, Is.EqualTo(0x0000s).Within(1s))
        Assert.That(voltsToAdc -9.75<V>, Is.EqualTo(0x8100s).Within(1s))
        Assert.That(voltsToAdc 5.25<V>, Is.EqualTo(0x3F80s).Within(1s))

        let voltsToAdc =
            this.PicoScope.GetVoltageToAdcCountConversionAsync(Range._200mV, 0.010<V>)
            |> Async.RunSynchronously

        Assert.That(voltsToAdc 0.210<V>, Is.EqualTo(0x7F00s).Within(1s))
        Assert.That(voltsToAdc 0.010<V>, Is.EqualTo(0x0000s).Within(1s))
        Assert.That(voltsToAdc -0.190<V>, Is.EqualTo(0x8100s).Within(1s))
        Assert.That(voltsToAdc -0.090<V>, Is.EqualTo(0xC080s).Within(1s))

    [<Test>]
    member this.``Can get ADC to voltage conversion function``() =
        let adcToVolts =
            this.PicoScope.GetAdcCountToVoltageConversionAsync(Range._5V, -0.050<V>)
            |> Async.RunSynchronously

        Assert.That(adcToVolts 0x7F00s, Is.EqualTo(4.950<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0x0000s, Is.EqualTo(-0.050<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0x8100s, Is.EqualTo(-5.050<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0x3F80s, Is.EqualTo(2.450<V>).Within(0.0000001<V>))

        let adcToVolts =
            this.PicoScope.GetAdcCountToVoltageConversionAsync(Range._10mV, 0.0<V>)
            |> Async.RunSynchronously

        Assert.That(adcToVolts 0x7F00s, Is.EqualTo(0.010<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0x0000s, Is.EqualTo(0.0<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0x8100s, Is.EqualTo(-0.010<V>).Within(0.0000001<V>))
        Assert.That(adcToVolts 0xC080s, Is.EqualTo(-0.005<V>).Within(0.0000001<V>))

    [<Test>]
    member this.``ADC to volts and volts to ADC are inverse``() =
        let adcToVolts =
            this.PicoScope.GetAdcCountToVoltageConversionAsync(Range._5V, 0.25<V>)
            |> Async.RunSynchronously

        let voltsToAdc =
            this.PicoScope.GetVoltageToAdcCountConversionAsync(Range._5V, 0.25<V>)
            |> Async.RunSynchronously

        Assert.That(adcToVolts (voltsToAdc 0.43<V>), Is.EqualTo(0.43<V>).Within(0.0001<V>))
        Assert.That(adcToVolts (voltsToAdc -3.3<V>), Is.EqualTo(-3.3<V>).Within(0.0001<V>))
        Assert.That(adcToVolts (voltsToAdc 1.75<V>), Is.EqualTo(1.75<V>).Within(0.0001<V>))
        Assert.That(adcToVolts (voltsToAdc 4.65<V>), Is.EqualTo(4.65<V>).Within(0.0001<V>))
        Assert.That(adcToVolts (voltsToAdc -2.25<V>), Is.EqualTo(-2.25<V>).Within(0.0001<V>))

        Assert.That(voltsToAdc (adcToVolts 0x4310s), Is.EqualTo(0x4310s).Within(1s))
        Assert.That(voltsToAdc (adcToVolts 0xF080s), Is.EqualTo(0xF080s).Within(1s))
        Assert.That(voltsToAdc (adcToVolts 0x6210s), Is.EqualTo(0x6210s).Within(1s))
        Assert.That(voltsToAdc (adcToVolts 0x8A0Cs), Is.EqualTo(0x8A0Cs).Within(1s))
        Assert.That(voltsToAdc (adcToVolts 0x3200s), Is.EqualTo(0x3200s).Within(1s))

    // TODO:
    // memory segmentation
    // downsampling ratio