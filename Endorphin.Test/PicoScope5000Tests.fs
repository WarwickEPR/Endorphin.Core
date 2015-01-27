namespace Endorphin.Test.PicoScope5000

open Endorphin.Core.StringUtils
open Endorphin.Core.Units
open Endorphin.Instrument.PicoScope5000
open NUnit.Framework
open System
open System.Threading
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Reactive.Linq
open Config

[<TestFixture>]
type ``PicoScope 5000 series tests``() = 
    let picoScopeSession = new PicoScope5000Session(picoScope5000serial)
    let _ = log4netConfig()
    
    [<TestFixtureTearDown>]
    member this.``Disconnect from PicoScope``() =
        picoScopeSession.CloseSessionAsync() |> Async.RunSynchronously

    [<TearDown>]
    member this.``Check response after test``() =
        // if the agent crashes then this will fail
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            do! pico.PingAsync() } 
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit driver version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! driverVersion = pico.GetUnitDriverVersionAsync() 
            
            let regex = @"\d\.\d\.\d\.\d"
            match driverVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." driverVersion
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit USB version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! usbVersion = pico.GetUnitUsbVersionAsync() 
            
            let regex = @"\d\.\d"
            match usbVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." usbVersion
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit hardware version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! hardwareVersion = pico.GetUnitHardwareVersionAsync() 
            
            let regex = @"\d"
            match hardwareVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." hardwareVersion
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit model number``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! modelNumber = pico.GetUnitModelNumberAsync() 
            
            let regex = @"\d{4}[AB]"
            match modelNumber with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." modelNumber
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit serial number``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! serial = pico.GetUnitSerialAsync() 
            
            let regex = @"[A-Z&&\d]{5}/\d{3}"
            match serial with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." serial
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit calibration date``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! calibrationDate = pico.GetUnitCalibrationDateAsync() 
            
            Assert.DoesNotThrow((fun () ->
                DateTime.ParseExact(calibrationDate, "ddMMMyy", new Globalization.CultureInfo("en-gb")) |> ignore),
                sprintf "PicoScope response %s does not match expected format." calibrationDate) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit kernel version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! kernelVersion = pico.GetUnitKernelVersionAsync() 
            
            let regex = @"\d\.\d"
            match kernelVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." kernelVersion
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit digital hardware version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! digitalHardwareVersion = pico.GetUnitDigitalHardwareVersionAsync()
             
            let regex = @"\d"
            match digitalHardwareVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." digitalHardwareVersion
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit analogue hardware version``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! analogueHardwareVersion = pico.GetUnitAnalogueHardwareVersionAsync()
             
            let regex = @"\d"
            match analogueHardwareVersion with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." analogueHardwareVersion
                |> Assert.Fail }
        |> Async.RunSynchronously
    
    [<Test>]
    member this.``Can get unit firmware version 1``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! firmwareVersion1 = pico.GetUnitFirmwareVersion1Async()
             
            let regex = @"\d\.\d"
            match firmwareVersion1 with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." firmwareVersion1
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get unit firmware version 2``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            let! firmwareVersion2 = pico.GetUnitFirmwareVersion2Async()
             
            let regex = @"\d\.\d"
            match firmwareVersion2 with
            | ParseRegex regex [] -> 
                Assert.Pass()
            | _ -> 
                sprintf "PicoScope response %s does not match expected format." firmwareVersion2
                |> Assert.Fail }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get all unit info as list``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            
            let! infoList = pico.GetAllUnitInfoAsync() 

            let! driverVersion = pico.GetUnitDriverVersionAsync() 
            let! usbVersion = pico.GetUnitUsbVersionAsync() 
            let! hardwareVersion = pico.GetUnitHardwareVersionAsync() 
            let! modelNumber = pico.GetUnitModelNumberAsync() 
            let! serial = pico.GetUnitSerialAsync() 
            let! calibrationDate = pico.GetUnitCalibrationDateAsync() 
            let! kernelVersion = pico.GetUnitKernelVersionAsync() 
            let! digitalHardwareVersion = pico.GetUnitDigitalHardwareVersionAsync() 
            let! analogueHardwareVersion = pico.GetUnitAnalogueHardwareVersionAsync() 
            let! firmwareVersion1 = pico.GetUnitFirmwareVersion1Async() 
            let! firmwareVersion2 = pico.GetUnitFirmwareVersion2Async() 

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
        
            Assert.AreEqual(expected, infoList) }
        |> Async.RunSynchronously
        
    [<Test>]
    member this.``Can flash LED``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            
            pico.SetLedFlash (LedRepeat 3s)
            do! Async.Sleep 5000

            pico.SetLedFlash LedIndefiniteRepeat
            do! Async.Sleep 5000

            pico.SetLedFlash LedOff }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get analogue offset limits``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let! (upper, lower) = pico.GetAnalogueOffsetLimitsAsync(Range._20V, Coupling.DC) 
            Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
            Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
            Assert.AreEqual(20<V>, upper)

            let! (upper, lower) = pico.GetAnalogueOffsetLimitsAsync(Range._5V, Coupling.AC) 
            Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
            Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
            Assert.AreEqual(20<V>, upper)

            let! (upper, lower) = pico.GetAnalogueOffsetLimitsAsync(Range._200mV, Coupling.DC) 
            Assert.IsTrue(upper > lower, "Lower limit is larger than upper.")
            Assert.IsTrue((upper = abs lower), "Lower and upper limits do not have the same absolute vaue.")         
            Assert.AreEqual(0.25<V>, upper) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can set channel settings``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            pico.SetChannelSettings (Channel.A, (Enabled { 
                Coupling = Coupling.DC 
                Range = Range._2V
                AnalogueOffset = 0.2<V>
                BandwidthLimit = BandwidthLimit._20MHz }))
            
            pico.SetChannelSettings (Channel.B, Disabled)
            pico.SetChannelSettings (Channel.A, Disabled) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can set simple or automatic trigger``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let voltsToAdc = pico.GetVoltageToAdcCountConversion(Range._10V, 0.25<V>)

            pico.SetTrigger (SimpleTrigger
                { Channel = Channel.A
                  AdcThreshold = 5.0<V> |> voltsToAdc
                  ThresholdDirection = ThresholdDirection.Rising
                  DelaySamplesAfterTrigger = 0u
                  AutoTrigger = None })

            pico.SetTrigger (AutoTrigger 500s<ms>) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get voltage to ADC conversion function``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            
            let voltsToAdc = pico.GetVoltageToAdcCountConversion(Range._10V, 0.25<V>)
            Assert.That(voltsToAdc 10.25<V>, Is.EqualTo(0x7F00s).Within(1s))
            Assert.That(voltsToAdc 0.25<V>, Is.EqualTo(0x0000s).Within(1s))
            Assert.That(voltsToAdc -9.75<V>, Is.EqualTo(0x8100s).Within(1s))
            Assert.That(voltsToAdc 5.25<V>, Is.EqualTo(0x3F80s).Within(1s))

            let voltsToAdc = pico.GetVoltageToAdcCountConversion(Range._200mV, 0.010<V>)
            Assert.That(voltsToAdc 0.210<V>, Is.EqualTo(0x7F00s).Within(1s))
            Assert.That(voltsToAdc 0.010<V>, Is.EqualTo(0x0000s).Within(1s))
            Assert.That(voltsToAdc -0.190<V>, Is.EqualTo(0x8100s).Within(1s))
            Assert.That(voltsToAdc -0.090<V>, Is.EqualTo(0xC080s).Within(1s)) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``Can get ADC to voltage conversion function``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()
            
            let adcToVolts = pico.GetAdcCountToVoltageConversion(Range._5V, -0.050<V>)
            Assert.That(adcToVolts 0x7F00s, Is.EqualTo(4.950<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0x0000s, Is.EqualTo(-0.050<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0x8100s, Is.EqualTo(-5.050<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0x3F80s, Is.EqualTo(2.450<V>).Within(0.0000001<V>))

            let adcToVolts = pico.GetAdcCountToVoltageConversion(Range._10mV, 0.0<V>)
            Assert.That(adcToVolts 0x7F00s, Is.EqualTo(0.010<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0x0000s, Is.EqualTo(0.0<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0x8100s, Is.EqualTo(-0.010<V>).Within(0.0000001<V>))
            Assert.That(adcToVolts 0xC080s, Is.EqualTo(-0.005<V>).Within(0.0000001<V>)) }
        |> Async.RunSynchronously

    [<Test>]
    member this.``ADC to volts and volts to ADC are inverse``() =
        async {
            use! pico = picoScopeSession.RequestControlAsync()

            let adcToVolts = pico.GetAdcCountToVoltageConversion(Range._5V, 0.25<V>)
            let voltsToAdc = pico.GetVoltageToAdcCountConversion(Range._5V, 0.25<V>)
            
            Assert.That(adcToVolts (voltsToAdc 0.43<V>), Is.EqualTo(0.43<V>).Within(0.0001<V>))
            Assert.That(adcToVolts (voltsToAdc -3.3<V>), Is.EqualTo(-3.3<V>).Within(0.0001<V>))
            Assert.That(adcToVolts (voltsToAdc 1.75<V>), Is.EqualTo(1.75<V>).Within(0.0001<V>))
            Assert.That(adcToVolts (voltsToAdc 4.65<V>), Is.EqualTo(4.65<V>).Within(0.0001<V>))
            Assert.That(adcToVolts (voltsToAdc -2.25<V>), Is.EqualTo(-2.25<V>).Within(0.0001<V>))

            Assert.That(voltsToAdc (adcToVolts 0x4310s), Is.EqualTo(0x4310s).Within(1s))
            Assert.That(voltsToAdc (adcToVolts 0xF080s), Is.EqualTo(0xF080s).Within(1s))
            Assert.That(voltsToAdc (adcToVolts 0x6210s), Is.EqualTo(0x6210s).Within(1s))
            Assert.That(voltsToAdc (adcToVolts 0x8A0Cs), Is.EqualTo(0x8A0Cs).Within(1s))
            Assert.That(voltsToAdc (adcToVolts 0x3200s), Is.EqualTo(0x3200s).Within(1s)) }
        |> Async.RunSynchronously

    // TODO:
    // memory segmentation
    // downsampling