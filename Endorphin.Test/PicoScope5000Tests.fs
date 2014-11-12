namespace Endorphin.Test

open Endorphin.Core.Utils
open Endorphin.Instrument.PicoScope5000
open Devices
open NUnit.Framework
open System

[<TestFixture>]
type ``PicoScope 5000 series tests``() = 
    let picoScope : PicoScope5000 option ref = ref None

    member this.PicoScope =
        match !picoScope with
        | Some(scope) -> scope
        | None -> raise (new NullReferenceException())

    [<TestFixtureSetUp>]
    member this.``Connect to PicoScope``() =
        picoScope := Some(new PicoScope5000())

        let isMainsPowered =
            this.PicoScope.GetUnitIsMainsPoweredAsync()
            |> Async.RunSynchronously
        Assert.IsTrue(isMainsPowered, 
            "The PicoScope is not mains-powered. These unit test assume that the device has mains-power.")

    [<TestFixtureTearDown>]
    member this.``Disconnect from PicoScope``() =
        (this.PicoScope :> IDisposable).Dispose()
        picoScope := None

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
        
