namespace Endorphin.Instrument.Keysight

open NUnit.Framework
open Endorphin.Instrument.Keysight.RfSource
open Endorphin.Core.Units
open System
open Utils

  [<TestFixture>]
  type ``Keysight RF Live Test`` () =
    let cwd = IO.Directory.GetCurrentDirectory()
    let logfile = sprintf "%s\\%s" cwd "livetestoutput.log"
    let log = configureLogToFile "RfSource live test" logfile
    let rfI = openRfInstrument <| visaInstrument("TCPIP0::192.168.1.2")

    [<Test>]
    member t.identify () =
        let response = rfI.identify() |> run
        Assert.IsTrue(response.Contains("N5172B"))
        ()
    
    [<TestCase(-2.0)>]
    member t.power (power : float) =
        rfI.setPower <| powerInDbm power
        let newPower = rfI.getPower () |> run
        Assert.AreEqual(powerInDbm(power), newPower)
        ()

    [<TestCase(3.21e9)>]
    member t.frequency (freq : float) =
        rfI.setFrequency <| frequencyInHz freq
        let nfreq = rfI.getFrequency () |> run
        Assert.AreEqual( frequencyInHz(freq), nfreq)
        ()
