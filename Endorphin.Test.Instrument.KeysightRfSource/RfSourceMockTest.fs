namespace Endorphin.Instrument.Keysight

open NUnit.Framework
open Endorphin.Instrument.Keysight.RfSource
open Endorphin.Core.Units
open System
open Utils

module ``Keysight RF Mock Test`` =

    let cwd = IO.Directory.GetCurrentDirectory()
    let filename = sprintf "%s\\%s" cwd "testoutput.log"
    let log = configureLogToFile "RfSource mock test" filename

    [<TestCase("identifying string")>]
    let identify (str : string) =
        let cmdtest (cmd : string) =
            Assert.AreEqual("*IDN?",cmd)
            ()
        let response = (queryMock str cmdtest).identify() |> run
        Assert.AreEqual(str,response)
        ()
    
    [<TestCase("ON",true)>]
    [<TestCase("OFF",false)>]
    let isRfOn (str : string) (result : bool) =
        let response = (readMock str).isRfOn() |> run
        Assert.AreEqual(result,response)
        ()

    [<TestCase("ON",false)>]
    [<TestCase("OFF",true)>]
    let isRfOff (str : string) (result : bool) =
        let response = (readMock str).isRfOff() |> run
        Assert.AreEqual(result,response)
        ()

    [<TestCase("2.0",2.0)>]
    let getPowerDbm (str : string) (power : float) =
        let response = (readMock str).getPower() |> run
        Assert.AreEqual(powerInDbm(power),response)
        ()

    [<TestCase("2.0e9",2.0e9)>]
    let getFrequency (str : string) (freq : float) =
        let response = (readMock str).getFrequency() |> run
        Assert.AreEqual(frequencyInHz(freq),response)
        ()




