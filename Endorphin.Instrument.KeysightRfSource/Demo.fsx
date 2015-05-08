// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight
open log4net.Config

BasicConfigurator.Configure()

async {
    let keysight = RfSource.openInstrument "TCPIP0::192.168.1.2"
    let! id = RfSource.queryIdentity keysight
    printf "%A" id
    
    RfSource.setModulationState keysight Off
    RfSource.Frequency.setCwFrequency keysight (FrequencyInHz 1.0e9<Hz>)
    let! amplitude = RfSource.Amplitude.queryCwAmplitude keysight
    printfn "%A" amplitude 
    
    do! RfSource.closeInstrument keysight }
|> Async.RunSynchronously

// Define your library scripting code here

