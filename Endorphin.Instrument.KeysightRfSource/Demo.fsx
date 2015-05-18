// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight
open log4net.Config
open ExtCore.Control

BasicConfigurator.Configure()

let printResult =
    function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let sweepExperiment startFrequency stopFrequency =
    asyncChoice {
        let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 3000
        let! identity = RfSource.queryIdentity keysight
        printf "%A" identity

        let sweepSettings =
            frequencyStepSweepInHz startFrequency stopFrequency
            |> withPoints 200
            |> withFixedPowerInDbm -3.0<dBm>
            |> withDwellTime (Some (DurationInSec 1e-2<s>))   

        printfn "\nSetting up experiment:\n%A" sweepSettings
        do! RfSource.Sweep.Step.setup keysight sweepSettings
    
    //    do! RfSource.setModulationState keysight Off
    //    do! RfSource.Frequency.setCwFrequency keysight (FrequencyInHz 1.0e9<Hz>)
        let! amplitude = RfSource.Amplitude.queryCwAmplitude keysight
        printfn "%A" amplitude 
    
        do! RfSource.closeInstrument keysight }

sweepExperiment 1.0e9<Hz> 2.0e9<Hz>
|> Async.RunSynchronously
|> printResult


