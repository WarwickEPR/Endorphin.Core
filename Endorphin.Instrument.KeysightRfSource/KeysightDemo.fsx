
#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @".\bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"

open log4net.Config
open Endorphin.Instrument.Keysight.RfSource
open Endorphin.Core.Units

BasicConfigurator.Configure()

let rfInst = openRfInstrument <| visaInstrument("TCPIP0::192.168.1.2")
async {
    let! id = rfInst.identify()
    printfn "ID: %A" id
    rfInst.setPower <| powerInDbm -20.0
    let! power = rfInst.getPower()
    printfn "Power: %A" power
    do! rfInst.close()
} |> Async.RunSynchronously


