// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"

open Endorphin.Core
open log4net.Config

BasicConfigurator.Configure()

async {
    let rfSource = NationalInstruments.openInstrument "TCPIP0::192.168.1.2"
    let! id = rfSource.Query "*IDN?"
    printf "%A" id
    do! rfSource.Close() }
|> Async.RunSynchronously




// Define your library scripting code here

