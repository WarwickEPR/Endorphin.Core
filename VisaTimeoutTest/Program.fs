// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Endorphin.Core
open log4net.Config
open System.Threading

BasicConfigurator.Configure() |> ignore

let wait () =
    printfn "Sleeping until keypress"
    System.Console.ReadKey true |> ignore
    printfn "and continuing"


let waittime t = async {
    printfn "Sleeping %d ms" t
    do! Async.Sleep t
    printfn "and continuing" }

let waitt = async { do! waittime (15*1000) }


[<EntryPoint>]
let main argv = 
    async {
        let rfSource = NationalInstruments.openInstrument "TCPIP0::192.168.1.3"
        do wait()
        let! id = rfSource.Query "*IDN?"
        printf "%A" id
        do! rfSource.Close() }
    |> Async.RunSynchronously
    do wait()
    0
