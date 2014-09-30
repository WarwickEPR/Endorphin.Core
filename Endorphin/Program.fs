// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Control
open NationalInstruments.VisaNS
open MagnetController
open MagnetRampManager
open System
open System.Linq
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Threading
open System.Collections.Generic
open PicoScopeDriver


(* let magnetControllerTest =
    printfn "Magnet controller example."
    // start a VISA session to the magnet controller's instrument address, obtained from App.config
    use session = 
        "GPIB0::4::INSTR"
        |> ResourceManager.GetLocalManager().Open :?> MessageBasedSession

    // create the magnet controller actor with throttled input
    let magnetController = MailboxProcessor.Start <| magnetControllerMailbox session

    // create the ramp manager actor
    let rampManager = MailboxProcessor.Start <| magnetRampManagerMailbox magnetController
    printfn "Press return to send the ramp parameters to the magnet controller..."
    Console.ReadLine() |> ignore

    printfn "Wait for the magnet controller to close the application once the ramp is finished, or return to cancel the ramp and return to zero..."
    
    let ready = new BehaviorSubject<bool>(false)

    let ramp = { startingCurrent = 0.3
                 finalCurrent = 1.5
                 rampRate = 0.05 
                 returnToZero = true
                 readyForRamp = ready.AsObservable() }
    
    let rampStatus = rampManager.PostAndReply(fun replyChannel -> PerformRamp(ramp, replyChannel))
    use printStatus = rampStatus
                      |> Observable.subscribe(fun status ->
                          printfn "Ramp status: %A" status
                          if status = ReadyToRamp
                          then printfn "Acquisition getting ready..."
                               Thread.Sleep(1000)
                               ready.OnNext(true))

    let didFinish = Observable.IgnoreElements(rampStatus)
                    |> Observable.map (fun _ -> true)
    
    let didNotFinish = Observable.Start(Console.ReadLine)
                       |> Observable.map (fun _ -> false)
    
    use cancelOnReturn = Observable.Amb(didFinish, didNotFinish)
                         |> Observable.subscribe (fun finished -> if finished = false then rampManager.Post(CancelRamp(true)))

    Console.ReadLine() |> ignore *)

let picoscopeTest =
    // find all connected PicoScopes
    let connectedPicos = PicoScope5000.GetConnectedUnitSerials()
    if connectedPicos.Length = 0 then failwith "No PicoScopes found."

    // connect to the first available one
    use pico = new PicoScope5000(connectedPicos.First())
    // print device details
    let info = pico.GetUnitInfo()
    for detail in info do
        printfn "%A" detail

    Console.ReadLine() |> ignore

[<EntryPoint>]
let main argv = 
    picoscopeTest
    0