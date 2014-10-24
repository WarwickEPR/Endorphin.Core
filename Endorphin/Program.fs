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
open PicoScope5000Api
open PicoScope5000Agent
open Units

let magnetControllerTest () =
    printfn "Magnet controller example."
    // start a VISA session to the magnet controller's instrument address, obtained from App.config
    use session = 
        "GPIB0::4::INSTR"
        |> ResourceManager.GetLocalManager().Open :?> MessageBasedSession
    Thread.Sleep(1000)

    // create the magnet controller actor with throttled input
    let magnetController = MailboxProcessor.Start <| magnetControllerMailbox session

    // create the ramp manager actor
    let rampManager = MailboxProcessor.Start <| magnetRampManagerMailbox magnetController
    printfn "Press return to send the ramp parameters to the magnet controller..."
    Console.ReadLine() |> ignore

    printfn "Wait for the magnet controller to close the application once the ramp is finished, or return to cancel the ramp and return to zero..."
    
    let ready = new BehaviorSubject<bool>(false)

    let ramp = { startingCurrent = amps 0.3
                 finalCurrent = amps 1.5
                 rampRate = ampsPerSecond 0.05 
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

    Console.ReadLine() |> ignore
    
    magnetController.PostAndReply(PrepareToCloseSession)

let picoScopeTest() =
    // connect to the first available one
    let pico = openFirstConnectedDevice()

    // enable channel A, disable channels B, C and D
    let channelSettings enabled =
        { enabled = enabled
          coupling = Coupling.DC
          range = Range._5V
          analogueOffset = 0.0<V> }
    
    SetChannelSettings(Channel.A, channelSettings true) |> pico.Post
    SetChannelSettings(Channel.B, channelSettings false) |> pico.Post
    SetChannelSettings(Channel.C, channelSettings false) |> pico.Post
    SetChannelSettings(Channel.D, channelSettings false) |> pico.Post

    // auto-trigger after 1ms 
    SetAutoTrigger(1s<ms>) |> pico.Post

    // create a stream agent
    let (streamAgent, agentStatus) = CreateStreamAgent |> pico.PostAndReply
    
    // observe channel A 
    let x = (fun replyChannel -> Observe(Channel.A, Downsampling.None, replyChannel)) 
            |> streamAgent.PostAndReply
    
    // every 1024 * 1024 samples, print "Got some data!"
    x.Buffer(1024 * 1024) 
    |> Observable.add(fun _ -> printfn "Got some data!")
    
    // run the acquisition for about 10s sampling at 1ms intervals and no downsampling
    let streamingParameters =
        { streamingInterval = Microseconds(1u) 
          downsamplingRatio = 1u
          maximumPreTriggerSamples = 0u
          maximumPostTriggerSamples = 1024u * 1024u * 10u
          autoStop = true }
    let streamStatus = (fun replyChannel -> RunStream(streamingParameters, replyChannel)) 
                       |> streamAgent.PostAndReply

    // print stream status updates
    streamStatus 
    |> Observable.add(fun status -> printfn "Stream status: %A" status)

    // wait for the user to press enter
    Console.ReadLine() |> ignore

    // stop the stream if it didn't stop automatically and discard the stream agent
    StopStream |> streamAgent.Post
    Discard |> streamAgent.Post

    // wait for the stream agent to finish and close the connection to the device
    Observable.Wait(agentStatus.DefaultIfEmpty(Discarded)) |> ignore
    CloseUnit |> pico.PostAndReply
    
    
[<EntryPoint>]
let main argv = 
    picoScopeTest()
    0