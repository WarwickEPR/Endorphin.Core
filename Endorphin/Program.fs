// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Control
open NationalInstruments.VisaNS
open MagnetController
open MagnetRampManager
open System
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Threading
open Units
open Endorphin.Instrument.PicoScope5000

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

    let ramp = 
      { startingCurrent = amps 0.3
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
    do
        use pico = new PicoScope5000Agent()
        // enable channel A, disable channels B, C and D
        let inputRange = Range._10mV
    
        let channelSettings enabled =
          { enabled = enabled
            coupling = Coupling.DC
            range = inputRange
            analogueOffsetInVolts = 0.0 }
    
        pico.SetChannelSettings(Channel.A, channelSettings true)
        pico.SetChannelSettings(Channel.B, channelSettings false)
        pico.SetChannelSettings(Channel.C, channelSettings false)
        pico.SetChannelSettings(Channel.D, channelSettings false)

        // auto-trigger after 1ms 
        pico.SetAutoTrigger(1s)

        let adcToVolts = pico.GetAdcCountToVoltsConversion(inputRange, 0.0)
    
        // create a stream agent
        do
            use streamAgent = pico.CreateStreamAgent()
            // observe channel A 
            let x = streamAgent.Observe(Channel.A, Downsampling.None) 
    
            // print samples in 64 sample blocks
            x.Buffer(64) 
            |> Observable.add(
                   fun block ->
                       printfn "Sample block:"
                       for sample in block do
                           printf "%.2fmV " (1000.0 * adcToVolts sample)
                       printfn "")
    
            // run the acquisition for about 10s sampling at 1ms intervals and no downsampling
            let streamingParameters =
              { streamingInterval = Milliseconds(1u) 
                downsamplingRatio = 1u
                maximumPreTriggerSamples = 0u
                maximumPostTriggerSamples = uint32 (64 * 32)
                autoStop = true }

            let streamStatus = streamAgent.RunStream(streamingParameters)

            // print stream status updates
            streamStatus 
            |> Observable.add(fun status -> printfn "Stream status: %A" status)

            // wait for the user to press enter
            Console.ReadLine() |> ignore

            // stop the stream if it didn't stop automatically
            streamAgent.StopStream()
        )
    )
    
    
[<EntryPoint>]
let main argv = 
    picoScopeTest()
    0