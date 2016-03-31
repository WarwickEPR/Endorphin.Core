#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/FSharp.Charting.0.90.13/lib/net40/FSharp.Charting.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope5000.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"

open System
open FSharp.Control.Reactive
open Endorphin.Instrument.PicoScope5000

[<AutoOpen>]
module Common =

    let printStatusUpdates acquisition =
        Acquisition.status acquisition
        |> Observable.add (printfn "%A") // print stream status updates (preparing, streaming, finished...) 

    let printSamples inputs acquisition =
        Signal.voltageByTime inputs acquisition
        |> Observable.add (printfn "Sample: %A")

    let printAdc inputs acquisition =
        Signal.adcCount inputs acquisition
        |> Observable.add (printfn "Sample Adc: %A")

    let printBlockCount inputs acquisition =
        Signal.blockSampleCount inputs acquisition
        |> Observable.add (printfn "Block count: %A")

    let printSampled inputs acquisition =
        Signal.voltageByTime inputs acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 50.0)
        |> Observable.add (printfn "Sample: %A")

    let printSampledDigital input acquisition =
        Signal.digitalByteByTime input acquisition
        |> Observable.sample (TimeSpan.FromMilliseconds 50.0)
        |> Observable.add (printfn "Digital Sample: %A")

    let printDigitalTags bit input acquisition =
        Signal.digitalBitByTime bit input acquisition
        |> Signal.digitalEdge Signal.RisingEdge
        |> Observable.add (printfn "Edge: %A")

    let printPulseRate bit inputs acquisition =
        Signal.digitalBitByTime bit inputs acquisition
        |> Signal.digitalEdge Signal.RisingEdge
        |> Observable.bufferSpan (TimeSpan.FromSeconds 0.5)
        |> Observable.add (fun x -> (printfn "Pulse rate: %d /s" (x.Count * 2)))

    let printPulseTotalCount bit input acquisition =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        Signal.digitalBitByTime bit input acquisition
        |> Signal.digitalEdge Signal.RisingEdge
        |> Observable.count
        |> Observable.add (fun x -> let t = timer.ElapsedMilliseconds;
                                    printfn "Received %d pulses in %.1f s. Approx rate: %d ks/s" x (float t*0.001) (x/int t))
    let printRate inputs acquisition =
        Signal.adcCountByTime inputs acquisition
        |> Observable.bufferSpan (TimeSpan.FromSeconds 0.5)
        |> Observable.add (fun x -> (printfn "Rate: %.1f ks/s" (float x.Count * 0.002)))

    let printTotalCount inputs acquisition =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        Signal.adcCountByTime inputs acquisition
        |> Observable.count
        |> Observable.add (fun x -> let t = timer.ElapsedMilliseconds;
                                    printfn "Received %d samples in %.1f s. Approx rate: %d ks/s" x (float t*0.001) (x/int t))

    let printWhenFinished inputs acquisition =
        Signal.adcCount inputs acquisition
        |> Observable.last
        |> Observable.add (fun x -> printfn "Reached the end of the sample stream")
