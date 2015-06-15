#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope5000.dll"
#r "bin/Debug/ExtCore.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.PicoScope5000
open ExtCore.Control
open System

let defer f = { new IDisposable with member __.Dispose() = f () }

let parameters = 
    let setupChannels =
        Acquisition.empty
        |> Acquisition.enableChannel ChannelA DC Range_200mV Voltage.zero FullBandwidth
        |> Acquisition.sampleChannel ChannelA NoDownsampling
        |> StreamingAcquisition.withNoDownsampling
    
    StreamingAcquisition.createParameters Resolution_14bit (Interval.fromMilliseconds 20<ms>) (1024u * 1024u)
    |> setupChannels

asyncChoice {
    let! picoScope = PicoScope.openFirst Resolution_8bit
    use connection = defer (fun () -> PicoScope.closeInstrument picoScope |> ignore)
    
    let acquisition = StreamingAcquisition.createAcquisition parameters
    
    StreamingAcquisition.sampleObserved ChannelA NoDownsamplingBuffer acquisition
    |> Observable.add (printfn "%d")

    StreamingAcquisition.statusChanged acquisition
    |> Observable.add (printfn "%A")
    
    let! acquisitionHandle = StreamingAcquisition.start picoScope acquisition
    
    do! Async.Sleep 30000 |> AsyncChoice.liftAsync
    do! StreamingAcquisition.stop picoScope acquisitionHandle }
|> Async.RunSynchronously
|> printfn "%A"