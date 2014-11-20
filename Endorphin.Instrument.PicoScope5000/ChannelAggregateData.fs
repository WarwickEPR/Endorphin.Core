namespace Endorphin.Instrument.PicoScope5000

open System
open System.Reactive.Linq
open System.Runtime.InteropServices
open log4net

type ChannelAggregateData(channel : Channel) =
    static let log = LogManager.GetLogger typeof<ChannelAggregateData>

    let voltageOverflow = new Event<unit>()
    let sampleBlock = new Event<int16 array * int16 array * TriggerPosition>()
    let completed = new Event<unit>()
    let error = new Event<Exception>()
    
    do
        (sprintf "Initialising channel %A aggregate data.") |> log.Info

        if not (Array.exists (fun inputChannel -> inputChannel = channel) 
                 [| Channel.A ; Channel.B ; Channel.C ; Channel.D |]) then
            invalidArg "channel" "Cannot stream non-input channel" channel

    member internal this.ProcessDataReadyAsync(maxDataBuffer, minDataBuffer, startIndex : uint32, numberOfSamples, didOverflow, triggerPosition) =
        async {
            (sprintf "Reading %d samples for channel %A aggregate buffers." numberOfSamples channel) |> log.Info

            (sprintf "Channel %A voltage %s overflow." channel (if didOverflow then "did" else "did not")) |> log.Info
            if didOverflow then voltageOverflow.Trigger()

            let maxSamples = Array.zeroCreate(numberOfSamples)
            let minSamples = Array.zeroCreate(numberOfSamples)
            Array.Copy(maxDataBuffer, int startIndex, maxSamples, 0, numberOfSamples)
            Array.Copy(minDataBuffer, int startIndex, minSamples, 0, numberOfSamples)
        
            (sprintf "Channel %A aggregate data triggering sample block event." channel) |> log.Info
            (maxSamples, minSamples, triggerPosition)
            |> sampleBlock.Trigger }

    member internal this.TriggerError exn =
        (sprintf "Channel %A aggregate data triggering error event." channel) |> log.Info
        error.Trigger exn

    member internal this.TriggerCompleted() =
        (sprintf "Channel %A aggregate data triggering completed event." channel) |> log.Info
        completed.Trigger()
    
    [<CLIEvent>]
    member this.VoltageOverflow = voltageOverflow.Publish

    [<CLIEvent>]
    member this.SampleBlock = sampleBlock.Publish
    
    [<CLIEvent>]
    member this.Completed = completed.Publish
    
    [<CLIEvent>]
    member this.Error = error.Publish

    member this.Channel = channel
    
    member this.Samples =
        let throw : (exn -> int16 * int16) = fun exn -> raise exn
        let failure = Observable.Select(this.Error, throw)
        let success =
            Observable.SelectMany(this.SampleBlock, fun (blockMax, blockMin, _) -> 
                Array.zip blockMax blockMin
                |> Array.toSeq)
                .TakeUntil(this.Completed)
        Observable.merge success failure

type internal AggregateBuffer =
    { dataBufferMax : int16 array
      dataBufferMin : int16 array
      channelAggregateStream : ChannelAggregateData }
