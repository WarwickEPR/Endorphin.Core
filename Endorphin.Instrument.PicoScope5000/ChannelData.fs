namespace Endorphin.Instrument.PicoScope5000

open System
open System.Reactive.Linq
open System.Runtime.InteropServices
open log4net

type ChannelData(channel : Channel, downsampling : Downsampling) = 
    let log = LogManager.GetLogger typeof<ChannelData>

    let voltageOverflow = new Event<unit>()
    let sampleBlock = new Event<int16 array * TriggerPosition>()
    let completed = new Event<unit>()
    let error = new Event<Exception>()

    do
        (sprintf "Initialising channel %A data with %A downsampling." channel downsampling) |> log.Info

        if not (Array.exists (fun inputChannel -> inputChannel = channel) 
                 [| Channel.A ; Channel.B ; Channel.C ; Channel.D |]) then
            invalidArg "channel" "Cannot stream non-input channel" channel
        if downsampling = Downsampling.Aggregate then
            invalidArg "downsampling" 
                "Cannot create ChannelStream with aggregate downsampling. Use ChannelAggregateStream instead." 
                downsampling
    
    member internal this.ProcessDataReadyAsync(dataBuffer, startIndex : uint32, numberOfSamples, didOverflow, triggerPosition) =
        async {
            (sprintf "Reading %d samples for channel %A buffer." numberOfSamples channel) |> log.Info
            
            (sprintf "Channel %A voltage %s overflow." channel (if didOverflow then "did" else "did not")) |> log.Info
            if didOverflow then voltageOverflow.Trigger()

            let samples = Array.zeroCreate(numberOfSamples)
            Array.Copy(dataBuffer, int startIndex, samples, 0, numberOfSamples)
        
            (sprintf "Channel %A data triggering sample block event." channel) |> log.Info
            (samples, triggerPosition)
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
    member this.Downsampling = downsampling
    
    member this.Samples =
        let throw : (exn -> int16) = fun exn -> raise exn
        let failure = Observable.Select(this.Error, throw)
        let success =
            Observable.SelectMany(this.SampleBlock, fun (block, _) -> Array.toSeq block)
                      .TakeUntil(this.Completed)
        Observable.merge success failure

type internal Buffer = 
    { dataBuffer : int16 array
      channelStream : ChannelData }
