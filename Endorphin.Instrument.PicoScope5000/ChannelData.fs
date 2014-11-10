namespace Endorphin.Instrument.PicoScope5000

open System
open System.Runtime.InteropServices

type ChannelData(channel : Channel, downsampling : Downsampling) = 
    let voltageOverflow = new Event<unit>()
    let sampleBlock = new Event<int16 array * TriggerPosition>()
    let completed = new Event<unit>()

    do
        if not (Array.exists (fun inputChannel -> inputChannel = channel) 
                 [| Channel.A ; Channel.B ; Channel.C ; Channel.D |]) then
            invalidArg "channel" "Cannot stream non-input channel" channel
        if downsampling = Downsampling.Aggregate then
            invalidArg "downsampling" 
                "Cannot create ChannelStream with aggregate downsampling. Use ChannelAggregateStream instead." 
                downsampling
    
    member internal this.ProcessDataReadyAsync(dataBuffer, startIndex : uint32, numberOfSamples, didOverflow, triggerPosition) =
        async {
            if didOverflow then voltageOverflow.Trigger()

            let samples = Array.zeroCreate(numberOfSamples)
            Array.Copy(dataBuffer, int startIndex, samples, 0, numberOfSamples)
        
            (samples, triggerPosition)
            |> sampleBlock.Trigger }

    member internal this.TriggerCompleted() = completed.Trigger()
    
    [<CLIEvent>]
    member this.VoltageOverflow = voltageOverflow.Publish

    [<CLIEvent>]
    member this.SampleBlock = sampleBlock.Publish
    
    [<CLIEvent>]
    member this.Completed = completed.Publish

    member this.Channel = channel
    member this.Downsampling = downsampling

type internal Buffer = 
    { dataBuffer : int16 array
      gcHandle : GCHandle
      channelStream : ChannelData }
