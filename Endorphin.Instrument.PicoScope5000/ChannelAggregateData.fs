namespace Endorphin.Instrument.PicoScope5000

open System
open System.Runtime.InteropServices

type ChannelAggregateData(channel : Channel) =
    let voltageOverflow = new Event<unit>()
    let sampleBlock = new Event<int16 array * int16 array * TriggerPosition>()
    let completed = new Event<unit>()
    
    do
        if not (Array.exists (fun inputChannel -> inputChannel = channel) 
                 [| Channel.A ; Channel.B ; Channel.C ; Channel.D |]) then
            invalidArg "channel" "Cannot stream non-input channel" channel

    member internal this.ProcessDataReadyAsync(maxDataBuffer, minDataBuffer, startIndex : uint32, numberOfSamples, didOverflow, triggerPosition) =
        async {
            if didOverflow then voltageOverflow.Trigger()

            let maxSamples = Array.zeroCreate(numberOfSamples)
            let minSamples = Array.zeroCreate(numberOfSamples)
            Array.Copy(maxDataBuffer, int startIndex, maxSamples, 0, numberOfSamples)
            Array.Copy(minDataBuffer, int startIndex, minSamples, 0, numberOfSamples)
        
            (maxSamples, minSamples, triggerPosition)
            |> sampleBlock.Trigger }

    member internal this.TriggerCompleted() = completed.Trigger()
    
    [<CLIEvent>]
    member this.VoltageOverflow = voltageOverflow.Publish

    [<CLIEvent>]
    member this.SampleBlock = sampleBlock.Publish
    
    [<CLIEvent>]
    member this.Completed = completed.Publish

    member this.Channel = channel

type internal AggregateBuffer =
    { dataBufferMax : int16 array
      dataBufferMin : int16 array
      channelAggregateStream : ChannelAggregateData }
