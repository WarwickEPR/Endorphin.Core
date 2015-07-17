namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Runtime.InteropServices
open ExtCore.Control
open Parsing

[<RequireQualifiedAccess>]
module Inputs =
    let enabledChannels inputs = Map.keys inputs.InputSettings
    
    let hasDownsampling inputs = 
        inputs.InputSampling
        |> Set.exists (fun sampling -> sampling.DownsamplingMode <> NoDownsampling) 

    let settingsForChannel inputChannel acquisition = 
        match Map.tryFind  inputChannel acquisition.InputSettings with
        | Some inputSettings -> EnabledChannel inputSettings
        | None               -> DisabledChannel

    let inputSampling inputs = 
        inputs.InputSampling |> Set.toSeq

    let private enableChannelSet inputChannels coupling range voltageOffset bandwidth inputs =
        let repeatedInputs = Set.intersect inputChannels (enabledChannels inputs)
        if not (repeatedInputs |> Set.isEmpty) then
            failwith "Cannot enable the following inputs which are already enabled: %A" repeatedInputs

        { inputs with
            InputSettings =
                inputChannels
                |> Set.toSeq
                |> Seq.map (fun inputChannel ->
                    (inputChannel, { Coupling = coupling ; Range = range ; AnalogueOffset = voltageOffset ; BandwidthLimit = bandwidth }))
                |> Map.ofSeq
                |> Map.union inputs.InputSettings }

    let private sampleChannelSet inputChannels downsamplingMode inputs =
        if not (Set.isSubset inputChannels (enabledChannels inputs)) then
            failwith "Cannot acquire an input which has no specified input settings."

        if    (downsamplingMode =  Acquisition.NoDownsampling &&      hasDownsampling inputs)
           || (downsamplingMode <> Acquisition.NoDownsampling && not (hasDownsampling inputs)) then
            failwith "Cannot combine inputs with downsampling and no downsampling in the same acquisition."

        { inputs with
            InputSampling = 
                inputChannels
                |> Set.map (fun channel -> { InputChannel = channel ; DownsamplingMode = downsamplingMode })
                |> Set.union inputs.InputSampling }
        
    let none = { InputSampling = Set.empty ; InputSettings = Map.empty }

    let enableChannel  channel  coupling range voltageOffset bandwidth = enableChannelSet (Set.singleton channel) coupling range voltageOffset bandwidth
    let enableChannels channels coupling range voltageOffset bandwidth = enableChannelSet (Set.ofList channels)   coupling range voltageOffset bandwidth

    let sampleChannel  channel  downsamplingMode = sampleChannelSet (Set.singleton channel) downsamplingMode 
    let sampleChannels channels downsamplingMode = sampleChannelSet (Set.ofList channels)   downsamplingMode 

    let internal downsamplingMode inputs =
        inputs.InputSampling
        |> Set.map (fun sampling -> sampling.DownsamplingMode)
        |> downsamplingModeEnumForSet

    module internal Buffers =

        let bufferMap     acquisitionBuffers = acquisitionBuffers.Buffers
        let memorySegment acquisitionBuffers = acquisitionBuffers.MemorySegment

        let private createAcquisitionBuffers memorySegment buffers = { Buffers = buffers ; MemorySegment = memorySegment }
        
        let private allocateBuffers bufferLength inputSampling =
            match inputSampling.DownsamplingMode with
            | NoDownsampling -> [ NoDownsamplingBuffer ]
            | Averaged       -> [ AveragedBuffer ]
            | Decimated      -> [ DecimatedBuffer ]
            | Aggregate      -> [ AggregateBuffer Maximum ; AggregateBuffer Minimum ]
            |> List.map (fun buffer -> (inputSampling.InputChannel, buffer), Array.zeroCreate<AdcCount> bufferLength)

        let findByInputSampling inputSampling inputs =
            match inputSampling.DownsamplingMode with
            | NoDownsampling -> SingleBuffer (Map.find (inputSampling.InputChannel, NoDownsamplingBuffer)    inputs.Buffers)
            | Averaged       -> SingleBuffer (Map.find (inputSampling.InputChannel, AveragedBuffer)          inputs.Buffers)
            | Decimated      -> SingleBuffer (Map.find (inputSampling.InputChannel, DecimatedBuffer)         inputs.Buffers)
            | Aggregate      -> BufferPair   (Map.find (inputSampling.InputChannel, AggregateBuffer Maximum) inputs.Buffers,
                                              Map.find (inputSampling.InputChannel, AggregateBuffer Minimum) inputs.Buffers) 

        let allocateAcquisitionBuffers memorySegment (SampleIndex bufferLength) inputs =
            GC.Collect() // force garbage collection before allocating
            inputs.InputSampling
            |> Set.toSeq 
            |> Seq.collect (allocateBuffers <| int bufferLength)
            |> Map.ofSeq
            |> createAcquisitionBuffers memorySegment
        
        let private allocateGCHandle obj = GCHandle.Alloc(obj, GCHandleType.Pinned)
        let private disposableForGCHandles (gcHandles : GCHandle seq) =
            { new IDisposable with 
                member __.Dispose() = 
                    gcHandles |> Seq.iter (fun handle -> handle.Free())
                    GC.Collect() } // force garbage collection immediately after freeing the buffers

        let pinningHandle acquisitionBuffers =
            bufferMap acquisitionBuffers
            |> Map.toSeq
            |> Seq.map snd 
            |> Seq.map allocateGCHandle
            |> disposableForGCHandles