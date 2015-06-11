namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Runtime.InteropServices
open ExtCore.Control
open Parsing

[<RequireQualifiedAccess>]
module Acquisition =
    let enabledChannels acquisition = Map.keys acquisition.InputSettings
    
    let hasDownsampling acquisition = 
        acquisition.InputSampling
        |> Set.exists (fun sampling -> sampling.DownsamplingMode <> NoDownsampling) 

    let settingsForChannel inputChannel acquisition = 
        match Map.tryFind  inputChannel acquisition.InputSettings with
        | Some inputSettings -> EnabledChannel inputSettings
        | None               -> DisabledChannel

    let inputSampling acquisition = 
        acquisition.InputSampling |> Set.toSeq

    let private enableChannelSet inputChannels coupling range voltageOffset bandwidth acquisition =
        let repeatedInputs = Set.intersect inputChannels (enabledChannels acquisition)
        if not (repeatedInputs |> Set.isEmpty) then
            failwith "Cannot enable the following inputs which are already enabled: %A" repeatedInputs

        { acquisition with
            InputSettings =
                inputChannels
                |> Set.toSeq
                |> Seq.map (fun inputChannel ->
                    (inputChannel, { Coupling = coupling ; Range = range ; AnalogueOffset = voltageOffset ; BandwidthLimit = bandwidth }))
                |> Map.ofSeq
                |> Map.union acquisition.InputSettings }

    let private sampleChannelSet inputChannels downsamplingMode acquisition =
        if not (Set.isSubset inputChannels (enabledChannels acquisition)) then
            failwith "Cannot acquire an input which has no specified input settings."

        if    (downsamplingMode =  Acquisition.NoDownsampling &&      hasDownsampling acquisition)
           || (downsamplingMode <> Acquisition.NoDownsampling && not (hasDownsampling acquisition)) then
            failwith "Cannot combine inputs with downsampling and no downsampling in the same acquisition."

        { acquisition with
            InputSampling = 
                inputChannels
                |> Set.map (fun channel -> { InputChannel = channel ; DownsamplingMode = downsamplingMode })
                |> Set.union acquisition.InputSampling }
        
    let empty = { InputSampling = Set.empty ; InputSettings = Map.empty }

    let enableChannel  channel  coupling range voltageOffset bandwidth = enableChannelSet (Set.singleton channel) coupling range voltageOffset bandwidth
    let enableChannels channels coupling range voltageOffset bandwidth = enableChannelSet (Set.ofList channels)   coupling range voltageOffset bandwidth

    let sampleChannel  channel  downsamplingMode = sampleChannelSet (Set.singleton channel) downsamplingMode 
    let sampleChannels channels downsamplingMode = sampleChannelSet (Set.ofList channels)   downsamplingMode 

    let internal downsamplingMode acquisition =
        acquisition.InputSampling
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

        let findByInputSampling inputSampling acquisition =
            match inputSampling.DownsamplingMode with
            | NoDownsampling -> SingleBuffer (Map.find (inputSampling.InputChannel, NoDownsamplingBuffer)    acquisition.Buffers)
            | Averaged       -> SingleBuffer (Map.find (inputSampling.InputChannel, AveragedBuffer)          acquisition.Buffers)
            | Decimated      -> SingleBuffer (Map.find (inputSampling.InputChannel, DecimatedBuffer)         acquisition.Buffers)
            | Aggregate      -> BufferPair   (Map.find (inputSampling.InputChannel, AggregateBuffer Maximum) acquisition.Buffers,
                                              Map.find (inputSampling.InputChannel, AggregateBuffer Minimum) acquisition.Buffers) 

        let allocateAcquisitionBuffers memorySegment (SampleIndex bufferLength) acquisition =
            acquisition.InputSampling
            |> Set.toSeq 
            |> Seq.collect (allocateBuffers <| int bufferLength)
            |> Map.ofSeq
            |> createAcquisitionBuffers memorySegment
        
        let private allocateGCHandle obj = GCHandle.Alloc(obj, GCHandleType.Pinned)
        let private disposableForGCHandles (gcHandles : GCHandle seq) =
            { new IDisposable with 
                member __.Dispose() = gcHandles |> Seq.iter (fun handle -> handle.Free()) }
                // TODO run GC before and afer

        let pinningHandle acquisitionBuffers =
            bufferMap acquisitionBuffers
            |> Map.toSeq
            |> Seq.map snd 
            |> Seq.map allocateGCHandle
            |> disposableForGCHandles