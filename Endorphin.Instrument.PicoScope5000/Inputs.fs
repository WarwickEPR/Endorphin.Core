// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Runtime.InteropServices
open Parsing

[<RequireQualifiedAccess>]
/// Functions for specifying acquisition inputs and sampling for a PicoScope 5000 series acquisition.
module internal Inputs =

    /// Returns the set of enabled channels in the given acquisition inputs.
    let enabledChannels inputs = Map.keys inputs.InputSettings
    
    /// Indicates whether the given acquisition inputs use downsampling.
    let hasDownsampling inputs = 
        inputs.InputSampling
        |> Set.exists (fun sampling -> sampling.DownsamplingMode <> NoDownsampling) 

    /// Returns the channel settings specified for the given input channel in the acquisition inputs.
    let settingsForChannel inputChannel acquisition = 
        match Map.tryFind  inputChannel acquisition.InputSettings with
        | Some inputSettings -> EnabledChannel inputSettings
        | None               -> DisabledChannel

    /// Returns a sequence of all inputs channels and their respective downsampling modes (each channel can
    /// have multiple downsampling modes) for the given acquisition inputs.
    let inputSampling inputs = 
        inputs.InputSampling |> Set.toSeq

    /// Returns a modified set of acquisition inputs which has the specified set of input channels enabled
    /// with the given input settings. Fails if a channel in the set is already enabled.
    let private enableChannelSet inputChannels coupling range voltageOffset bandwidth inputs =
        let repeatedInputs = Set.intersect inputChannels (enabledChannels inputs)
        if not (repeatedInputs |> Set.isEmpty) then
            failwithf "Cannot enable the following inputs which are already enabled: %A" repeatedInputs

        { inputs with
            InputSettings =
                inputChannels
                |> Set.toSeq
                |> Seq.map (fun inputChannel ->
                    (inputChannel, { Coupling = coupling ; Range = range ; AnalogueOffset = voltageOffset ; BandwidthLimit = bandwidth }))
                |> Map.ofSeq
                |> Map.union inputs.InputSettings }
    
    /// Returns a modified set of acquisition inputs which has the specified set of input channels sampled
    /// with the given downsampling mode. Note that an input channel must first be enabled before sampling
    /// can be set. Also note that "no downsampling" cannot be combined with other downsampling modes.
    let private sampleChannelSet inputChannels downsamplingMode inputs =
        if not (Set.isSubset inputChannels (enabledChannels inputs)) then
            failwith "Cannot acquire an input which has no specified input settings."

        if inputs.InputSampling <> Set.empty then
            if    (downsamplingMode =  Acquisition.NoDownsampling &&      hasDownsampling inputs)
               || (downsamplingMode <> Acquisition.NoDownsampling && not (hasDownsampling inputs)) then
                failwith "Cannot combine inputs with downsampling and no downsampling in the same acquisition."

        { inputs with
            InputSampling = 
                inputChannels
                |> Set.map (fun channel -> { InputChannel = channel ; DownsamplingMode = downsamplingMode })
                |> Set.union inputs.InputSampling }
    
    /// Returns an empty set of acquisition inputs.
    let none = { InputSampling = Set.empty ; InputSettings = Map.empty }

    /// Returns a modified set of acquisition inputs which has the specified input channel enabled with the
    /// given input settings. Fails if the channel is already enabled.
    let enableChannel  channel  coupling range voltageOffset bandwidth = enableChannelSet (Set.singleton channel) coupling range voltageOffset bandwidth

    /// Returns a modified set of acquisition inputs which has the specified list of input channels enabled
    /// with the given input settings. Fails if any of the channels in the set is already enabled.
    let enableChannels channels coupling range voltageOffset bandwidth = enableChannelSet (Set.ofList channels)   coupling range voltageOffset bandwidth

    /// Returns a modified set of acquisition inputs which has the specified input channel sampled with
    /// the given downsampling mode. Fails if the channel is not enabled. Also fails if the acquisition
    /// has inputs which are sampled with no downsampling while another downsampling mode is given or
    /// vice-versa.
    let sampleChannel  channel  downsamplingMode = sampleChannelSet (Set.singleton channel) downsamplingMode 
    
    /// Returns a modified set of acquisition inputs which has the specified list of input channels
    /// sampled with the given downsampling mode. Fails if the channel is not enabled. Also fails if the
    /// acquisition has inputs which are sampled with no downsampling while another downsampling mode is
    /// given or vice-versa.
    let sampleChannels channels downsamplingMode = sampleChannelSet (Set.ofList channels)   downsamplingMode 

    /// Returns the downsampling mode enumeration value for the given set of acquisition inputs which contains
    /// all required downsampling modes combined together using bitwise OR.
    let downsamplingMode inputs =
        inputs.InputSampling
        |> Set.map (fun sampling -> sampling.DownsamplingMode)
        |> downsamplingModeEnumForSet
    
    /// Functions related to creating and managing acquisition buffers.
    module Buffers =
        
        /// Returns the map from input channel and buffer downsampling to acquisition buffer for the given set
        /// of acquisition buffers.
        let bufferMap     acquisitionBuffers = acquisitionBuffers.Buffers

        /// Gives the memory segment from which data is being transferred into the acquisition buffers.
        let memorySegment acquisitionBuffers = acquisitionBuffers.MemorySegment

        /// Creates a set of acquisition buffers with the specified memory segment.
        let private createAcquisitionBuffers memorySegment buffers = { Buffers = buffers ; MemorySegment = memorySegment }

        /// Finds the buffer corresponding to the given input sampling (input channel and downsampling mode)
        /// in the given set of acquisition buffers.
        let findByInputSampling inputSampling buffers =
            match inputSampling.DownsamplingMode with
            | NoDownsampling -> SingleBuffer (Map.find (inputSampling.InputChannel, NoDownsamplingBuffer)    buffers.Buffers)
            | Averaged       -> SingleBuffer (Map.find (inputSampling.InputChannel, AveragedBuffer)          buffers.Buffers)
            | Decimated      -> SingleBuffer (Map.find (inputSampling.InputChannel, DecimatedBuffer)         buffers.Buffers)
            | Aggregate      -> BufferPair   (Map.find (inputSampling.InputChannel, AggregateBuffer Maximum) buffers.Buffers,
                                              Map.find (inputSampling.InputChannel, AggregateBuffer Minimum) buffers.Buffers) 
        
        /// Allocates acquisition buffers for the given input sampling (input channel and downsampling mode)
        /// and buffer length.
        let private allocateBuffers bufferLength inputSampling =
            match inputSampling.DownsamplingMode with
            | NoDownsampling -> [ NoDownsamplingBuffer ]
            | Averaged       -> [ AveragedBuffer ]
            | Decimated      -> [ DecimatedBuffer ]
            | Aggregate      -> [ AggregateBuffer Maximum ; AggregateBuffer Minimum ]
            |> List.map (fun buffer -> (inputSampling.InputChannel, buffer), Array.zeroCreate<AdcCount> bufferLength)
    
        /// Allocates acquisition buffers for the given set of acquisition inputs and memory segment with
        /// the given buffer length.
        let allocateAcquisitionBuffers memorySegment (bufferLength : SampleIndex) inputs =
            GC.Collect() // force garbage collection before allocating

            inputs.InputSampling
            |> Set.toSeq 
            |> Seq.collect (allocateBuffers <| int bufferLength)
            |> Map.ofSeq
            |> createAcquisitionBuffers memorySegment
        
        /// Allocates a pinned garbage collector handle for the given object.
        let private allocatePinnedGCHandle obj = GCHandle.Alloc(obj, GCHandleType.Pinned)

        /// Creates an instance of IDisposable which will free the given sequence of garbage collector
        /// handles when the Dispose method is called, and then force garbage collection immediately
        /// so that the memory is released.
        let private disposableForGCHandles (gcHandles : GCHandle seq) =
            { new IDisposable with 
                member __.Dispose() = 
                    gcHandles |> Seq.iter (fun handle -> handle.Free())
                    GC.Collect() }

        /// Creates a pinning handle for the given acquisition buffers which will prevent the garbage
        /// collector from moving them in order to defragment managed memory. This is required for native
        /// code interoperability as the native code is unaware of managed memory. The returned IDisposable
        /// will release all allocated pinning handles, allowing the memory to be freed.
        let createPinningHandle acquisitionBuffers =
            bufferMap acquisitionBuffers
            |> Map.toSeq
            |> Seq.map (snd >> allocatePinnedGCHandle)
            |> disposableForGCHandles