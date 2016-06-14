namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open System

[<AutoOpen>]
module internal AcquisitionCommon =

    /// extract the generic acquisition basis from any type of acquisition
    let internal common = function
    | StreamingAcquisition acquisition  -> acquisition.Common
    | BlockAcquisition acquisition      -> acquisition.Common
    | RapidBlockAcquisition acquisition -> acquisition.Acquisition.Common

    /// Gives a list of the keys for all the output channels in the acquisition
    let channels acquisition =
        Map.keys (common acquisition).DataBuffers.[0] |> Set.toList

    /// function to set up devices and allocate buffers
    module Common =

        /// Gives a list of the keys for all the output channels in the acquisition
        let channels acq =
            Map.keys acq.DataBuffers.[0] |> Set.toList

        /// allocate and return new acquisition buffers using given buffer length
        let allocateStreamingBuffer (parameters:AcquisitionParameters) =
            Inputs.Buffers.allocateAcquisitionBuffers
                <| 1u
                <| parameters.BufferLength
                <| parameters.Inputs

        /// allocate and return new acquisition buffers sized to take all the acquired data
        let allocateBlockBuffers count (parameters:BlockParameters) =
            Inputs.Buffers.allocateAcquisitionBuffers
                <| count
                <| (parameters.PreTriggerSamples + parameters.PostTriggerSamples)
                <| parameters.Acquisition.Inputs

        let setDataBuffersByChannel segment bufferIndex (acq:AcquisitionCommon) = async {
            for inputSampling in acq.Parameters.Inputs.InputSampling do
                do! PicoScope.DataBuffers.setDataBuffer
                     acq.PicoScope
                     inputSampling.InputChannel
                     inputSampling.DownsamplingMode
                     segment
                     <| Inputs.Buffers.findBuffers bufferIndex inputSampling acq.DataBuffers }

        /// Prepares the device for acquisition by setting up the channel settings, triggering,
        /// device resolution and data buffers.
        let prepareDevice acquisition = async {
            let acq = common acquisition
            acq.StatusChanged.Trigger (Next PreparingAcquisition)
            do! PicoScope.ChannelSettings.setAcquisitionInputChannels acq.PicoScope acq.Parameters.Inputs }
            // let toAdc = Trigger.Streaming.thresholdToAdc acq.Parameters
            // do! PicoScope.Triggering.setTrigger acq.PicoScope toAdc acq.Parameters.Trigger }

        /// Check on the consistency and correctness of acquisition parameters
        let private checkDownsampling parameters =
            if parameters.Inputs |> Inputs.hasDownsampling && parameters.DownsamplingRatio = None then
                invalidArg "Downsampling ratio" "Failed to create acquisition: specified inputs have downsampling but no downsampling ratio is specified."

            if not (parameters.Inputs |> Inputs.hasDownsampling) && parameters.DownsamplingRatio <> None then
                invalidArg "Downsampling ratio" "Failed to create acquisition: specified inputs have no downsampling but a downsampling ratio is specified."

        let createAcquisition picoScope parameters buffers =
            checkDownsampling parameters
            { Parameters      = parameters
              PicoScope       = picoScope
              DataBuffers     = buffers
              SamplesObserved = new Event<AcquiredSamples>()
              StopCapability  = new CancellationCapability<StopStatus>()
              StatusChanged   = new NotificationEvent<AcquisitionStatus>() }

        let markFinishedAutomatically acq =
            if not acq.StopCapability.IsCancellationRequested then
                acq.StopCapability.Cancel
                    <| { StoppedAutomatically = true; Failed = None }
                acq.StatusChanged.Trigger (Next <| FinishedAcquisition acq.StopCapability.Options.StoppedAutomatically)

        /// Creates a sample block for the given ValuesReady callback parameters with blank arrays
        /// which have to be filled by copying values from the streaming buffers.
        let private createSampleBlock (acq:AcquisitionCommon) valuesReady =
            let sampleCount = valuesReady.NumberOfSamples
            let capture = valuesReady.Capture
            { Samples = channels acq
                        |> Seq.ofList
                        |> Seq.map (fun sampling -> (sampling, Array.zeroCreate sampleCount))
                        |> Map.ofSeq
              Capture          = capture
              Length           = valuesReady.NumberOfSamples 
              VoltageOverflows = valuesReady.VoltageOverflows }
        
        /// Copies values from the acquisition buffer into the given sample block for the given
        /// StreamingValuesReady callback parameters. Copying is parallelised.
        let private copySampleBlockValues (acq:AcquisitionCommon) bufferIndex valuesReady sampleBlock =
            acq.DataBuffers.[bufferIndex]
            |> Map.toSeq
            |> Seq.map (fun (inputSampling, buffer) -> async {
                let sampleArray = Map.find inputSampling sampleBlock.Samples
                let startIndex  = valuesReady.StartIndex
                let sampleCount = valuesReady.NumberOfSamples
                Array.Copy(buffer, int startIndex, sampleArray, 0, int sampleCount) })
            |> Async.Parallel
            |> Async.Ignore

        let copySamples (acq:AcquisitionCommon) bufferIndex valuesReady = async {
            let sampleBlock = createSampleBlock acq valuesReady
            do! copySampleBlockValues acq bufferIndex valuesReady sampleBlock
            return sampleBlock }
