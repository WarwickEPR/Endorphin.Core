namespace Endorphin.Instrument.PicoScope3000

open Endorphin.Core
open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Parameters =

    module Acquisition =
        /// Creates streaming acquisition parameters with the specified resolution, sample interval and
        /// buffer length and default values for everything else. The default memory segment is 0. The 
        /// device is set to trigger automatically after 1 ms. Acquisition is set to stop manually and
        /// no downsampling is used. No inputs are enabled.
        let create sampleInterval bufferLength =
            { Inputs            = Inputs.none
              Trigger           = Trigger.auto 1s<ms>
              SampleInterval    = sampleInterval
              BufferLength      = bufferLength
              DownsamplingRatio = None }

        /// Returns modified streaming acquisition parameters with the given sample interval.
        let withSampleInterval sampleInterval (parameters : AcquisitionParameters) = { parameters with SampleInterval = sampleInterval }

        /// Returns modified streaming acquisition parameters with the specified acquisition buffer length.
        let withBufferLength bufferLength (parameters : AcquisitionParameters) = { parameters with BufferLength = bufferLength }

        /// Returns modified streaming acquisition parameters with the specified trigger settings.
        let withTrigger trigger (parameters : AcquisitionParameters) = { parameters with Trigger = trigger }

        /// Returns modified streaming acquisition parameters with the specified downsampling ratio.
        let withDownsamplingRatio downsamplingRatio (parameters : AcquisitionParameters) =
            if not (Inputs.hasDownsampling parameters.Inputs) then
                invalidArg "Downsampling ratio" "Cannot specifiy a downsampling ratio for an acquisition which has no downsampled inputs."
            { parameters with DownsamplingRatio = Some downsamplingRatio }

        /// Returns modified streaming acquisition parameters with the specified input channel enabled with the
        /// given input settings. Fails if the channel is already enabled.
        let enableChannel channel coupling range voltageOffset bandwidth (parameters : AcquisitionParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.enableChannel (Analogue channel) coupling range voltageOffset bandwidth }

        /// Returns modified acquisition parameters with the specified list of input channels enabled
        /// with the given input settings. Fails if any of the channels in the set is already enabled.
        let enableChannels channels coupling range voltageOffset bandwidth (parameters : AcquisitionParameters) =
            let analogueChannels = channels |> List.map Analogue
            { parameters with Inputs = parameters.Inputs |> Inputs.enableChannels analogueChannels coupling range voltageOffset bandwidth }

        let enableDigitalPort channels level (parameters : AcquisitionParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.enableDigitalChannels [channels] level }

        let enableDigitalPorts channels level (parameters : AcquisitionParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.enableDigitalChannels channels level }

        /// Returns modified acquisition parameters with the specified input channel sampled with
        /// the given downsampling mode. Fails if the channel is not enabled. Also fails if the acquisition
        /// has inputs which are sampled with no downsampling while another downsampling mode is given or
        /// vice-versa.
        let private sampleInputChannel channel downsamplingMode (parameters : AcquisitionParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.sampleChannel channel downsamplingMode }
        let sampleChannel channel downsamplingMode parameters =
            sampleInputChannel (Analogue channel) downsamplingMode parameters
        let sampleDigitalPort port downsamplingMode parameters =
            sampleInputChannel (Digital port) downsamplingMode parameters
        /// Returns modified streaming acquisition parameters with the specified list of input channels
        /// sampled with the given downsampling mode. Fails if the channel is not enabled. Also fails if the
        /// acquisition has inputs which are sampled with no downsampling while another downsampling mode is
        /// given or vice-versa.
        let private sampleInputChannels channels downsamplingMode (parameters : AcquisitionParameters) =
            { parameters with Inputs = parameters.Inputs |> Inputs.sampleChannels channels downsamplingMode }
        let sampleChannels channels downsamplingMode parameters =
            sampleInputChannels (channels |> List.map Analogue) downsamplingMode parameters



    module Streaming =
        /// Creates streaming acquisition parameters with the specified resolution, sample interval and
        /// buffer length and default values for everything else. The default memory segment is 0. The 
        /// device is set to trigger automatically after 1 ms. Acquisition is set to stop manually and
        /// no downsampling is used. No inputs are enabled.
        open Acquisition.Streaming
        let create acquisition = { Acquisition = acquisition
                                   StreamStop  = ManualStop }

        /// Returns modified streaming acquisition parameters with the given sample interval.
        let withAcquisition acquisition (parameters : StreamingParameters) =
            { parameters with Acquisition = acquisition }

        /// Returns modified streaming acquisition parameters specifying that the acquisition will not stop
        /// automatically after a fixed number of samples has been acquired but have to be stopped manually.
        let withManualStop parameters = { parameters with StreamStop = ManualStop }

        /// Returns modified streaming acquisition parameters specifying that the acquisition will stop
        /// automatically after the specified number of samples before and after the trigger has been
        /// captured.
        let withAutoStop maxPreTriggerSamples maxPostTriggerSamples parameters =
            { parameters with StreamStop = AutoStop(maxPreTriggerSamples, maxPostTriggerSamples) }

        let streamingCapture parameters =
            StreamingParameters parameters

    module Block =
        /// Creates streaming acquisition parameters with the specified resolution, sample interval and
        /// buffer length and default values for everything else. The default memory segment is 0. The 
        /// device is set to trigger automatically after 1 ms. Acquisition is set to stop manually and
        /// no downsampling is used. No inputs are enabled.
        open Acquisition.Block
        let create acquisition =
            { Acquisition        = acquisition
              PreTriggerSamples  = 0
              PostTriggerSamples = 0
              Buffering          = Streaming }
        
        let withPreTriggerSamples samples parameters =
            { parameters with PreTriggerSamples = samples }

        let withPostTriggerSamples samples parameters =
            { parameters with PostTriggerSamples = samples }

        let withBuffering buffering parameters =
            { parameters with Buffering = buffering }

        let blockCapture parameters =
            BlockParameters parameters

        let rapidBlockCapture count parameters =
            RapidBlockParameters (count,parameters)
