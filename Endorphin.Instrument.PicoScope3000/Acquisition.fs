namespace Endorphin.Instrument.PicoScope3000

open Endorphin.Core
open System
open FSharp.Control.Reactive
open System.Reactive.Concurrency

module Acquisition =

    module private Streaming =
        /// Creates a streaming acquisition on the given PicoScope 3000 series device with the specified
        /// streaming parameters.
        let create picoScope (parameters:StreamingParameters) =
            let buffers = Common.allocateStreamingBuffer parameters.Acquisition
            let acquisition = Common.createAcquisition picoScope parameters.Acquisition buffers
            { Streaming.Common = acquisition
              Streaming.Parameters  = parameters }

        /// Initiates the streaming acquisition.
        let startAcquiring (acquisition:StreamingAcquisition) = async {
            let! sampleInterval = PicoScope.Acquisition.startStreaming acquisition.Common.PicoScope acquisition.Parameters
            acquisition.Common.StatusChanged.Trigger (Next <| Acquiring sampleInterval) }

        /// Handles a callback with the given StreamingValues ready parameters.
        let handleStreamingValuesReady acq bufferAgent (valuesReady:StreamingValuesReady) =
            // check if the acquisition has been stopped manually
            if not acq.StopCapability.IsCancellationRequested then
                if valuesReady.DidAutoStop then // or automatically
                    { StoppedAutomatically = true ; Failed = None }
                    |> acq.StopCapability.Cancel

            if valuesReady.ValuesReady.NumberOfSamples <> 0 then

                Async.StartWithContinuations(
                    BufferAgent.handleSamplesAvailable bufferAgent valuesReady.ValuesReady,
                    ignore,
                    (fun exn -> 
                        { StoppedAutomatically = false ; Failed = Some exn }
                        |> acq.StopCapability.Cancel ),
                    ignore)

        /// Polls the device continuously until the acquisition is stopped manually or automatically.
        let rec pollUntilFinished acq bufferAgent = async {
            if not acq.StopCapability.IsCancellationRequested then
                do! PicoScope.Acquisition.pollStreamingLatestValues 
                        acq.PicoScope (handleStreamingValuesReady acq bufferAgent)
                    |> Async.Ignore
                
                do! Async.Sleep 5 // poll at 5 ms intervals
                do! pollUntilFinished acq bufferAgent
            else 
                match acq.StopCapability.Options.Failed with
                | Some exn -> raise exn
                | None     -> return () }


    module private Block =

        /// Allocate buffers for transferring captured samples in block mode
        let private allocateBlockBuffers (parameters:BlockParameters) =
            match parameters.Buffering with
            | Streaming ->
                Common.allocateStreamingBuffer parameters.Acquisition
            | SingleCapture
            | MultipleCapture _
            | AllCaptures ->
                Common.allocateBlockBuffers 1u parameters

        let create picoScope (parameters:BlockParameters) =
            let buffers = allocateBlockBuffers parameters
            let acquisition = Common.createAcquisition picoScope parameters.Acquisition buffers
            { BlockAcquisition.Common = acquisition
              Parameters  = parameters }

        let startAcquiring (acquisition:BlockAcquisition) = async {
            let acq = acquisition.Common
            let! timebase = PicoScope.Sampling.findTimebaseForSampleInterval acq.PicoScope MemorySegment.zero acq.Parameters.SampleInterval
            acquisition.Common.StatusChanged.Trigger (Next <| Acquiring timebase.SampleInterval)
            do! PicoScope.Acquisition.runBlock acquisition.Common.PicoScope acquisition timebase.Timebase }

            // async workflow calls RunBlock with a callback to here as an Async<unit> if status ok
            // Run GetValues in an async workflow, then handleValuesReady on the output
            // That will copy data out of the pinned buffer into sampleblocks and trigger emission
            // TODO handle emission of a sequence of sample blocks

         // push the samples to acquisition observable
        let handleSamplesReady (acq:AcquisitionCommon) bufferIndex valuesReady = async {
            let! samples = Common.copySamples acq bufferIndex valuesReady
            acq.SamplesObserved.Trigger samples }

        let sampleCount (acquisition:BlockAcquisition) =
            uint32 <| acquisition.Parameters.PostTriggerSamples + acquisition.Parameters.PreTriggerSamples

        let chunk size n =
            let edges = match n % size with
                        | 0u -> [0u .. size .. n]
                        | _  -> List.append [0u .. size .. n] [n]
            edges
            |> List.pairwise
            |> List.map (fun (a,b) -> (uint32 a,uint32 <| b-a))

        let fetchStreamingSequence segment (acquisition:BlockAcquisition) =
            let numberOfSamples = sampleCount acquisition
            let chunkSize = uint32 <| acquisition.Common.Parameters.BufferLength
            let picoscope = acquisition.Common.PicoScope
            async {
                for (start,length) in chunk chunkSize numberOfSamples do
                    let! valuesReady = PicoScope.Acquisition.getValues picoscope acquisition segment start length
                    do! valuesReady |> handleSamplesReady acquisition.Common 0 }

        let fetchBlock segment (acquisition:BlockAcquisition) =
            let numberOfSamples = sampleCount acquisition
            let picoscope = acquisition.Common.PicoScope
            async {
                let! valuesReady = PicoScope.Acquisition.getValues picoscope acquisition segment 0u numberOfSamples
                do! valuesReady |> handleSamplesReady acquisition.Common 0}

        let fetchBlocks (acquisition:BlockAcquisition) =
            match acquisition.Parameters.Buffering with
            | Streaming ->
                fetchStreamingSequence MemorySegment.zero acquisition
            | SingleCapture
            | MultipleCapture _
            | AllCaptures ->
                fetchBlock MemorySegment.zero acquisition

    module private RapidBlock = 

        /// Allocate buffers for transferring captured samples in rapid block mode
        let private allocateRapidBlockBuffers n (parameters:BlockParameters) =
            match parameters.Buffering with
            | Streaming ->
                Common.allocateStreamingBuffer parameters.Acquisition
            | SingleCapture ->
                Common.allocateBlockBuffers 1u parameters
            | MultipleCapture captures ->
                if captures < n then
                    Common.allocateBlockBuffers captures parameters
                else
                    Common.allocateBlockBuffers n parameters
            | AllCaptures ->
                Common.allocateBlockBuffers n parameters

        let create picoScope count (parameters:BlockParameters) =
            let buffers = allocateRapidBlockBuffers count parameters
            let acquisition = Common.createAcquisition picoScope parameters.Acquisition buffers
            { RapidBlockAcquisition.Acquisition =
                { BlockAcquisition.Common = acquisition
                  BlockAcquisition.Parameters  = parameters }
              RapidBlockAcquisition.Count = count }

        let startAcquiring (acquisition:RapidBlockAcquisition) =
            Block.startAcquiring acquisition.Acquisition

        let noOfMemorySegments = function
        | StreamingAcquisition _ 
        | BlockAcquisition _                -> 1u:NumberOfCaptures
        | RapidBlockAcquisition acquisition -> acquisition.Count

        let memorySegments acquisition =
            [(0u:MemorySegment) .. (noOfMemorySegments acquisition - 1u)]
        
        let notifyCaptureComplete capture (acquisition:BlockAcquisition) =
            acquisition.Common.StatusChanged.Trigger <| (Next <| FinishedCapture capture)

        let fetchStreamingSequence count (acquisition:BlockAcquisition) =
            async {
                for capture in 0u .. count-1u do
                    do! Common.setDataBuffersByChannel capture 0 acquisition.Common
                    do! Block.fetchStreamingSequence capture acquisition
                    notifyCaptureComplete capture acquisition }

        let fetchMultipleBlockSequence count chunkSize acquisition =
            let samples = Block.sampleCount acquisition
            let picoscope = acquisition.Common.PicoScope
            async {
                for (start,length) in Block.chunk chunkSize count do
                    let finish = start + length - 1u
                    for capture in start .. finish do
                        let index = int capture - int start
                        do! Common.setDataBuffersByChannel capture (int capture - int start) acquisition.Common
                    let! valuesReady = PicoScope.Acquisition.getValuesBulk picoscope acquisition start finish (uint32 samples)
                    for values in valuesReady do
                        do! Block.handleSamplesReady acquisition.Common (int values.Capture - int start) values
                        notifyCaptureComplete values.Capture acquisition }

        let fetchBlocks (acquisition:RapidBlockAcquisition) =
            match acquisition.Acquisition.Parameters.Buffering with
            | Streaming ->
                fetchStreamingSequence acquisition.Count acquisition.Acquisition
            | SingleCapture ->
                fetchMultipleBlockSequence acquisition.Count 1u acquisition.Acquisition
            | MultipleCapture count ->
                fetchMultipleBlockSequence acquisition.Count count acquisition.Acquisition
            | AllCaptures ->
                fetchMultipleBlockSequence acquisition.Count acquisition.Count acquisition.Acquisition


    [<AutoOpen>]
    module private Workflow =

        let prepareDevice acquisition = async {
            do! Common.prepareDevice acquisition
            match acquisition with 
            | StreamingAcquisition _
            | BlockAcquisition _      -> do! Common.setDataBuffersByChannel MemorySegment.zero 0 (common acquisition)
            | RapidBlockAcquisition acquisition' -> 
                let picoscope = (common acquisition).PicoScope
                let! samplesPerSegment = PicoScope.Sampling.segmentMemory picoscope acquisition'.Count
                let samplesRequired = int <| Block.sampleCount acquisition'.Acquisition
                if samplesPerSegment < samplesRequired then
                    failwithf "Too many samples requested per capture. Requested %d but each segment can only take %d" samplesRequired samplesPerSegment
                do! PicoScope.Acquisition.setNumberOfCaptures picoscope acquisition'.Count }

        /// Given a prepared Acquisition, allocate buffers, set up device and prime for triggered acquisition
        let startAcquiring = function
            | StreamingAcquisition acquisition ->
                Streaming.startAcquiring acquisition
            | BlockAcquisition acquisition ->
                Block.startAcquiring acquisition
            | RapidBlockAcquisition acquisition ->
                RapidBlock.startAcquiring acquisition

        let internal handleData = function
        | StreamingAcquisition acquisition -> async {
            let bufferAgent = BufferAgent.create acquisition.Common
            do! Streaming.pollUntilFinished acquisition.Common bufferAgent
            do! BufferAgent.waitToFinishProcessing bufferAgent }
        | BlockAcquisition acquisition -> async {
            do! Block.fetchBlocks acquisition
            Common.markFinishedAutomatically acquisition.Common }
        | RapidBlockAcquisition acquisition ->
            RapidBlock.fetchBlocks acquisition


    /// Represents a handle to a streaming acquisition which has been started on a PicoScope 3000 series
    /// device. This can be used to stop the acquisition manually and/or wait for it to finish.
    /// Returned by start functions
    type AcquisitionHandle =
        internal { Acquisition  : Acquisition 
                   WaitToFinish : Async<AcquisitionResult> }

    /// Runs an acquisition with the given cancellation token. If the token is cancelled, then acquisition
    /// will be aborted. To ensure that the acquisition is stopped cleanly, no other cancellation 
    /// compensations should be registered with this cancellation token which will post commands to the
    /// PicoScope. Returns a handle which can be used to stop the acquisition manually or wait for it to
    /// finish asynchronously.
    let startWithCancellationToken (acquisition:Acquisition) cancellationToken =
        if (common acquisition).StopCapability.IsCancellationRequested then
            failwith "Cannot start an acquisition which has already been stopped."
            
        // create a result channel which will be used to register the acquisition result and await
        // it asynchronously
        let resultChannel = new ResultChannel<_>()

        // if the acquisition finishes normally, stop it and register the result
        let finishAcquisition acquisitionResult =
            let acq = common acquisition
            Async.StartWithContinuations(
                PicoScope.Acquisition.stop acq.PicoScope,
                (fun () ->
                    acq.StatusChanged.Trigger Completed
                    resultChannel.RegisterResult AcquisitionCompleted),
                (fun stopExn ->
                    acq.StatusChanged.Trigger (Error stopExn)
                    resultChannel.RegisterResult (AcquisitionError stopExn)),
                ignore)

        // if an error occurs stop the acquisition and register result result
        let stopAcquisitionAfterError exn =
            let acq = common acquisition
            Async.StartWithContinuations(
                PicoScope.Acquisition.stop acq.PicoScope,
                (fun () ->
                    acq.StatusChanged.Trigger (Error exn)
                    resultChannel.RegisterResult (AcquisitionError exn)),
                (fun stopExn ->
                    acq.StatusChanged.Trigger (Error stopExn)
                    resultChannel.RegisterResult (AcquisitionError stopExn)),
                ignore)
  
        // if the acquisition is cancelled, stop it and register the result
        let stopAcquisitionAfterCancellation _ =
            let acq = common acquisition
            Async.StartWithContinuations(
                PicoScope.Acquisition.stop acq.PicoScope,
                (fun () ->
                    acq.StatusChanged.Trigger (Next <| CancelledAcquisition)
                    acq.StatusChanged.Trigger Completed
                    resultChannel.RegisterResult AcquisitionCancelled),
                (fun stopExn ->
                    acq.StatusChanged.Trigger (Error stopExn)
                    resultChannel.RegisterResult (AcquisitionError stopExn)),
                ignore)


        // define the acquisition workflow
        let acquisitionWorkflow = async {
            let acq = common acquisition
            use __ = Inputs.Buffers.createPinningHandle acq.DataBuffers
            do! prepareDevice acquisition
            do! startAcquiring acquisition
            do! handleData acquisition }

            
        // start it with the continuations defined above and the given cancellation token
        Async.StartWithContinuations(
                acquisitionWorkflow,
                finishAcquisition,
                stopAcquisitionAfterError, 
                stopAcquisitionAfterCancellation,
                cancellationToken)

        // and return an acquisition handle which can be used to await the result
        { Acquisition = acquisition ; WaitToFinish = resultChannel.AwaitResult () }
  
  
  
    /// Supplies notifications of changes in the acquisition status
    let status acquisition =
        (common acquisition).StatusChanged.Publish
        |> Observable.fromNotificationEvent
        |> Observable.observeOn (new EventLoopScheduler())

    /// Prepare to run an acquisition using the supplied parameters
    let prepare picoscope = function
    | StreamingParameters parameters -> StreamingAcquisition <| Streaming.create picoscope parameters
    | BlockParameters parameters     -> BlockAcquisition <| Block.create picoscope parameters
    | RapidBlockParameters (count,parameters)     -> RapidBlockAcquisition <| RapidBlock.create picoscope count parameters
        
    /// Starts a streaming acquisition. If the ability to abort the acquisition via a cancellation
    /// token is required, then start the acquisition using startWithCancellationToken instead. Returns
    /// a handle which can be used to stop the acquisition manually or wait for it to finish
    /// asynchronously.
    let start acquisition = startWithCancellationToken acquisition Async.DefaultCancellationToken

    /// Starts a streaming acquisition as a child (sharing the same cancellation token) to the current
    /// asynchronous workflow. To ensure that the acquisition is stopped cleanly, no other cancellation 
    /// compensations should be registered with this cancellation token which will post commands to the
    /// PicoScope. Returns a handle which can be used to stop the acquisition manually or wait for it to
    /// finish asynchronously.
    let startAsChild acquisition = async {
        let! ct = Async.CancellationToken
        return startWithCancellationToken acquisition ct }

    /// Asynchronously waits for the acquisition associated with the given handle to finish and returns
    /// the result.
    let waitToFinish acquisitionHandle = acquisitionHandle.WaitToFinish
        
    /// Manually stops the streaming acquisition associated with the given handle.
    let stop (acquisitionHandle:AcquisitionHandle) =
        let acq = common acquisitionHandle.Acquisition
        if not acq.StopCapability.IsCancellationRequested then
            { StoppedAutomatically = false ; Failed = None } |> acq.StopCapability.Cancel 

    /// Manually stops the streaming acquisition associated with the given handle and asynchronously
    /// waits for it to finish.
    let stopAndFinish (acquisitionHandle:AcquisitionHandle) =
        stop acquisitionHandle
        waitToFinish acquisitionHandle

