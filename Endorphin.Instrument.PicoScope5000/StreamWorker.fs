namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Endorphin.Core.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open log4net
open System.Reactive.Linq

/// Specifies the input settings and downsamplingfi modes to be used for a channel during a streaming acquisition.
type ChannelStream = 
    { /// Specifies the input settings for the channel, such as voltage input range and input coupling.
      InputSettings : InputSettings
      /// Specifies the collection of downsampling modes to be used to sample a channel during the acquisition. Note
      /// that set should either contain DownsamplingMode.None or a non-empty collection of other DownsamplingModes.
      /// Otherwise the StreamWorker will raise an exception upon instantiation.
      DownsamplingModes : Set<DownsamplingMode> }

/// Describes a buffer during an acquisition by its channel and respctive BufferDownsampling.
type ChannelBuffer =
    { /// The channel from which the data in the buffer is sampled.
      Channel : Channel
      /// The BufferDownsampling by which the data in the buffer is sampled.
      BufferDownsampling : BufferDownsampling }

    with
    
    override channelBuffer.ToString() =
        let bufferString = 
            match channelBuffer.BufferDownsampling with
            | NoDownsampling -> "No downsampling"
            | AggregateMax -> "Aggregate maximum"
            | AggregateMin -> "Aggregate minimum"
            | Averaged -> "Averaged"
            | Decimated -> "Decimated"
        sprintf "Channel %A, %s" channelBuffer.Channel bufferString

/// Specifies the parameters for a PicoScope 5000 streaming acquistion to be executed by a StreamWorker.
type PicoScope5000Stream =
    { /// The requested sample interval in nanoseconds. Note that the actual hardware sample interval may differ so
      /// that it is an integer multiple of the device clock rate. The actuahl value is supplied by the
      /// StreamWorker.StatusChanged event when the StreamStatus.Streaming value occurs.
      SampleInterval : int<ns> 
      /// Specifies the optional downsampling ratio to be used. Specify None for no downsampling and insure that
      /// only active ChannelStreams with DownsamplingMode.None are specified. Conversely you must give a value
      /// if ChannelStreams specify other DownsamplingModes.
      DownsamplingRatio : uint32 option
      /// Specifies whether the stream stops automatically after given number of samples is acquired or manually.
      StreamStop : StreamStop
      /// Specifies the trigger event required to start the streaming acquisition.
      TriggerSettings : TriggerSettings
      /// Specifies all channels which will be sampled during the acquisition with their respective ChannelStreams.
      ActiveChannels : Map<Channel, ChannelStream>
      /// Specifies the PicoScope device memory segment to which the streaming acquisition should write data.
      MemorySegment : uint32 }
    
    with
    
    /// Creates an instance of uint32 option with a value indicating no downsampling. Interoperability helper.
    static member NoDownsampling : uint32 option = None

    /// Creates an instance of uint32 option with a value indicating the specified downsampling ratio. Interoperability
    /// helper.
    static member DownsamplingWithRatio (ratio : uint32) = Some ratio

/// Indicates the status of a StreamWorker.
type StreamStatus =
    /// Indicates the the StreamWorker has begun setting up the PicoScope device for the acquisition.
    | PreparingStream
    /// Indicates that the StreamWorker is ready to begin streaming, once the SetReadyToStart method is called.
    | ReadyToStream
    /// Indicates that the streaming acquisition has started with the provided hardware sample interval. Note that
    /// this may differ from the requested sample interval so that it is an integer multiple of the device clock rate.
    | Streaming of hardwareSampleInterval : int<ns>
    /// Indicates that the streaming acquisition has finished, specifying whether it stopped manually or automatically.
    | FinishedStream of didAutoStop : bool
    /// Indicates that the streaming acquisition was cancelled before starting.
    | CanceledStream of exn : OperationCanceledException
    /// Indicates that the streaming acquisition failed due to an error.
    | FailedStream of exn : exn

/// Specifies whether a stream was stopped manually or automatically.
type StreamStopOptions =
    { DidAutoStop : bool }

/// Contains a block of samples acquired from the PicoScope device data buffers during a streaming acquisition.
type SampleBlock =
    { /// A collection containing a sample array for each channel and respective BufferDownsampling.
      Samples : Map<ChannelBuffer, int16 array>
      /// The number of samples in each sample array.
      Length : int
      /// The set of channels for which a voltage overflow occured during the sample block.
      VoltageOverflows : Set<Channel> }

/// Executes a streaming acquisition workflow on a PicoScope 5000 series device according to the given stream
/// parameters.
type StreamWorker(pico : PicoScope5000, stream) =
    static let log = LogManager.GetLogger typeof<StreamWorker> // logger

    /// events
    let statusChanged = new Event<StreamStatus>()
    let sampleBlockObserved = new Event<SampleBlock>()
    
    // capture the current synchronisation context so that events can be fired on the UI thread or thread pool accordingly
    let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

    // handle which is used to indicate that the stream should start once it is prepared
    let readyToStart = new ManualResetHandle(false)
    // cancellation capability which is used to cancel the stream before it begins
    let cancellationCapability = new CancellationCapability()
    // cancellation capability which is used to stop polling the PicoScope driver for the latest acquired values,
    // cancelled either if the autoStop flag is set during a callback or if the Stop method is called manually
    let stopCapability = new CancellationCapability<StreamStopOptions>()

    // the downsampling ratio which will be sent to the PicoScope driver
    let downsamplingRatio =
        match stream.DownsamplingRatio with
        | Some value -> value
        | None -> 1u // 1 in the case of no downsampling

    // compute a suitable buffer lenght for each data buffer according to the stream acquisition parameters
    // TODO: optimise this calculation to reduce buffer size as much as possible
    let bufferLength =
        // set the minimum buffer length to correspond to 256 seconds worth of samples
        // this is probably excessive - TODO: do stress testing to see if it can be reduced
        let minLength = 256.0 * 1e9 / (float (int stream.SampleInterval * int downsamplingRatio))
        // compute the buffer length so that it is a power of two and at least as large as the minimum length
        // chosen
        let rec computeBufferLength length = 
            if (float) length >= minLength
            then length
            else computeBufferLength (2 * length)

        computeBufferLength 1
    
    // determine the set of channels which will be active during the streaming acquisition
    let activeChannelSet =
        seq { for channel in stream.ActiveChannels -> channel.Key }
        |> Set.ofSeq

    // determine the set of active downsampling ratio modes in the streaming acquistion by combining those which
    // are in use with a logical OR operation
    let downsamplingModes =
        seq { for channel in stream.ActiveChannels -> channel.Value }
        |> Seq.collect (fun channelStream -> Set.toSeq channelStream.DownsamplingModes)
        |> Seq.fold (|||) DownsamplingMode.None // combine the required types of downsampling using logical OR
    
    // define the streaming parameters to be sent to the PicoScope driver
    let streamingParameters = 
        { SampleInterval = stream.SampleInterval
          StreamStop = stream.StreamStop
          DownsamplingRatio = downsamplingRatio
          DownsamplingModes = downsamplingModes
          BufferLength = uint32 bufferLength }

    do // sanity checks
        if stream.ActiveChannels.Count = 0 then // if no channels are active
            invalidArg "stream.activeChannels" // raise an exception
                "Set of active channels must be non-empty." stream.ActiveChannels

        // if a DownsamplingMode is specified by not a downsampling ratio
        if stream.DownsamplingRatio.IsNone && downsamplingModes <> DownsamplingMode.None then 
            invalidArg "stream.downsampling" // raise an exception
                "A downsampling ratio must be specified for a stream where an active channel uses downsampling." stream.DownsamplingRatio

        // if a downsampling ratio is specified but the downsampling mode is DownsamplingMode.None
        if stream.DownsamplingRatio.IsSome && downsamplingModes = DownsamplingMode.None then
            invalidArg "stream.downsampling" // raise an exctionep
                "A downsampling ratio was specified but none of the active channels use downsampling." stream.DownsamplingRatio

        // for all active channels
        stream.ActiveChannels
        |> Map.iter (fun channel channelStream ->
            if channelStream.DownsamplingModes.IsEmpty then // if the set of downsampling modes is empty
                invalidArg "stream.activeChannels" // raise an exception
                    "List downsampling modes for active channel must be non-empty." (channel, channelStream)
            
            // if the set contains DownsamplingMode.None and the computed downsampling mode for the stream is not DownsamplingMode.None
            if downsamplingModes <> DownsamplingMode.None && channelStream.DownsamplingModes.Contains DownsamplingMode.None then
                invalidArg "stream.activeChannels" // raise an exception
                    "DownsamplingMode.None cannot be used in a stream which uses another form of downsampling." (channel, channelStream))

    /// The stream parameters specified for the acquisition.
    member __.Stream = stream
    
    /// Event fires when the StreamWorker status changes. Events are fired on the System.Threading.SynchronizationContext which
    /// instantiates the worker.
    member __.StatusChanged =
        Observable.CreateFromEvent(
            statusChanged.Publish,
            // fire OnCompleted when the stream finishes
            (fun status -> 
                match status with
                | FinishedStream _ -> true
                | _ -> false), 
            // fire OnError when the stream fails or is canceled
            (fun status ->
                match status with
                | FailedStream exn -> Some exn
                | CanceledStream exn -> Some (exn :> exn)
                | _ -> None))
            .ObserveOn(syncContext)

    /// Event fires when a new block of samples is read from the PicoScope data buffer. Events are fired on the
    /// System.Threading.SynchronizationContext which instantiated the worker.
    member streamWorker.SampleBlockObserved = 
        sampleBlockObserved.Publish
            .TakeUntil(streamWorker.StatusChanged.LastAsync())
            .ObserveOn(syncContext)

    /// Event fires for every slice of samples observed during the acquisition. A sample slice contains a sample for every active channel
    /// and respective BufferDownsampling. Events are fired on the System.Threading.SynchronizationContext which instantiated the worker.
    member streamWorker.SampleSliceObserved =
       (sampleBlockObserved.Publish // for every sample block observed
        |> Event.collectSeq (fun block -> seq { // fire once for each slice
            for index in 0 .. block.Length - 1 ->
                block.Samples 
                |> Map.map (fun _ buffer -> buffer.[index]) }))
            .TakeUntil(streamWorker.StatusChanged.LastAsync())
            .ObserveOn(syncContext)
    
    /// Event fires for each sample observed on the specified channel for the particular BufferDownsampling. Events are fired on the
    /// System.Threading.SynchronizationContext which instantiated the worker.
    member streamWorker.SampleObserved channelBuffer =
       (sampleBlockObserved.Publish // for every sample block observed
        |> Event.collectSeq (fun block -> // fire once for every sample
            block.Samples.[channelBuffer] // for the specified channel and BufferDownsampling.
            |> Array.toSeq))
            .TakeUntil(streamWorker.StatusChanged.LastAsync())
            .ObserveOn(syncContext)

    /// Event fires if a voltage overflow occurs on the specified channel. Events are fired on the System.Threading.SynchronizationContext
    /// which instantiated the worker.
    member streamWorker.VoltageOverflow channel =
       (sampleBlockObserved.Publish // for every sample block observed
        |> Event.choose (fun block -> // fire if a voltage overflow occured on the specified channel
            if block.VoltageOverflows.Contains channel then Some () else None))
            .TakeUntil(streamWorker.StatusChanged.LastAsync())
            .ObserveOn(syncContext)

    /// Stops the streaming acquisition manually.
    member __.Stop() =
        // don't stop the stream manually if it is already stopping automatically
        if not stopCapability.IsCancellationRequested then
            "Stream worker stopping manually." |> log.Info
            cancellationCapability.Cancel()
            stopCapability.Cancel { DidAutoStop = false }
            readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    /// Initiates the streaming acquisition workflow and sets the ready-to-start flag.
    member worker.PrepareAndStart() =
        worker.SetReadyToStart()
        worker.Prepare()

    /// Sets the ready-to-start flag indiciating that the streaming acquisition should start immediately once the device is ready.
    member __.SetReadyToStart() =
        "Setting stream worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    /// Initiates the streaming acquisition workflow. The SetReadyToStart method will need to be called before the acquisition begins.
    member __.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare stream as cancellation was already requested."

        "Stream worker preparing." |> log.Info
        sprintf "Active channels: %A." stream.ActiveChannels |> log.Info
        sprintf "Sample interval: %d ns." (int stream.SampleInterval) |> log.Info
        sprintf "Trigger settings: %A." stream.TriggerSettings |> log.Info
        sprintf "Stream stop: %A." stream.StreamStop |> log.Info
        
        // define the streaming acquisition workflow
        let streamWorkflow buffers = async {

            // this callback will be sent to the PicoScope and called by the driver when it has written new data to the buffer
            let dataCallback (streamingValues : StreamingValuesReady) =
                // if the acquisition has already been set to stop then don't process the written samples
                if not stopCapability.IsCancellationRequested then
                    // if the auto-stop flag is set on the data then stop polling the driver
                    if streamingValues.DidAutoStop then
                        stopCapability.Cancel { DidAutoStop = true }

                    // if the number of samples sent by the driver is non-zero (can be zero if this is to just indicate that the
                    // stream has stopped automatically)
                    if streamingValues.NumberOfSamples <> 0 then
                        // then define a workflow which will read the samples from the buffer into a new array
                        let readSamples = 
                            buffers // take the acquisition buffers
                            |> Map.toSeq
                            |> Seq.map (fun (channelBuffer, buffer) -> async { // and map each to an asynchronous workflow
                                let samples = Array.zeroCreate streamingValues.NumberOfSamples // create an empty array
                                // then copy the specified range of data from the buffer into the array and return the result
                                Array.Copy(buffer, int streamingValues.StartIndex, samples, 0, streamingValues.NumberOfSamples)
                                return (channelBuffer, samples) })
                            |> Async.Parallel // compose the workflows in parallel

                        // define a workflow which will read the samples and raise the sampleBlockObserved event
                        async {
                            sprintf "Received stream data block of %d samples" streamingValues.NumberOfSamples |> log.Info
                            sprintf "Stream did %s." (if streamingValues.DidAutoStop then "auto-stop" else "not auto-stop") |> log.Info
                            let! samples = readSamples

                            // raise the event on the synchronisation context (thread pool or UI thread)
                            sampleBlockObserved.Trigger
                                { Samples = samples |> Map.ofSeq
                                  Length = streamingValues.NumberOfSamples
                                  VoltageOverflows = streamingValues.VoltageOverflows }}
                        |> Async.Start // start the workflow on the threadpool to avoid slowing down the driver thread
  
            // define a workflow which will poll the PicoScope for the latest stream values until acquisition has stopped
            let rec pollLoop (acquisition : IStreamingAcquisition) = async {
                "Polling PicoScope for stream values..." |> log.Info
                do! acquisition.GetLatestValues dataCallback // poll the streaming acquisition and wait for the request to complete
                do! Async.Sleep 100 // wait 100 ms before polling again

                // if the streaming acquisition hasn't been stopped then continue polling
                if not stopCapability.IsCancellationRequested then
                    do! pollLoop acquisition }

            "Initiating streaming..." |> log.Info
            use! acquisition = pico.RunStreamingAsync streamingParameters // start the streaming acquisition
               
            sprintf "Started streaming with sammple interval: %A ns." acquisition.SampleInterval |> log.Info
            // raise the statusChanged event to indicate that streaming has started
            statusChanged.Trigger (Streaming acquisition.SampleInterval)

            "Starting poll loop." |> log.Info
            do! pollLoop acquisition // poll the PicoScope for data until acquisition stops
            
            let didAutoStop = stopCapability.Options.DidAutoStop
            sprintf "Stream finished successfully %s auto-stop." (if didAutoStop then "with" else "without") |> log.Info
            // raise the statusChanged event to indicated that streaming has finished
            statusChanged.Trigger (FinishedStream didAutoStop) }

        // define a workflow which will set up the device for the acquisition and start the acquisition workflow
        let startupWorkflow = async {

            // define a workflow which will set up the device channels according to the specified streaming parameters
            let setupChannels = async {
                "Setting up stream channels." |> log.Info
                pico.SetTrigger stream.TriggerSettings // set up triggering

                // check that the requested channels are available on the device
                let! availableChannels = pico.GetAvailableChannelsAsync()
                if not (activeChannelSet.IsSubsetOf availableChannels) then // raise an exception if not
                    failwith "Some input channels required by the stream are not available on the PicoScope."
                
                // for each device channel
                for channel in availableChannels do
                    match channel with
                    | channel when stream.ActiveChannels.ContainsKey channel ->
                        // enable it with the specified settings if it is going to be used in the stream
                        let inputSettings = stream.ActiveChannels.[channel].InputSettings
                        pico.SetChannelSettings(channel, Enabled inputSettings)
                    | channel ->
                        // disable it otherwise
                        pico.SetChannelSettings(channel, Disabled) }
            
            // define a workflow which will create the acquisition buffer arrays and set the driver to use them
            let createBuffers = async {
                "Creating stream buffers." |> log.Info
                return stream.ActiveChannels // for every active channel
                |> Map.toSeq
                |> Seq.collect (fun (channel, channelStream) -> seq {
                    for downsampling in channelStream.DownsamplingModes do
                        match downsampling.BufferFormat with 
                        
                        | Single bufferDownsampling -> // if the buffer format requires a single buffer
                            // create the array
                            let buffer = Array.zeroCreate bufferLength
                            let channelBuffer = { Channel = channel; BufferDownsampling = bufferDownsampling }
                            
                            // set it to the driver and add it to the sequence of buffers
                            pico.SetDataBuffer(channel, buffer, stream.MemorySegment, downsampling)
                            yield (channelBuffer, buffer) 

                        | Pair (bufferDownsamplingMax, bufferDownsamplingMin) -> // if the buffer format requires a pair
                            // create the arrays
                            let bufferMax = Array.zeroCreate bufferLength
                            let bufferMin = Array.zeroCreate bufferLength
                            let channelBuferMax = { Channel = channel; BufferDownsampling = bufferDownsamplingMax }
                            let channelBuferMin = { Channel = channel; BufferDownsampling = bufferDownsamplingMin }

                            // set the pair of buffers to the deriver and add them to the sequence of buffers
                            pico.SetAggregateDataBuffers(channel, bufferMax, bufferMin, stream.MemorySegment)
                            yield (channelBuferMax, bufferMax)
                            yield (channelBuferMin, bufferMin) })
                |> Map.ofSeq }

            // define a workflow which will wait for the ready-to-start flag to be set
            let awaitReadyForStream = async {
                "Waiting for ready-to-stream signal..." |> log.Info
                // raise an event inidcating that the StreamWorker is waiting for the ready-to-stream signal
                statusChanged.Trigger ReadyToStream
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore }
            
            // use the defined workflows to perform the streaming acquisition    

            "Preparing for stream acquisition." |> log.Info
            // raise an event indicating that the StreamWorker has started preparing for the stream
            statusChanged.Trigger PreparingStream
            
            // if cancellation occurs, discard any data buffers which may have been set
            use! __ = Async.OnCancel (fun () -> 
                pico.DiscardDataBuffers())

            do! setupChannels // set up the device channels
            let! buffers = createBuffers // create and set up the data buffers
            do! awaitReadyForStream // wait for the ready-to-start flag to be set
            
            // then start the stream workflow on the thread pool with the specified continuations
            Async.StartWithContinuations(
                streamWorkflow buffers,
                (ignore), // statusChanged event fired elsewhere on finish
                (fun exn -> // error
                    stopCapability.Cancel { DidAutoStop = false }
                    log.Error (sprintf "Stream failed during acquisition acquisition due to error %A." exn, exn)
                    statusChanged.Trigger (FailedStream exn)),
                ignore) } // workflow manually checks the stopCapability so no cancellation token or cancellation continuation
        
        // start the startup workflow on the thread pool, which will subsequently start the acquisition workflow once the device is set up and the
        // ready-to-start flag is set
        "Starting stream workflow." |> log.Info
        Async.StartWithContinuations(
            startupWorkflow,
            (ignore), // no continuation unless there is an error or cancellation: this workflow starts the acquisition workflow at the end
            (fun exn -> // error
                log.Error ((sprintf "Stream failed before starting due to error %A." exn), exn)
                statusChanged.Trigger (FailedStream exn)),
            (fun exn -> // canceled event won't be raised unless the stream is stopped before it starts running
                "Stream canceled before starting." |> log.Info
                statusChanged.Trigger (CanceledStream (new OperationCanceledException("Stream canceled before starting")))), 
                cancellationCapability.Token)