namespace Endorphin.Instrument.PicoScope5000

open System
open Endorphin.Core
open System.Collections.Generic


/// Functions for concurrently processing data in the streaming buffer as soon as it becomes available.
module internal BufferAgent =

    /// Represents a message to the buffer agent, indicating that samples have become available for readout
    /// or that they have been processed by the agent.
    type private Message =
        | SamplesAvailable of valuesReady : ValuesReady * replyChannel: AsyncReplyChannel<exn option>
        | SamplesProcessed of index : int
        | AcquisitionFinished of replyChannel : AsyncReplyChannel<exn option>

    /// Acquisition data buffer agent which ensures that samples in the acquisition are processed in order
    /// and without buffer overflow.
    type BufferAgent = private BufferAgent of agent : Agent<Message>
        
    /// Advances the buffer position, ensuring that it happens continuously.
    let private advanceBufferPosition (acq:AcquisitionCommon) valuesReady (loop, index) =
        if valuesReady.StartIndex <> index then 
            Choice.fail (Exception "Acquisition buffer position advanced discontinuously.")
        else 
            let index' = index + uint32 valuesReady.NumberOfSamples
            let bufferLength = uint32 <| acq.Parameters.BufferLength
            Choice.succeed (loop + index' / bufferLength, 
                            index' % bufferLength)

    /// Checks for buffer overflow by ensuring that the write position has not advanced beyond the
    /// read position by a complete loop.
    let private checkOverflow readPosition writePosition = 
        let (loop : uint32, index : uint32) = readPosition
        let (loop', index') = writePosition
            
        if ((loop' = loop + 1u) && (index' >= index)) || (loop' > loop + 1u)
        then Choice.fail (Exception "Acquisition buffer overflow detected.")
        else Choice.succeed ()

    /// Handles a callback with the given StreamingValuesReady parameters and posts a message back
    /// to the buffer agent with the samples which were copied out of the buffer.
    let private processSamples acquisition valuesReady index (bufferAgent : Agent<_>) = async {
        let! samples = Common.copySamples acquisition 0 valuesReady 
        bufferAgent.Post (SamplesProcessed index) // sent a message to the agent
        return (valuesReady, samples) }

    /// Initialises a BufferAgent for the acquisition.
    let create (acq:AcquisitionCommon) = BufferAgent <| Agent.Start (fun mailbox ->
            
        // use a queue to ensure that data blocks are pushed to acquisition observable in order
        // queue items are a tuple of (index, async promise) where the index indicates the order
        // in which the blocks arrived
        let processing = new Queue<_>()
        let enqueue item    = processing.Enqueue item
        let dequeue ()      = processing.Dequeue ()
        let queueIsEmpty () = (processing.Count = 0)
        let nextIndex ()    = processing.Peek () |> fst

        // dequeue ready samples from the front of the queue, advancing the read position
        let rec dequeueReadyBlocks readPosition ready = async {
            if (not <| queueIsEmpty ()) && (ready |> Set.contains (nextIndex ())) then
                let (next, waitToProcess) = dequeue () // dequeue the promise
                let! (valuesReady, samples) = waitToProcess
                let readPosition' = readPosition |> Choice.bind (advanceBufferPosition acq valuesReady)
                let ready' = ready |> Set.remove next // remove the index from the ready set
                acq.SamplesObserved.Trigger samples // push the samples to acquisition observable
                return! dequeueReadyBlocks readPosition' ready'
            else return (readPosition, ready) }

        // process buffer agent messages while acquisition is on-going
        let rec acquiring readPosition writePosition ready writeIndex = async {
            let! message = mailbox.Receive()
            match message with
                
            | SamplesAvailable (valuesReady, replyChannel) ->
                // if new samples available, try to advance the write position and check for overflow
                let bufferAdvanceResult = choice {
                    let! readPosition' = readPosition
                    let! writePosition' = writePosition |> advanceBufferPosition acq valuesReady
                    do! checkOverflow readPosition' writePosition'
                    return writePosition' }
                        
                match bufferAdvanceResult with
                | Choice.Success writePosition' ->
                    let! waitToProcess = // copy the samples out of the buffer in the background
                        processSamples acq valuesReady writeIndex mailbox
                        |> Async.StartChild
                    enqueue (writeIndex, waitToProcess) // and add the promise to the queue
                    replyChannel.Reply None
                    return! acquiring readPosition writePosition' ready (writeIndex + 1)
                | Choice.Failure exn ->
                    replyChannel.Reply (Some exn)
                
            | SamplesProcessed readIndex ->
                // if new samples have been processed, add the block index to the ready set and
                // dequeue any blocks which are ready at the front of the queue
                let! (readPosition', ready') = Set.add readIndex ready |> dequeueReadyBlocks readPosition
                return! acquiring readPosition' writePosition ready' writeIndex

            | AcquisitionFinished replyChannel ->
                // go into the finishing state 
                return! finishing replyChannel readPosition writePosition ready }

        // process buffer agent messages while in the finishing state, where only SamplesProcessed
        // messages are expected; once all sample blocks are processed, the reply channel is notified
        and finishing replyChannel readPosition writePosition ready = async {
                
            // if the read position has successfully advanced to the write position, the acquisition
            // has finished
            match readPosition with
            | Choice.Success p when p = writePosition -> replyChannel.Reply None
            | Choice.Failure exn                      -> replyChannel.Reply (Some exn)
            | _ -> // otherwise keep processing messages
                let! message = mailbox.Receive()
                match message with
                | SamplesProcessed readIndex ->
                    let! (readPosition', ready') = Set.add readIndex ready |> dequeueReadyBlocks readPosition
                    return! finishing replyChannel readPosition' writePosition ready'
                
                | SamplesAvailable (_, replyChannel')
                | AcquisitionFinished replyChannel' ->
                    let error = Exception "Buffer agent received unexpected message after acquisition finished."
                    replyChannel.Reply  (Some error)
                    replyChannel'.Reply (Some error) }

        // initialise in the acquiring state
        acquiring (Choice.succeed (0u, 0u)) (0u, 0u) Set.empty 0)

    /// Notifies the buffer agent that samples are available to be copied out of the acquisition buffer
    /// asynchronously. Throws an exception if buffer overflow is detected or the buffer is advanced
    /// discontinuously.
    let handleSamplesAvailable (BufferAgent agent) valuesReady = async {
        let! result = 
            (fun replyChannel -> SamplesAvailable(valuesReady, replyChannel))
            |> agent.PostAndAsyncReply
            
        match result with
        | Some exn -> raise exn
        | None     -> return () }

    /// Waits for the buffer agent to finish processing samples which it has been notified about. Throws
    /// an exception if an error occurs during processing.
    let waitToFinishProcessing (BufferAgent agent) = async {
        let! result = AcquisitionFinished |> agent.PostAndAsyncReply 
            
        match result with
        | Some exn -> raise exn
        | None     -> return () }
