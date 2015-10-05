namespace Endorphin.Instrument.Keysight

open Endorphin.Core
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Experiment.Translate
open IQ.Control

/// Functions for controlling the writing of experiments using the dual ARB system on the machine.
module Control =
    /// Parallelise the storage of a sequence of storable values by an Async.Parallel workflow.
    let private parallelizeStorage store instrument sequence =
        sequence
        |> Seq.map (store instrument)
        |> Async.Parallel

    /// Write several storage commands concatenated into one semi-colon separated SCPI command.
    /// The sequence must be a sequence of identifiers and the data to be written.
    let private storeConcatenatedById store
                                      (construct : 'Id -> 'StoredType)
                                      (instrument : RfSource)
                                      (commands : seq<'ValueMapper * 'Key * ('Id * 'Storable)>) = async {
        do! store commands instrument
        let sequence = Seq.map (fun (_, _, el) -> el) commands
        return Seq.map (fun (id, _) -> construct id) sequence |> Array.ofSeq }

    /// Zip a set of valueMaps taking ids and sequences of (id * data), along with a single key, into
    /// one sequence of the 3-tuple zipped up.
    let private zipIdCommands valueMapBuilder key sequence =
        let valueMaps = Seq.map valueMapBuilder sequence
        let keys = seq {for _ in 1 .. Seq.length sequence -> key }
        Seq.zip3 valueMaps keys sequence

    /// Command to write file to volatile memory.
    /// Command reference p.133, p.151. p.133 is technically for the ":MEM:DATA" command rather
    /// than the ":MMEM:DATA" command, but they are identical, and the former has more
    /// information.
    let private storeDataKey = ":MMEM:DATA"

    /// Functions for controlling the storage, querying and playback of waveforms onto the machine.
    module Waveform =
        /// Key to select a segment or a sequence from the machine
        /// Command reference p.355.
        let private selectArbFileKey = ":RAD:ARB:WAV"
        /// Select a waveform on the machine.
        let select = IO.setValueString storedWaveformFilename selectArbFileKey

        /// Set the header file of the given waveform to have the currently set ARB settings, except
        /// also with the passed 
        let private setHeaderArbClockFrequency instrument frequency waveform = async {
            do! select instrument waveform
            do! ARB.setClock instrument frequency
            do! ARB.setHeaderFile instrument }

        /// Functions for controlling waveforms which are explicitly known to be segments rather than the
        /// generic type.  This is where the waveform abstraction fails for speed or space reasons!
        module Segment =
            /// Create an internal stored segment representation.
            let private idToStoredWaveform = SegmentId >> StoredWaveform

            /// How many Segments to write in one chunk when writing a whole sequence of them.
            let private segmentsPerChunk = 4

            /// Store the three files associated with any segment in the machine's volatile memory,
            /// using the filename given as the id. Returns a StoredSegment type representing the
            /// data stored.
            let internal store instrument (id, segment) =
                let encoded = ARB.Encode.segment id segment
                let commands = seq {
                        yield (ARB.Encode.waveformDataString, storeDataKey, encoded)
                        yield (ARB.Encode.markersDataString, storeDataKey, encoded) }
                async {
                    do! IO.setValueBytesSequence commands instrument
                    let stored = idToStoredWaveform id
                    do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency stored
                    return stored }

            /// Query a waveform file of a segment for its datablock.
            let private queryWaveformFile instrument id =
                IO.queryFileBytes ARB.Decode.parseWaveformFile storeDataKey instrument (waveformFolder, id)

            /// Query a marker file of a segment for its datablock.
            let private queryMarkerFile instrument id =
                IO.queryFileBytes ARB.Decode.parseMarkerFile storeDataKey instrument (markerFolder, id)

            /// Query a pair of segment files (waveform and markers) for their segment.
            let internal query instrument id = async {
                let! (i, q) = queryWaveformFile instrument id
                let! markers = queryMarkerFile instrument id
                let samples = Array.map3 ARB.Decode.parseSample i q markers
                let incrementSampleCount (SampleCount i) = SampleCount (i + 1u)
                let rec loop acc lastId accI = function
                    | i when i = Array.length samples ->
                        Segment (id, { SegmentSamples = acc; SegmentLength = uint16 <| Array.length samples })
                    | i ->
                        let curId = Sample.hash samples.[i]
                        if curId = lastId then
                            acc.[accI] <- (fst acc.[accI], incrementSampleCount <| snd acc.[accI])
                            loop acc lastId accI (i + 1)
                        else
                            let acc' = Array.append acc [| (samples.[i], SampleCount 1u) |]
                            loop acc' curId (accI + 1) (i + 1)
                return loop [||] "" 0 0 }

            /// Store a sequence of segments and their ids into the volatile memory of the machine.
            /// Returns an array of StoredSegments.
            let internal storeSeq instrument sequence = async {
                let encoded = Seq.map (fun (id, seg) -> (id, ARB.Encode.segment id seg)) sequence
                let builder map (_, _) = fun (_, el) -> map el
                let zipper map = zipIdCommands (builder map) storeDataKey encoded
                let interleave one two = seq { yield one; yield two }
                let waveformCommands = zipper ARB.Encode.waveformDataString
                let markersCommands  = zipper ARB.Encode.markersDataString
                let commands =
                    Seq.map2 interleave waveformCommands markersCommands
                    |> Seq.concat
                    |> Seq.chunkBySize segmentsPerChunk
                let store = storeConcatenatedById IO.setValueBytesSequence idToStoredWaveform
                let! waveforms =
                    parallelizeStorage store instrument commands
                    |> Async.map Array.concat
                for waveform in waveforms do
                    do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency waveform
                return waveforms }

            /// Command to delete all waveform, markers and header files stored in the BBG memory
            /// of the machine (the usual place that they're stored in).
            /// Command reference p.152.
            let private deleteAllSegmentsKey = ":MMEM:DEL:WFM"
            /// Delete all the waveform, markers and header files stored in the BBG memory of the
            /// machine (the usual storage location).
            let deleteAll = IO.writeKey deleteAllSegmentsKey

        /// Functions for controlling waveforms which are explicitly known to be sequences rather than the
        /// generic type.  This is where the waveform abstraction fails for speed or space reasons!
        module Sequence =
            /// Create an internal stored sequence representation.
            let private idToStoredWaveform = SequenceId >> StoredWaveform

            /// How many Seqeunces to write in one chunk when writing a whole sequence of them.
            let private sequencesPerChunk = 32

            /// Key to store sequences to the machine.
            /// Command reference p.345.
            let private storeSequenceKey = ":RAD:ARB:SEQ"
            /// Write a sequence file to the machine and returns the stored sequence type.
            let internal store instrument (id, sequence) = async {
                do! IO.setValueString (ARB.Encode.sequence id) storeSequenceKey instrument sequence
                let stored = idToStoredWaveform id
                do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency stored
                return stored }

            /// Write a sequence of sequences files to the machine, and return an array of the stored
            /// sequence type.
            let internal storeSeq instrument (sequence : seq<string * Sequence>) = async {
                let builder (_, _) = fun (id, el) -> ARB.Encode.sequence id el
                let commands =
                    sequence
                    |> zipIdCommands builder storeSequenceKey
                    |> Seq.chunkBySize sequencesPerChunk
                let store = storeConcatenatedById IO.setValueStringSequence idToStoredWaveform
                let! sequences =
                    parallelizeStorage store instrument commands
                    |> Async.map Array.concat
                for sequence in sequences do
                    do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency sequence
                return sequences }

            /// Command to delete all sequence files stored in the internal memory of the machine
            /// (the usual storage location).
            /// Command reference p.146. Uses ":MEM" rather than ":MMEM" for some reason.
            let private deleteAllSequencesKey = ":MEM:DEL:SEQ"
            /// Delete all the sequence files stored in the internal memory of the machine (the usual
            /// storage location).
            let deleteAll = IO.writeKey deleteAllSequencesKey

        // module Waveform = (continued base level)

        /// Store a waveform onto the machine, but don't begin playing it yet.
        let store instrument = function
            | Segment (id, data)  -> Segment.store  instrument (id, data)
            | Sequence (id, data) -> Sequence.store instrument (id, data)

        /// Store a sequence of waveforms onto the machine.
        let storeSeq instrument (sequence : Waveform seq) = async {
            let arr = Array.zeroCreate<StoredWaveform> (Seq.length sequence)
            let mutable i = 0
            for element in sequence do
                let! head = store instrument element
                arr.[i] <- head
                i <- i + 1
            return arr }

        /// Command to delete any file on the machine by name.  If a waveform file is passed,
        /// any associated markers and header files are deleted alongside it.
        /// Command reference p.152.
        let private deleteFileKey = ":MMEM:DEL:NAME"
        /// Delete a waveform from the machine's data storage.  Includes deleting the waveform
        /// file, the markers file and the headers file (if present).
        let delete = IO.setValueString storedWaveformFilename deleteFileKey
        /// Delete all stored waveforms on the machine.
        let deleteAll instrument = async {
            do! Segment.deleteAll  instrument
            do! Sequence.deleteAll instrument }

        /// Begin playing a waveform stored on the instrument.
        let playStored instrument stored = async {
            do! select instrument stored
            do! ARB.turnOn instrument }

        /// Store a segment file on to the machine, then begin playing it back as soon as possible.
        let play instrument waveform = async {
            let! stored = store instrument waveform
            do! playStored instrument stored
            return stored }

    /// Functions for controlling the writing, reading and playing of experiments on the machine.
    module Experiment =
        /// Store an experiment onto the machine as a set of necessary sequences and samples.
        /// Turns off the ARB before writing, because the machine doesn't seem to like doing both
        /// at once.  This is just stored, not played yet.
        let store instrument experiment = async {
            do! ARB.turnOff instrument
            let encoded = toEncodedExperiment experiment
            let! storedSegments   = Waveform.Segment.storeSeq instrument encoded.Segments
            let! storedSequences  = encoded.Sequences  |> Waveform.Sequence.storeSeq instrument
            let! storedPoints     = encoded.Points     |> Waveform.Sequence.storeSeq instrument
            let! storedExperiment = encoded.Experiment |> Waveform.Sequence.store    instrument
            return {
                StoredExperiment = storedExperiment
                StoredPoints     = storedPoints
                StoredWaveforms  = Array.append storedSegments storedSequences
                RfBlankRoute     = encoded.Metadata.RfBlankMarker } }

        /// Play a previously stored experiment through the dual ARB system.
        let playStored instrument experiment = Waveform.playStored instrument experiment.StoredExperiment

        /// Store an experiment, and then immediately begin playback on the dual ARB system.
        let play instrument experiment = async {
            let! stored = store instrument experiment
            do! playStored instrument stored }

#if DEBUG
    module Print =
        open Experiment.Translate

        /// In debug mode, compiled the experiment, then print it out, rather than writing to the machine.
        let compiled experiment =
            let compiled = toCompiledExperiment experiment
            printCompiledExperiment compiled

        /// In debug mode, compress the experiment, then print it out, rather than writing to the machine.
        let compressed experiment =
            let compressed = toCompressedExperiment experiment
            printCompressedExperiment compressed

        /// Print out the sequence files used by each experiment in order.
        let experimentSequences stored =
            stored.StoredExperiment
            |> extractStoredWaveformId
            |> printfn "%s"

        /// Print out the filename of a stored experiment.
        let experimentFile stored =
            printfn "%s" (storedWaveformFilename stored.StoredExperiment)
#endif