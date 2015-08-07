namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Hashing

module Control =
    [<AutoOpen>]
    module Store =
        open ARB.Translate
        open Experiment.Translate

        /// How many Segments to write in one chunk when writing a whole sequence of them.
        let private segmentsPerChunk = 4
        /// How many Seqeunces to write in one chunk when writing a whole sequence of them.
        let private sequencesPerChunk = 32

        /// Concatenate two error strings into one string.
        let private addErrorStrings str1 str2 = str1 + " AND ALSO " + str2
        /// Generic function to parallelise the the storage of a sequence of storables.
        let private parallelizeStorageFold errorState errorFold store instrument sequence =
            sequence
            |> Seq.map (store instrument)
            |> AsyncChoice.Parallel errorState errorFold
        /// Parallelise the storage of a sequence of storables, concatenating any errors which may
        /// occur.
        let private parallelizeStorage store instrument sequence =
            parallelizeStorageFold "" addErrorStrings store instrument sequence

        /// Write several storage commands concatenated into one semi-colon separated SCPI command.
        /// The sequence must be a sequence of identifiers and the data to be written.
        let private storeConcatenatedById store
                                          (construct : 'Id -> 'StoredType)
                                          (instrument : RfSource)
                                          commands = asyncChoice {
            do! store commands instrument
            let sequence = Seq.map (fun (_, _, el) -> el) commands
            return Seq.map (fun (id, _) -> construct id) sequence |> Array.ofSeq }

        /// Zip a set of valueMaps taking ids and sequences of (id * data), along with a single key, into
        /// one sequence of the 3-tuple zipped up.
        let private zipIdCommands valueMapBuilder key sequence =
            let valueMaps = Seq.map valueMapBuilder sequence
            let keys = seq {for _ in 1 .. Seq.length sequence -> key }
            Seq.zip3 valueMaps keys sequence

        /// Create an internal stored segment representation.
        let private toStoredSegment id = StoredSegment id
        /// Create and internal stored sequence representation.
        let private toStoredSequence id = StoredSequence id

        /// Command to write file to volatile memory.
        /// Command reference p.133, p.151. p.133 is technically for the ":MEM:DATA" command rather
        /// than the ":MMEM:DATA" command, but they are identical, and the former has more
        /// information.
        let private storeDataKey = ":MMEM:DATA"
        /// Store the three files associated with any segment in the machine's volatile memory,
        /// using the filename given as the id. Returns a StoredSegment type representing the
        /// data stored.
        let internal storeSegmentById instrument (id, segment) =
            let encoded = toEncodedSegmentFiles segment id
            let commands = seq {
                    yield (waveformDataString, storeDataKey, encoded)
                    yield (markersDataString, storeDataKey, encoded)
                    yield (headerDataString, storeDataKey, encoded) }
            asyncChoice {
                do! IO.setValueBytesSequence commands instrument
                return toStoredSegment id }

        /// Store the given sequence on the machine, and return a StoredSegment type which can be
        /// used to identify the segment internally.
        let storeSegment instrument segment =
            let id = SegmentId <| hexHash segmentToBytes segment
            storeSegmentById instrument (id, segment)

        /// Store a sequence of segments and their ids into the volatile memory of the machine.
        /// Returns an array of StoredSegments.
        let internal storeSegmentSequenceById instrument sequence =
            let encoded = Seq.map (fun (id, seg) -> (id, toEncodedSegmentFiles seg id)) sequence
            let builder map (_, _) = fun (_, el) -> map el
            let zipper map = zipIdCommands (builder map) storeDataKey encoded
            let interleave one two three = seq { yield one; yield two; yield three }

            let waveformCommands = zipper waveformDataString
            let markersCommands  = zipper markersDataString
            let headerCommands   = zipper headerDataString

            let commands =
                Seq.map3 interleave waveformCommands markersCommands headerCommands
                |> Seq.concat
                |> Seq.chunkBySize segmentsPerChunk
            let store = storeConcatenatedById IO.setValueBytesSequence toStoredSegment
            parallelizeStorage store instrument commands
            |> AsyncChoice.map Array.concat

        /// Store a sequence of segments onto the machine, and return an array of identifiers to those
        /// segments.
        let storeSegmentSequence instrument sequence =
            let helper el = ((SegmentId <| hexHash segmentToBytes el), el)
            let sequence' = Seq.map helper sequence
            storeSegmentSequenceById instrument sequence'

        /// Key to store sequences to the machine.
        /// Command reference p.345.
        let private storeSequenceKey = ":RAD:ARB:SEQ"
        /// Write a sequence file to the machine and returns the stored sequence type.
        let internal storeSequenceById instrument (id, sequence) = asyncChoice {
            do! IO.setValueBytes (sequenceDataString id) storeSequenceKey instrument sequence
            return toStoredSequence id }
        /// Write a sequence to the machine, and return an identifier for that sequence.
        let storeSequence instrument sequence =
            let id = SequenceId <| hexHash sequenceToBytes sequence
            storeSequenceById instrument (id, sequence)

        /// Write a sequence of sequences files to the machine, and return an array of the stored
        /// sequence type.
        let internal storeSequenceSequenceById instrument (sequence : seq<SequenceId * Sequence>) =
            let builder (_, _) = fun (id, el) -> sequenceDataString id el
            let commands =
                sequence
                |> zipIdCommands builder storeSequenceKey
                |> Seq.chunkBySize sequencesPerChunk
            let store = storeConcatenatedById IO.setValueBytesSequence toStoredSequence
            parallelizeStorage store instrument commands
            |> AsyncChoice.map Array.concat
        /// Write a sequence of sequences files to the machine, and return an array of identifiers
        /// to those sequences.
        let storeSequenceSequence instrument sequence =
            let sequence' = seq {
                for el in sequence do
                    let id = SequenceId <| hexHash sequenceToBytes el
                    yield (id, el) }
            storeSequenceSequenceById instrument sequence'

        /// Store an experiment onto the machine as a set of necessary sequences and samples.
        let storeExperiment instrument experiment = asyncChoice {
            let! encoded = toEncodedExperiment experiment
            let! storedSegments = storeSegmentSequenceById instrument encoded.Segments
            let! storedSequences =
                encoded.Sequences
                |> Seq.map (fun (id, sqn) -> (id, toRegularSequence sqn))
                |> storeSequenceSequenceById instrument
            let! storedExperiments =
                encoded.Experiments
                |> Seq.map (fun (id, exp) -> (id, toRegularSequence exp))
                |> storeSequenceSequenceById instrument
            return {
                StoredExperiments= storedExperiments
                StoredSegments   = storedSegments
                StoredSequences  = storedSequences
                ShotsPerPoint = encoded.Metadata.ShotsPerPoint
                Triggering = encoded.Metadata.TriggerSource } }

    [<AutoOpen>]
    module Delete =
        /// Command to delete all waveform, markers and header files stored in the BBG memory
        /// of the machine (the usual place that they're stored in).
        /// Command reference p.152.
        let private deleteAllSegmentsKey = ":MMEM:DEL:WFM"
        /// Delete all the waveform, markers and header files stored in the BBG memory of the
        /// machine (the usual storage location).
        let deleteAllStoredSegments = IO.writeKey deleteAllSegmentsKey

        /// Command to delete all sequence files stored in the internal memory of the machine
        /// (the usual storage location).
        /// Command reference p.146. Uses ":MEM" rather than ":MMEM" for some reason.
        let private deleteAllSequencesKey = ":MEM:DEL:SEQ"
        /// Delete all the sequence files stored in the internal memory of the machine (the usual
        /// storage location).
        let deleteAllStoredSequences = IO.writeKey deleteAllSequencesKey

        /// Command to delete any file on the machine by name.  If a waveform file is passed,
        /// any associated markers and header files are deleted alongside it.
        /// Command reference p.152.
        let private deleteFileKey = ":MMEM:DEL:NAME"
        /// Delete a segment from the machine's BBG data storage.  Includes deleting the waveform
        /// file, the markers file and the headers file (if present).
        let deleteStoredSegment = IO.setValueString storedSegmentFilename deleteFileKey
        /// Delete a sequence from the machine's internal data storage (the usual storage
        /// location).
        let deleteStoredSequence = IO.setValueString storedSequenceFilename deleteFileKey

#if DEBUG
    [<AutoOpen>]
    module Print =
        open Experiment.Translate

        /// In debug mode, compiled the experiment, then print it out, rather than writing to the machine.
        let printCompiledExperiment experiment = asyncChoice {
            let! compiled = toCompiledExperiment experiment
            printCompiledExperiment compiled }

        /// In debug mode, compress the experiment, then print it out, rather than writing to the machine.
        let printCompressedExperiment experiment = asyncChoice {
            let! compressed = toCompressedExperiment experiment
            printCompressedExperiment compressed }

        /// Print out the sequence files used by each experiment in order.
        let printExperimentSequences stored =
            stored.StoredExperiments
            |> Array.map extractStoredSequenceId
            |> Array.iter (printfn "%s")
#endif

    [<AutoOpen>]
    module Play =
        /// Key to select a segment or a sequence from the machine
        /// Command reference p.355.
        let private selectArbFileKey = ":RAD:ARB:WAV"
        /// Select a segment file on the machine.
        let selectSegment = IO.setValueString storedSegmentFilename selectArbFileKey
        /// Select a sequence file on the machine.
        let selectSequence = IO.setValueString storedSequenceFilename selectArbFileKey

        /// Key related to the state of the dual ARB player on the machine. Needs the output
        /// state to also be on before it will start to play.
        /// Command reference p.356.
        let private arbStateKey = ":RAD:ARB:STAT"
        /// Key related to the the modulation state of the RF channels.
        /// Command reference p.157.
        let private modulationStateKey = ":OUTP:MOD:STAT"
        /// Key for the overall RF output state. Must be On if anything is to play
        /// Command reference p.157.
        let private outputStateKey = ":OUTP:STAT"

        /// Set the state of the ARB generator of the given instrument. Can either be On
        /// or Off.
        let private setArbState value instrument = asyncChoice {
            do! IO.setOnOffState arbStateKey instrument value
            do! IO.setOnOffState modulationStateKey instrument value
            do! IO.setOnOffState outputStateKey instrument value }

        /// Turn on the ARB generator of the instrument.
        let turnOnArb = setArbState On
        /// Turn off the ARB generator of the instrument.
        let turnOffArb = setArbState Off

        /// Begin playing a segment stored on the instrument.
        let playStoredSegment instrument segment = asyncChoice {
            do! selectSegment instrument segment
            do! turnOnArb instrument }

        /// Begin playing a sequence stored on the instrument.
        let playStoredSequence instrument sequence = asyncChoice {
            do! selectSequence instrument sequence
            do! turnOnArb instrument }

        /// Store a segment file on to the machine, then begin playing it back as soon as possible.
        let playSegment instrument segment = asyncChoice {
            let! stored = storeSegment instrument segment
            do! playStoredSegment instrument stored
            return stored }

        /// Store a sequence file on to the machine. then begin playing it back as soon as possible.
        let playSequence instrument sequence = asyncChoice {
            let! stored = storeSequence instrument sequence
            do! playStoredSequence instrument stored
            return stored }