namespace Endorphin.Instrument.Keysight

open Endorphin.Core
open FSharp.Data.UnitSystems.SI.UnitSymbols

module Control =
    [<AutoOpen>]
    module Store =
        open Experiment.Translate
        open Power.Control
        open IQ.Control
        open Sweep.Configure
        open Sweep.Control
        open Sweep.Control.List

        /// Key to select a segment or a sequence from the machine
        /// Command reference p.355.
        let private selectArbFileKey = ":RAD:ARB:WAV"
        /// Select a waveform on the machine.
        let selectWaveform = IO.setValueString storedWaveformFilename selectArbFileKey

        /// How many Segments to write in one chunk when writing a whole sequence of them.
        let private segmentsPerChunk = 4
        /// How many Seqeunces to write in one chunk when writing a whole sequence of them.
        let private sequencesPerChunk = 32

        /// Parallelise the storage of a sequence of storables, concatenating any errors which may
        /// occur.
        let private parallelizeStorage store instrument sequence =
            sequence
            |> Seq.map (store instrument)
            |> Async.Parallel

        /// Write several storage commands concatenated into one semi-colon separated SCPI command.
        /// The sequence must be a sequence of identifiers and the data to be written.
        let private storeConcatenatedById store
                                          (construct : 'Id -> 'StoredType)
                                          (instrument : RfSource)
                                          commands = async {
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
        let private toStoredSegment = SegmentId >> StoredWaveform
        /// Create an internal stored sequence representation.
        let private toStoredSequence = SequenceId >> StoredWaveform

        /// Set the header file of the given waveform to have the currently set ARB settings, except
        /// also with the passed 
        let private setHeaderArbClockFrequency instrument frequency waveform = async {
            do! selectWaveform instrument waveform
            do! ARB.setClock instrument frequency
            do! ARB.setHeaderFile instrument }

        /// Command to write file to volatile memory.
        /// Command reference p.133, p.151. p.133 is technically for the ":MEM:DATA" command rather
        /// than the ":MMEM:DATA" command, but they are identical, and the former has more
        /// information.
        let private storeDataKey = ":MMEM:DATA"
        /// Store the three files associated with any segment in the machine's volatile memory,
        /// using the filename given as the id. Returns a StoredSegment type representing the
        /// data stored.
        let internal storeSegment instrument (id, segment) =
            let encoded = ARB.Encode.segment id segment
            let commands = seq {
                    yield (ARB.Encode.waveformDataString, storeDataKey, encoded)
                    yield (ARB.Encode.markersDataString, storeDataKey, encoded) }
            async {
                do! IO.setValueBytesSequence commands instrument
                let stored = toStoredSegment id
                do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency stored
                return stored }

        /// Query a waveform file of a segment for its datablock.
        let private queryWaveformFile instrument id =
            IO.queryFileBytes ARB.Decode.parseWaveformFile storeDataKey instrument (waveformFolder, id)

        /// Query a marker file of a segment for its datablock.
        let private queryMarkerFile instrument id =
            IO.queryFileBytes ARB.Decode.parseMarkerFile storeDataKey instrument (markerFolder, id)

        /// Query a pair of segment files (waveform and markers) for their segment.
        let private querySegment instrument id = async {
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
        let internal storeSegmentSequence instrument sequence = async {
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
            let store = storeConcatenatedById IO.setValueBytesSequence toStoredSegment
            let! waveforms =
                parallelizeStorage store instrument commands
                |> Async.map Array.concat
            for waveform in waveforms do
                do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency waveform
            return waveforms }

        /// Key to store sequences to the machine.
        /// Command reference p.345.
        let private storeSequenceKey = ":RAD:ARB:SEQ"
        /// Write a sequence file to the machine and returns the stored sequence type.
        let internal storeSequence instrument (id, sequence) = async {
            do! IO.setValueString (ARB.Encode.sequence id) storeSequenceKey instrument sequence
            let stored = toStoredSequence id
            do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency stored
            return stored }

        /// Write a sequence of sequences files to the machine, and return an array of the stored
        /// sequence type.
        let internal storeSequenceSequence instrument (sequence : seq<string * Sequence>) = async {
            let builder (_, _) = fun (id, el) -> ARB.Encode.sequence id el
            let commands =
                sequence
                |> zipIdCommands builder storeSequenceKey
                |> Seq.chunkBySize sequencesPerChunk
            let store = storeConcatenatedById IO.setValueStringSequence toStoredSequence
            let! sequences =
                parallelizeStorage store instrument commands
                |> Async.map Array.concat
            for sequence in sequences do
                do! setHeaderArbClockFrequency instrument ARB.defaultClockFrequency sequence
            return sequences }

        /// Store a waveform onto the machine.
        let storeWaveform instrument = function
            | Segment (id, data)  -> storeSegment instrument (id, data)
            | Sequence (id, data) -> storeSequence instrument (id, data)

        /// Store a sequence of waveforms onto the machine.
        let storeWaveformSequence instrument (sequence : Waveform seq) = async {
            let arr = Array.create (Seq.length sequence) Unchecked.defaultof<StoredWaveform>
            let mutable i = 0
            for element in sequence do
                let! head = storeWaveform instrument element
                arr.[i] <- head
                i <- i + 1
            return arr }

        /// Store an experiment onto the machine as a set of necessary sequences and samples.
        /// Turns off the ARB before writing, because the machine doesn't seem to like doing both
        /// at once.  This is just stored, it isn't ready to play yet.
        let storeExperiment instrument experiment = async {
            do! ARB.turnOff instrument
            let encoded = toEncodedExperiment experiment
            let! storedSegments = storeSegmentSequence instrument encoded.Segments
            let! storedSequences = encoded.Sequences |> storeSequenceSequence instrument
            let! storedPoints = encoded.Points |> storeSequenceSequence instrument
            let! storedExperiment = encoded.Experiment |> storeSequence instrument
            return {
                StoredExperiment = storedExperiment
                StoredPoints     = storedPoints
                StoredWaveforms  = Array.append storedSegments storedSequences
                RfBlankRoute     = encoded.Metadata.RfBlankMarker
                Frequencies      = encoded.Metadata.Frequencies
                Power            = encoded.Metadata.Power } }

        /// Steps needed to set up the sweep for use.
        let private setUpSweep instrument trigger (stored : StoredExperiment) = async {
            let options =
                defaultSweepOptions
                |> optionsWithStepTrigger None
                |> optionsWithListTrigger (Some trigger)
                |> optionsWithDwellTime None
                |> optionsWithRetrace Off
            do! setSweepOptions instrument options
            do! setSweepType instrument List
            let waveforms = Seq.replicate (Seq.length stored.Frequencies) stored.StoredExperiment
            do! setListWaveformSequence instrument waveforms
            do! setFrequencies instrument stored.Frequencies
            do! setPowers instrument [ stored.Power ] }

        /// Steps needed to turn the outputs on.
        let private setUpOutputs instrument stored = async {
            // ensure the RF blank marker is set, and is the correct polarity
            do! Routing.setRfBlankRoute instrument stored.RfBlankRoute
            do! Routing.setMarkerPolarity stored.RfBlankRoute instrument Positive
            // turn on various output modes
            do! setAlcState instrument Off
            do! setIqModulation instrument On
            do! ARB.turnOn instrument }

        /// Load a previously stored experiment into the list sweep file, overwriting whatever's
        /// already there.
        let loadStoredExperiment instrument trigger stored = async {
            do! setUpSweep instrument trigger stored
            do! setUpOutputs instrument stored }

        /// Store an experiment on the machine, then immediately load it into the list sweep file,
        /// ready to start playback.
        let storeAndPrimeExperiment instrument trigger experiment = async {
            let! stored = storeExperiment instrument experiment
            do! loadStoredExperiment instrument trigger stored
            return stored }

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
        /// Delete a waveform from the machine's data storage.  Includes deleting the waveform
        /// file, the markers file and the headers file (if present).
        let deleteStoredWaveform = IO.setValueString storedWaveformFilename deleteFileKey

        /// Delete all stored waveforms on the machine.
        let deleteAllStoredWaveforms instrument = async {
            do! deleteAllStoredSegments instrument
            do! deleteAllStoredSequences instrument }

    [<AutoOpen>]
    module Play =
        /// Begin playing a waveform stored on the instrument.
        let playStoredWaveform instrument stored = async {
            do! selectWaveform instrument stored
            do! ARB.turnOn instrument }

        /// Store a segment file on to the machine, then begin playing it back as soon as possible.
        let playWaveform instrument waveform = async {
            let! stored = storeWaveform instrument waveform
            do! playStoredWaveform instrument stored
            return stored }

#if DEBUG
    [<AutoOpen>]
    module Print =
        open Experiment.Translate

        /// In debug mode, compiled the experiment, then print it out, rather than writing to the machine.
        let printCompiledExperiment experiment =
            let compiled = toCompiledExperiment experiment
            printCompiledExperiment compiled

        /// In debug mode, compress the experiment, then print it out, rather than writing to the machine.
        let printCompressedExperiment experiment =
            let compressed = toCompressedExperiment experiment
            printCompressedExperiment compressed

        /// Print out the sequence files used by each experiment in order.
        let printExperimentSequences stored =
            stored.StoredExperiment
            |> extractStoredWaveformId
            |> printfn "%s"
#endif