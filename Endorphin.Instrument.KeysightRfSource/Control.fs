﻿namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Hashing

module Control =
    [<AutoOpen>]
    module Waveform =
        open Endorphin.Instrument.Keysight.Waveform.Translate

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
            asyncChoice {
                do! IO.setBytesValue waveformDataString storeDataKey instrument encoded
                do! IO.setBytesValue markersDataString storeDataKey instrument encoded
                do! IO.setBytesValue headerDataString storeDataKey instrument encoded
                return toStoredSegment id }

        /// Store the given sequence on the machine, and return a StoredSegment type which can be
        /// used to identify the segment internally.
        let storeSegment instrument segment =
            let id = SegmentId <| hexHash segmentToBytes segment
            storeSegmentById instrument (id, segment)

        /// Store a sequence of segments and their ids into the volatile memory of the machine.
        /// Returns an array of StoredSegments.
        let internal storeSegmentSequenceById instrument sequence =
            parallelizeStorage storeSegmentById instrument sequence

        /// Store a sequence of segments onto the machine, and return an array of identifiers to those
        /// segments.
        let storeSegmentSequence instrument sequence =
            let sequence' = seq {
                for el in sequence do
                    let id = SegmentId <| hexHash segmentToBytes el
                    yield (id, el) }
            storeSegmentSequenceById instrument sequence'

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
        let deleteStoredSegment = IO.setValue storedSegmentFilename deleteFileKey
        /// Delete a sequence from the machine's internal data storage (the usual storage
        /// location).
        let deleteStoredSequence = IO.setValue storedSequenceFilename deleteFileKey

        /// Key to store sequences to the machine.
        /// Command reference p.345.
        let private storeSequenceKey = ":RAD:ARB:SEQ"
        /// Write a sequence file to the machine and returns the stored sequence type.
        let internal storeSequenceById instrument (id, sequence) = asyncChoice {
            do! IO.setBytesValue (sequenceDataString id) storeSequenceKey instrument sequence
            return toStoredSequence id }
        /// Write a sequence to the machine, and return an identifier for that sequence.
        let storeSequence instrument sequence =
            let id = SequenceId <| hexHash sequenceToBytes sequence
            storeSequenceById instrument (id, sequence)

        /// Write a sequence of sequences files to the machine, and return an array of the stored
        /// sequence type.
        let internal storeSequenceSequenceById instrument sequence =
            parallelizeStorage storeSequenceById instrument sequence
        /// Write a sequence of sequences files to the machine, and return an array of identifiers
        /// to those sequences.
        let storeSequenceSequence instrument sequence =
            let sequence' = seq {
                for el in sequence do
                    let id = SequenceId <| hexHash sequenceToBytes el
                    yield (id, el) }
            storeSequenceSequenceById instrument sequence'

        /// Key to select a segment or a sequence from the machine
        /// Command reference p.355.
        let private selectArbFileKey = ":RAD:ARB:WAV"
        /// Select a segment file on the machine.
        let selectSegment = IO.setValue storedSegmentFilename selectArbFileKey
        /// Select a sequence file on the machine.
        let selectSequence = IO.setValue storedSequenceFilename selectArbFileKey

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


    /// Public functions for writing user-input experiments to the machine.
    [<AutoOpen>]
    module RfPulse =
        open Endorphin.Instrument.Keysight.RfPulse.Translate

        /// Store an experiment onto the machine as a set of necessary sequences and samples.
        let storeExperiment instrument experiment = asyncChoice {
            let! encoded = toEncodedExperiment experiment
            let! storedSegments = storeSegmentSequenceById instrument encoded.Segments
            let! storedSequences =
                encoded.Sequences
                |> Seq.map (fun (id, sqn) -> (id, toRegularSequence sqn))
                |> storeSequenceSequenceById instrument
            let! storedExperimentAsSequence =
                encoded.Experiment
                |> (fun (id, exp) -> (id, toRegularSequence exp))
                |> storeSequenceById instrument
            let storedExperiment = StoredExperimentId storedExperimentAsSequence
            return {
                StoredExperiment = storedExperiment
                StoredSegments   = storedSegments
                StoredSequences  = storedSequences } }

        /// Play a stored experiment on the machine.
        let playStoredExperiment instrument experiment = asyncChoice {
            let sequence = experiment |> experimentToStoredSequence
            do! playStoredSequence instrument sequence }

        /// Store an experiment on the machine, then begin playing it as soon as possible. Returns
        /// the stored experiment.
        let playExperiment instrument experiment = asyncChoice {
            let! stored = storeExperiment instrument experiment
            do! playStoredExperiment instrument stored
            return stored }