namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open IQData.Control
open CRC

module RfPulse =
    module internal Translate =
        [<AutoOpen>]
        module Encode =
            open IQData.Translate.Encode

            /// Create a string of hex values out of binary data
            let toHexString = sprintf "%016x"

            /// Create the name of an experiment to store in the machine and to use as an internal
            /// reference point.
            let makeExperimentName segments sequences =
                let bytesSegments =
                    segments
                    |> List.map getStoredSegmentASCIIString
                    |> List.fold Array.append [||]
                let bytesSequences =
                    sequences
                    |> List.map getStoredSequenceASCIIString
                    |> List.fold Array.append [||]
                bytesSequences
                |> Array.append bytesSegments
                |> crc64
                |> toHexString
                |> ExperimentId

            /// Cast an experiment ID into a sequence ID for storing in the machine
            let experimentToSequenceId (ExperimentId str) = SequenceId str

            /// Encode an experiment into a writeable form TODO!!
            let toEncodedExperiment (experiment : Experiment) =
                let segments = []
                let sequences = []
                let name = makeExperimentName segments sequences
                { Name = name
                  Segments = segments
                  Sequences = sequences
                  Experiment = { Name = experimentToSequenceId name; Sequence = [] } }

            let getExperimentId encoded =
                encoded.Experiment.Name
                |> extractSequenceId
                |> ExperimentId

    module Control =
        open Translate
        /// Store an experiment onto the machine as a set of necessary sequences and samples
        let storeExperiment instrument experiment =
            let encoded = toEncodedExperiment experiment
            asyncChoice {
                let! storedSegments =
                    encoded.Segments
                    |> Seq.ofList
                    |> storeSegmentSequence instrument
                let! storedSequences =
                    encoded.Sequences
                    |> Seq.ofList
                    |> storeSequenceSequence instrument
                return {
                    Id = getExperimentId encoded
                    Segments = storedSegments
                    Sequences = storedSequences } }