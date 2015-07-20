namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Waveform =
    /// Functions for configuring samples
    [<AutoOpen>]
    module Configure =
        /// A markers record with all markers turned off
        let noMarkers = { M1 = false; M2 = false; M3 = false; M4 = false }
        /// Basic data form of IQ point
        let defaultIQSample = {
            Sample.I = 0s;
            Sample.Q = 0s;
            Sample.Markers = noMarkers }
        /// Set value of the I sample
        let withI value sample = { sample with I = value }
        /// Set value of the Q sample
        let withQ value sample = { sample with Q = value }
        /// Set value of the first marker
        let private markersWithMarker1 value markers = { markers with M1 = value }
        /// Set value of the second marker
        let private markersWithMarker2 value markers = { markers with M2 = value }
        /// Set value of the third marker
        let private markersWithMarker3 value markers = { markers with M3 = value }
        /// Set value of the fourth marker
        let private markersWithMarker4 value markers = { markers with M4 = value }
        /// Set value of the first marker
        let withMarker1 value (sample : Sample) =
            { sample with Markers = markersWithMarker1 value sample.Markers }
        /// Set value of the second marker
        let withMarker2 value (sample : Sample) =
            { sample with Markers = markersWithMarker2 value sample.Markers }
        /// Set value of the third marker
        let withMarker3 value (sample : Sample) =
            { sample with Markers = markersWithMarker3 value sample.Markers }
        /// Set value of the fourth marker
        let withMarker4 value (sample : Sample) =
            { sample with Markers = markersWithMarker4 value sample.Markers }
        /// Set value of all markers at once
        let withMarkers markers (sample: Sample) =
            { sample with Markers = markers }

        /// Convert a Phase type into a float value of radians for use in the mathematical functions
        let private phaseToRadians =
            function
            | PhaseInRad (angle) -> angle / 1.0<rad>
            | PhaseInDeg (angle) -> angle * (Math.PI * 2.0 / 360.0) * 1.0<1/deg>

        /// The maximum amplitude in arbitrary units that the machine can take for an IQ point amplitude
        let private maximumMachineAmplitude = Int16.MaxValue

        /// Generate a sample at the given amplitude and phase.  The amplitude is relative to the
        /// maximum amplitude available with the current scaling setting on the machine.
        let generateSample relativeAmplitude phase markers =
            let phaseAngle = phaseToRadians phase
            let amplitude = relativeAmplitude * float maximumMachineAmplitude
            defaultIQSample
            |> withI (int16 (amplitude * Math.Cos phaseAngle))
            |> withQ (int16 (amplitude * Math.Sin phaseAngle))
            |> withMarkers markers

    /// Functions for encoding segments and samples into a writeable form
    [<AutoOpen>]
    module internal Translate =
        [<AutoOpen>]
        module Encode =
            /// Get the string form of a SegmentId
            let extractSegmentId (SegmentId id) = id
            /// Get the string form of a SequenceId
            let extractSequenceId (SequenceId id) = id

            /// Make a marker byte out of the booleans in an IQ sample
            let private getMarkerByte (sample : Sample) =
                ((Convert.ToByte sample.Markers.M4) <<< 3) ||| ((Convert.ToByte sample.Markers.M3) <<< 2)
                ||| ((Convert.ToByte sample.Markers.M2) <<< 1) ||| (Convert.ToByte sample.Markers.M1)

            /// Add a single encoded sample into an encoded segment
            let private addEncodedSample (total : EncodedSegment) (sample : EncodedSample) =
                { EncodedSegment.Name    = total.Name
                  EncodedSegment.IQ      = sample.IQ :: total.IQ
                  EncodedSegment.Markers = sample.Markers :: total.Markers }

            /// Convert array of bytes to bigendian if necessary
            let private toBigEndian bytes =
                if BitConverter.IsLittleEndian then
                    bytes |> Array.rev
                else
                    bytes

            /// Convert a 16-bit integer to an array of bytes in machine order
            let private toBytes (number : int16) =
                number
                |> BitConverter.GetBytes
                |> toBigEndian

            /// Encode a single sample into the necessary byte patterns
            let private toEncodedSample sample =
                { EncodedSample.IQ      = Array.append (toBytes sample.I) (toBytes sample.Q)
                  EncodedSample.Markers = getMarkerByte sample }

            /// Encode a segment into the necessary byte patterns
            let private toEncodedSegment (segment : Segment) =
                let emptySegment = { Name =
                                         segment.Name
                                         |> extractSegmentId
                                         |> Text.Encoding.ASCII.GetBytes
                                     IQ = []
                                     Markers = [] }
                segment.Data
                |> Seq.map toEncodedSample
                |> Seq.fold addEncodedSample emptySegment

            /// Make the data string, including the '#' character, the digits of length, the length
            /// and the data
            let private makeDataString (data : byte []) =
                let length = data.Length
                let digits = length.ToString().Length
                if digits >= 10 then
                    failwith "Can't write 1GB in one go!"
                Array.concat [
                    "#"B
                    Text.Encoding.ASCII.GetBytes(digits.ToString())
                    Text.Encoding.ASCII.GetBytes(length.ToString())
                    data ]

            /// ASCII string of the folder location for waveforms
            let private waveformFolder = "WFM1:"B
            /// ASCII string of the folder location for markers
            let private markerFolder   = "MKR1:"B
            /// ASCII string of the folder location for headers
            let private headerFolder   = "HDR1:"B
            /// ASCII string of the folder location for sequences
            let private sequenceFolder = "SEQ:"B

            /// Build up a full file name string for storing a file
            let private fileNameString folder name = Array.concat ["\""B; folder; name; "\""B]

            /// Total filename string for a waveform file
            let private makeWaveformFileString name = fileNameString waveformFolder name
            /// Total filename string for a markers file
            let private makeMarkerFileString name = fileNameString markerFolder name
            /// Total filename string for a header file
            let private makeHeaderFileString name = fileNameString headerFolder name
            /// Total filename string for a sequence file
            let private makeSequenceFileString name = fileNameString sequenceFolder name

            /// Build up a full string for data storage and location
            let private dataStorageString fileName dataString =
                Array.concat [fileName; ","B; dataString]

            /// Reverse a list of arrays, then concatenate
            let private reverseConcatenateToArray list =
                list
                |> List.rev
                |> List.reduce Array.append

            /// Reverse a list and convert it to an array
            let private reverseToArray list =
                list
                |> List.rev
                |> List.toArray

            /// Produce the full data strings necessary for writing the three different files
            /// to the machine, given the encoded segment to extract the data from.
            let toEncodedSegmentFiles (segment : Segment) =
                let encodedsegment = toEncodedSegment segment
                let waveformFileName = makeWaveformFileString encodedsegment.Name
                let markerFileName   = makeMarkerFileString   encodedsegment.Name
                let headerFileName   = makeHeaderFileString   encodedsegment.Name
                let segmentDataString =
                    encodedsegment.IQ
                    |> reverseConcatenateToArray
                    |> makeDataString
                let markerDataString =
                    encodedsegment.Markers
                    |> reverseToArray
                    |> makeDataString
                // TODO: fix header data string
                let headerDataString = "#10"B

                { Waveform = dataStorageString  waveformFileName segmentDataString
                  Markers  = dataStorageString  markerFileName   markerDataString
                  Header   = dataStorageString  headerFileName   headerDataString }

            /// Get the whole string necessary to write a waveform file to the machine
            let waveformDataString (encoded : EncodedSegmentFiles) = encoded.Waveform
            /// Get the whole string necessary to write a marker file to the machine
            let markersDataString (encoded : EncodedSegmentFiles) = encoded.Markers
            /// Get the whole string necessary to write a header file to the machine
            let headerDataString (encoded : EncodedSegmentFiles) = encoded.Header

            /// Create an internal stored segment representation
            let toStoredSegment (segment : Segment) =
                StoredSegment segment.Name

            /// Create and internal stored sequence representation
            let toStoredSequence (sequence : Sequence) =
                StoredSequence sequence.Name

            /// Make an ASCII string out of an object's ToString() method
            let toASCIIString obj =
                obj.ToString() |> Text.Encoding.ASCII.GetBytes
            /// Get the ASCII string of a segment's ID
            let getSegmentASCIIString (segment : Segment) =
                segment.Name
                |> extractSegmentId
                |> toASCIIString
            /// Get the ASCII string representation of a StoredSegment
            let getStoredSegmentASCIIString (StoredSegment id) =
                id
                |> extractSegmentId
                |> Text.Encoding.ASCII.GetBytes
            /// Get the ASCII string representation of a StoredSequence
            let getStoredSequenceASCIIString (StoredSequence id) =
                id
                |> extractSequenceId
                |> Text.Encoding.ASCII.GetBytes

            /// Get the full file name of a waveform file from the short name stored in the
            /// StoredSegment.  For example, if the StoredSegment name is "test", then this
            /// function returns "\"WFM1:test\""B
            let fullWaveformFileName (segment : StoredSegment) =
                segment
                |> getStoredSegmentASCIIString
                |> makeWaveformFileString

            /// Get the full file name of a sequence file from the short name stored in the
            /// StoredSequence.  For example, if the StoredSequence name is "test", then this
            /// function returns "\"SEQ:test\""B
            let fullSequenceFileName (sequence : StoredSequence) =
                sequence
                |> getStoredSequenceASCIIString
                |> makeSequenceFileString

            /// Make a sequence element into a tuple of the byte array of the full filename
            /// and the ASCII representation of the number of repetitions
            let private makeASCIISequenceElement =
                function
                | Segment (segment, reps)   -> (fullWaveformFileName segment,  toASCIIString reps)
                | Sequence (sequence, reps) -> (fullSequenceFileName sequence, toASCIIString reps)

            /// Encode a sequence element into the form "\"<filename>\",<reps>,<markers>"B
            let private toEncodedSequenceElement (element : SequenceElement) =
                let (name, reps) = makeASCIISequenceElement element
                Array.concat [ name ; ","B ; reps ; ",ALL"B ]

            /// Encode a whole sequence in an EncodedSequence
            let sequenceDataString (sequence : Sequence) =
                let name =
                    sequence.Name
                    |> extractSequenceId
                    |> Text.Encoding.ASCII.GetBytes
                sequence.Sequence
                |> List.map toEncodedSequenceElement
                |> List.map (Array.append ","B) // actually prepends ','B, but we want this
                |> List.reduce Array.append
                |> Array.append (makeSequenceFileString name)

        /// Functions for decoding segment and sequence data received from the machine
        [<AutoOpen>]
        module Decode =
            /// Convert a big-endian array of bytes into the host order
            let private toHostOrder bytes =
                if BitConverter.IsLittleEndian then
                    bytes |> Array.rev
                else
                    bytes

            /// Decompress the I and Q data back into a 2-tuple of I and Q
            let private getIQ (array : byte []) =
                let bytesI = toHostOrder [| array.[0]; array.[1] |]
                let intI   = BitConverter.ToInt16 (bytesI, 0)
                let bytesQ = toHostOrder [| array.[2]; array.[3] |]
                let intQ   = BitConverter.ToInt16 (bytesQ, 0)
                (intI, intQ)

            /// Decompress the markers back into a 4-tuple of the 4 Boolean markers
            let private getMarkers (markers : EncodedMarkers) =
                { M1 = Convert.ToBoolean(markers &&& 0x1uy)
                  M2 = Convert.ToBoolean(markers &&& 0x2uy)
                  M3 = Convert.ToBoolean(markers &&& 0x4uy)
                  M4 = Convert.ToBoolean(markers &&& 0x8uy) }

            /// Decode an encoded sample back into the internal representation of a sample
            let private toSample encodedIQ encodedMarkers =
                let (I, Q) = getIQ encodedIQ
                let markers = getMarkers encodedMarkers
                { I = I
                  Q = Q
                  Markers = markers }

            /// Decode an encoded segment back into the internal representation of the segment
            let private toSegment (encodedSegment : EncodedSegment) =
                let name =
                    encodedSegment.Name
                    |> Text.Encoding.UTF8.GetString
                    |> SegmentId
                let data =
                    List.map2 toSample encodedSegment.IQ encodedSegment.Markers
                    |> List.rev
                    |> List.toSeq
                { Segment.Name = name
                  Segment.Data = data }

            /// Test if two values are equal
            let private isEqual a b = (a = b)

            /// Find the index of the first occurence of a search term in an array
            let private firstOccurence term = Array.findIndex (isEqual term)

            let private splitArray start finish (array : 'a []) =
                [| for i in start .. finish -> array.[i] |]

            /// Get the file name out of the encoded data string
            let private parseBytesFilename data =
                // String is of form "\"WFM1:filename\",#..."B
                // Find index of the first character after the colon
                let start  = (firstOccurence ':'B data) + 1
                // Find index of the end of the file name by counting back from the comma
                // findIndex finds the first instance, so searching for '\"'B would find the
                // wrong place
                let finish = (firstOccurence ','B data) - 2
                splitArray start finish data

            /// Get the length of the data to be read
            let private getDataLength data =
                // Get index of the start of the data string
                let start = (firstOccurence '#'B data) + 1
                let digits = int data.[start]
                // Not the fastest way, but should be the most accurate
                data
                |> splitArray (start + 1) (start + digits)
                |> Text.Encoding.UTF8.GetString
                |> int
                // Could do it like this if it's faster, but we could have cruft after the data
                // (Array.length data) - (start + digits + 1)

    /// Functions for writing and receiving data from the machine
    [<AutoOpen>]
    module Control =
        open Translate

        /// Concatenate two error strings into one string
        let private addErrorStrings str1 str2 = str1 + " AND ALSO " + str2
        /// Generic function to parallelise the the storage of a sequence of storables
        let private parallelizeStorageFold errorState errorFold store instrument sequence =
            sequence
            |> Seq.map (store instrument)
            |> AsyncChoice.Parallel errorState errorFold
        /// Parallelise the storage of a sequence of storables, concatenating any errors which may
        /// occur
        let private parallelizeStorage store instrument sequence =
            parallelizeStorageFold "" addErrorStrings store instrument sequence

        /// <summary><para>
        /// Command to write file to volatile memory
        /// </para><para>
        /// Command reference p.133, p.151. p.133 is technically for the ":MEM:DATA" command rather
        /// than the ":MMEM:DATA" command, but they are identical, and the former has more
        /// information.
        /// </para></summary>
        let private storeDataKey = ":MMEM:DATA"
        /// Store the three files associated with any segment in the machine's volatile memory.
        /// Returns a StoredSegment type representing the data stored.
        let storeSegment instrument segment =
            let encoded = toEncodedSegmentFiles segment
            asyncChoice {
                do! IO.setBytesValue waveformDataString storeDataKey instrument encoded
                do! IO.setBytesValue markersDataString storeDataKey instrument encoded
                do! IO.setBytesValue headerDataString storeDataKey instrument encoded
                return toStoredSegment segment }

        /// Store a sequence of segments into the volatile memory of the machine.  Returns an
        /// array of StoredSegments.
        let storeSegmentSequence instrument sequence =
            parallelizeStorage storeSegment instrument sequence

        /// <summary><para>
        /// Command to delete all waveform, markers and header files stored in the BBG memory
        /// of the machine (the usual place that they're stored in)
        /// </para><para>
        /// Command reference p.152
        /// </para></summary>
        let private deleteAllSegmentsKey = ":MMEM:DEL:WFM"
        /// Delete all the waveform, markers and header files stored in the BBG memory of the
        /// machine (the usual storage location)
        let deleteAllStoredSegments = IO.writeKey deleteAllSegmentsKey
        /// <summary><para>
        /// Command to delete all sequence files stored in the internal memory of the machine
        /// (the usual storage location).
        /// </para><para>
        /// Command reference p.146. Uses ":MEM" rather than ":MMEM" for some reason.
        /// </para></summary>
        let private deleteAllSequencesKey = ":MEM:DEL:SEQ"
        /// Delete all the sequence files stored in the internal memory of the machine (the usual
        /// storage location).
        let deleteAllStoredSequences = IO.writeKey deleteAllSequencesKey

        /// <summary><para>
        /// Command to delete any file on the machine by name.  If a waveform file is passed,
        /// any associated markers and header files are deleted alongside it.
        /// </para><para>
        /// Command reference p.152
        /// </para></summary>
        let private deleteFileKey = ":MMEM:DEL:NAME"
        /// Delete a segment from the machine's BBG data storage.  Includes deleting the waveform
        /// file, the markers file and the headers file (if present).
        let deleteStoredSegment = IO.setBytesValue fullWaveformFileName deleteFileKey
        /// Delete a sequence from the machine's internal data storage (the usual storage
        /// location).
        let deleteStoredSequence = IO.setBytesValue fullSequenceFileName deleteFileKey

        /// <summary><para>
        /// Key to store sequences to the machine.
        /// </para><para>
        /// Command reference p.345
        /// </para></summary>
        let private storeSequenceKey = ":RAD:ARB:SEQ"
        /// Write a sequence file to the machine and returns the stored sequence type
        let storeSequence instrument sequence = asyncChoice {
            do! IO.setBytesValue sequenceDataString storeSequenceKey instrument sequence
            return toStoredSequence sequence }
        /// Write a sequence of sequences files to the machine, and return an array of the stored
        /// sequence type
        let storeSequenceSequence instrument sequence =
            parallelizeStorage storeSequence instrument sequence