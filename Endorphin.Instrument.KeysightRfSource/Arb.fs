namespace Endorphin.Instrument.Keysight

open System
open ExtCore.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module ARB =
    /// Minimum number of samples needed for a valid Segment on the machine.
    let minimumSegmentLength = 60u

    /// Functions for configuring samples.
    [<AutoOpen>]
    module Configure =
        /// A markers record with all markers turned off.
        let defaultMarkers = { M1 = false; M2 = false; M3 = false; M4 = false }
        /// Basic data form of IQ point.
        let defaultIqSample = {
            Sample.I = 0s
            Sample.Q = 0s
            Sample.Markers = defaultMarkers }
        /// Set value of the I sample.
        let withI value sample = { sample with I = value }
        /// Set value of the Q sample.
        let withQ value sample = { sample with Q = value }

        /// Set value of the first marker.
        let private markersWithMarker1 value markers = { markers with M1 = value }
        /// Set value of the second marker.
        let private markersWithMarker2 value markers = { markers with M2 = value }
        /// Set value of the third marker.
        let private markersWithMarker3 value markers = { markers with M3 = value }
        /// Set value of the fourth marker.
        let private markersWithMarker4 value markers = { markers with M4 = value }

        /// Set value of the first marker.
        let withMarker1 value (sample : Sample) =
            { sample with Markers = markersWithMarker1 value sample.Markers }
        /// Set value of the second marker.
        let withMarker2 value (sample : Sample) =
            { sample with Markers = markersWithMarker2 value sample.Markers }
        /// Set value of the third marker.
        let withMarker3 value (sample : Sample) =
            { sample with Markers = markersWithMarker3 value sample.Markers }
        /// Set value of the fourth marker.
        let withMarker4 value (sample : Sample) =
            { sample with Markers = markersWithMarker4 value sample.Markers }

        /// Set value of all markers at once.
        let withMarkers markers (sample: Sample) =
            { sample with Markers = markers }

        /// Convert a Phase type into a float value of radians for use in the mathematical functions.
        let private phaseToRadians = function
            // We want IQ to be equal at 0 phase, so rotate phases by pi/4
            | PhaseInRad (angle) -> (angle / 1.0<rad>) + (Math.PI / 4.0)
            | PhaseInDeg (angle) -> (angle * (Math.PI * 2.0 / 360.0) * 1.0<1/deg>) + (Math.PI / 4.0)

        /// The maximum amplitude in arbitrary units that the machine can take for an IQ point amplitude.
        let private maximumMachineAmplitude = Int16.MaxValue

        /// Generate a sample at the given amplitude and phase.  The amplitude is relative to the
        /// maximum amplitude available with the current scaling setting on the machine.
        let withAmplitudeAndPhase relativeAmplitude phase sample =
            let phaseAngle = phaseToRadians phase
            let amplitude = relativeAmplitude * float maximumMachineAmplitude
            sample
            |> withI (int16 (amplitude * Math.Cos phaseAngle))
            |> withQ (int16 (amplitude * Math.Sin phaseAngle))

        /// The default clock rate for the dual ARB system.
        let defaultArbClockFrequency = FrequencyInHz 150.0e6<Hz>

        /// Key for use with the dual ARB clock frequency.
        /// Command reference p.344.
        let private dualArbClockKey = ":RADIO:ARB:SCLOCK:RATE"
        /// Set the dual ARB clock frequency to the value specified.
        let setDualArbClock = IO.setFrequency dualArbClockKey
        /// Query the current value of the dual ARB clock frequency.
        let queryDualArbClock = IO.queryFrequency dualArbClockKey

        /// Key for saving header files of waveform segments in the dual ARB system.
        let private dualArbSaveHeaderKey = ":RADIO:ARB:HEADER:SAVE"
        /// Save the current dual ARB settings to the header file of the currently selected waveform.
        let setHeaderFile = IO.writeKey dualArbSaveHeaderKey

        [<AutoOpen>]
        module Trigger =
            /// Key for the type of the dual ARB system's trigger.
            /// Command reference p.347.
            let private arbTriggerTypeKey = ":RADIO:ARB:TRIGGER:TYPE"

            /// Key for the mode of the continuous trigger of the dual ARB system.
            /// Command reference p.349.
            let private arbContinuousModeKey = ":RADIO:ARB:TRIGGER:TYPE:CONTINUOUS"

            /// Key for the number of repeats in the dual ARB single trigger mode.
            /// Command reference p.351.
            let private arbSingleRepeatsKey = ":RADIO:ARB:TRIGGER:TYPE:SINGLE:REPEAT"

            /// Key for the polarity of the gate-type trigger of the dual ARB system.
            /// Command reference p.350.
            let private arbGatePolarityKey = ":RADIO:ARB:TRIGGER:TYPE:GATE"

            /// Key for the mode of the segment advance type trigger of the dual ARB system.
            /// Command reference p.350.
            let private arbSegmentAdvanceModeKey = ":RADIO:ARB:TRIGGER:TYPE:SADVANCE"

            /// Get a machine-readable string representation of the ARB trigger type.
            let private arbTriggerTypeString = function
                | Continuous _ -> "CONT"
                | Single _ -> "SING"
                | Gate _ -> "GATE"
                | SegmentAdvance _ -> "SADV"

            /// Convert an internal representation of the continuous type mode of the dual ARB triggering
            /// system into a machine representation.
            let private arbContinuousModeString = function
                | ArbContinuousFree -> "FREE"
                | ArbContinuousTrigger -> "TRIGGER"
                | ArbContinuousReset -> "RESET"

            /// Convert a machine representation of the continuous type mode of the dual ARB triggering
            /// system into an internal representation.
            let private parseArbContinuousMode str =
                match String.toUpper str with
                | "FREE" -> ArbContinuousFree
                | "TRIG" | "TRIGGER" -> ArbContinuousTrigger
                | "RES" | "RESET" -> ArbContinuousReset
                | _ -> failwithf "Unexpected ARB continuous mode trigger type string: %s" str

            /// Convert an internal representation of the segment advance type mode of the dual ARB
            /// triggering system into a machine representation.
            let private arbSegmentAdvanceModeString = function
                | ArbSegmentAdvanceSingle -> "SINGLE"
                | ArbSegmentAdvanceContinuous -> "CONTINUOUS"

            /// Convert a machine representation of the segment advance type mode of the dual ARB
            /// triggering system into a machine representation.
            let private parseArbSegmentAdvanceMode str =
                match String.toUpper str with
                | "SING" | "SINGLE" -> ArbSegmentAdvanceSingle
                | "CONT" | "CONTINUOUS" -> ArbSegmentAdvanceContinuous
                | _ -> failwithf "Unexpected ARB segment advance mode trigger type string: %s" str

            /// Set the type of the ARB trigger to the given type.
            let internal setArbTriggerType = IO.setValueString arbTriggerTypeString arbTriggerTypeKey

            /// Set the mode of the dual ARB continuous trigger.
            let internal setArbContinuousMode = IO.setValueString arbContinuousModeString arbContinuousModeKey
            /// Set the number of repeats per point in the dual ARB single trigger mode.
            let internal setArbSingleRepeats = IO.setUint16 arbSingleRepeatsKey
            /// Set the polarity of the gating trigger in the dual ARB system.
            let internal setArbGatePolarity = IO.setLowHighState arbGatePolarityKey
            /// Set the mode of the segment advance trigger of the dual ARB system.
            let internal setArbSegmentAdvanceMode =
                IO.setValueString arbSegmentAdvanceModeString arbSegmentAdvanceModeKey

            /// Set the dual ARB trigger to have the value given.
            let setArbTrigger instrument trigger = asyncChoice {
                do! setArbTriggerType instrument trigger
                match trigger with
                | Continuous mode     -> do! setArbContinuousMode instrument mode
                | Single reps         -> do! setArbSingleRepeats instrument reps
                | Gate polarity       -> do! setArbGatePolarity instrument polarity
                | SegmentAdvance mode -> do! setArbSegmentAdvanceMode instrument mode }

            /// Query the currently set value of the dual ARB system triggering.
            let queryArbTrigger instrument = asyncChoice {
                let helper str =
                    match String.toUpper str with
                        | "CONT" | "CONTINUOUS" ->
                            IO.queryValue parseArbContinuousMode arbContinuousModeKey instrument
                            |> AsyncChoice.map ArbTrigger.Continuous
                        | "SING" | "SINGLE" ->
                            IO.queryUint16 arbSingleRepeatsKey instrument
                            |> AsyncChoice.map ArbTrigger.Single
                        | "GATE" ->
                            IO.queryLowHighState arbGatePolarityKey instrument
                            |> AsyncChoice.map ArbTrigger.Gate
                        | "SADV" | "SADVANCE" ->
                            IO.queryValue parseArbSegmentAdvanceMode arbSegmentAdvanceModeKey instrument
                            |> AsyncChoice.map ArbTrigger.SegmentAdvance
                        | str -> failwithf "Unexpected ARB trigger type string: %s" str
                let! triggerType = IO.queryValue (fun str -> str) arbTriggerTypeKey instrument
                return! helper triggerType }

    /// Functions for encoding segments and samples into a writeable form.
    [<AutoOpen>]
    module internal Translate =
        [<AutoOpen>]
        module Encode =
            /// Make a marker byte out of the booleans in an IQ sample.
            let private getMarkerByte (sample : Sample) =
                ((Convert.ToByte sample.Markers.M4) <<< 3) ||| ((Convert.ToByte sample.Markers.M3) <<< 2)
                ||| ((Convert.ToByte sample.Markers.M2) <<< 1) ||| (Convert.ToByte sample.Markers.M1)

            /// Convert a 16-bit integer to an array of bytes in machine order.
            let private toBytes (number : int16) =
                [| byte ((number &&& 0xFF00s) >>> 8); byte (number &&& 0xFFs) |]

            /// Get a four-byte array of the IQ data in the correct endianness.
            let private iqBytes sample =
                let i = toBytes sample.I
                let q = toBytes sample.Q
                [| i.[0]; i.[1]; q.[0]; q.[1] |]

            /// Create a tuple of iq, markers encoded as byte sequences.
            let toEncodedSegmentData (segment : Segment) =
                let sampleCount = int segment.Length
                let iq = Array.create (sampleCount * 4) 0uy
                let markers = Array.create sampleCount 0uy
                let mutable sampleIndex = 0
                let mutable used = 0u
                let singleIq = Array.create 4 0uy
                let mutable singleMarkers = 0uy
                for i in 0 .. (sampleCount - 1) do
                    let (sample, SampleCount count) = segment.Samples.[sampleIndex]
                    if used = 0u then
                        singleIq.[0 .. 3] <- iqBytes sample
                        singleMarkers <- getMarkerByte sample
                    iq.[(4 * i) .. (4 * i) + 3] <- singleIq
                    markers.[i]      <- singleMarkers
                    if used = count - 1u then
                        used <- 0u
                        sampleIndex <- sampleIndex + 1
                    else used <- used + 1u
                (iq, markers)

            /// Encode a segment into the necessary byte patterns.
            let private toEncodedSegment segment =
                let (iq, markers) = toEncodedSegmentData segment
                { EncodedIQ = iq
                  EncodedMarkers = markers }

            /// Make the data string, including the '#' character, the digits of length, the length
            /// and the data.
            let private dataString (data : byte []) =
                let length = data.Length
                let digits = length.ToString().Length
                if digits >= 10 then
                    failwith "Can't write 1GB in one go!"
                Array.concat [
                    "#"B
                    Text.Encoding.ASCII.GetBytes(digits.ToString())
                    Text.Encoding.ASCII.GetBytes(length.ToString())
                    data ]

            /// Build up a full string for data storage and location.
            let private dataStorageString (fileName : string) dataString =
                Array.concat [System.Text.Encoding.ASCII.GetBytes fileName; ","B; dataString]

            /// Produce the full data strings necessary for writing the two different files
            /// to the machine, given the encoded segment to extract the data from.  Ignores
            /// the header file, but the only bits we usually care about here are more easily
            /// set by SCPI commands.
            let toEncodedSegmentFiles segment (SegmentId id) =
                let encodedSegment = toEncodedSegment segment
                let waveformFilename = waveformFileString id
                let markerFilename   = markerFileString   id
                let waveformDataString = dataString encodedSegment.EncodedIQ
                let markerDataString   = dataString encodedSegment.EncodedMarkers
                { Waveform = dataStorageString  waveformFilename waveformDataString
                  Markers  = dataStorageString  markerFilename   markerDataString }

            /// Get the whole string necessary to write a waveform file to the machine.
            let waveformDataString (encoded : EncodedSegmentFiles) = encoded.Waveform
            /// Get the whole string necessary to write a marker file to the machine.
            let markersDataString (encoded : EncodedSegmentFiles) = encoded.Markers

            /// Make a sequence element into a tuple of the byte array of the full filename
            /// and the ASCII representation of the number of repetitions.
            let private asciiSequenceElement (el, reps) =
                (storedWaveformFilename el, asciiString reps)

            /// Encode a sequence element into the form "\"<filename>\",<reps>,<markers>"B.
            let private toEncodedSequenceElement (element : SequenceElement) =
                let (name, reps) = asciiSequenceElement element
                Array.concat [ System.Text.Encoding.ASCII.GetBytes name; ","B; reps; ",ALL"B ]

            /// Convert a sequence into an ASCII string of its elements.
            let private sequenceData sequence =
                sequence
                |> List.map toEncodedSequenceElement
                |> List.map (Array.append ","B) // actually prepends ','B, but we want this
                |> List.reduce Array.append

            /// Encode a whole sequence in an EncodedSequence.
            let sequenceDataString (SequenceId id) (sequence : Sequence) =
                let name =
                    id
                    |> sequenceFileString
                    |> System.Text.Encoding.ASCII.GetBytes
                sequence
                |> sequenceData
                |> Array.append name

            /// Get a unique representation of a sample as a byte array.
            let sampleToBytes sample =
                let arr = Array.create 5 0uy
                arr.[0 .. 3] <- iqBytes sample
                arr.[4] <- getMarkerByte sample
                arr

            /// Get a unique representation of a segment as a byte array.
            let segmentToBytes segment =
                let length = segment.Samples.Length
                let arr = Array.create (length * 9) 0uy // 5 bytes per sample, 4 bytes per count
                for i in 0 .. length - 1 do
                    let (sample, SampleCount reps) = segment.Samples.[i]
                    arr.[(i * 9) + 0 .. (i * 9) + 4] <- sampleToBytes sample
                    arr.[(i * 9) + 5 .. (i * 9) + 8] <- BitConverter.GetBytes reps
                    // endianness doesn't matter here
                arr // return the byte array we just created

            /// Get a unique representation of a sequence as a byte array.
            let sequenceToBytes = sequenceData

        /// Functions for decoding segment and sequence data received from the machine.
        [<AutoOpen>]
        module Decode =
            /// Convert a big-endian array of bytes into the host order.
            let private toHostOrder bytes =
                if BitConverter.IsLittleEndian then
                    bytes |> Array.rev
                else
                    bytes

            /// Decompress the I and Q data back into a 2-tuple of I and Q.
            let private getIQ (array : byte []) =
                let bytesI = toHostOrder [| array.[0]; array.[1] |]
                let intI   = BitConverter.ToInt16 (bytesI, 0)
                let bytesQ = toHostOrder [| array.[2]; array.[3] |]
                let intQ   = BitConverter.ToInt16 (bytesQ, 0)
                (intI, intQ)

            /// Decompress the markers back into a 4-tuple of the 4 Boolean markers.
            let private getMarkers markers =
                { M1 = Convert.ToBoolean(markers &&& 0x1uy)
                  M2 = Convert.ToBoolean(markers &&& 0x2uy)
                  M3 = Convert.ToBoolean(markers &&& 0x4uy)
                  M4 = Convert.ToBoolean(markers &&& 0x8uy) }

            /// Decode an encoded sample back into the internal representation of a sample.
            let private toSample encodedIQ encodedMarkers =
                let (I, Q) = getIQ encodedIQ
                let markers = getMarkers encodedMarkers
                { I = I
                  Q = Q
                  Markers = markers }

            /// Test if two values are equal.
            let private isEqual a b = (a = b)

            /// Find the index of the first occurence of a search term in an array.
            let private firstOccurence term = Array.findIndex (isEqual term)

            /// Get a slice of an array, including the indexes start and finish.
            let private splitArray start finish (array : 'a []) =
                [| for i in start .. finish -> array.[i] |]

            /// Get the file name out of the encoded data string.
            let private parseBytesFilename data =
                // String is of form "\"WFM1:filename\",#..."B
                // Find index of the first character after the colon
                let start  = (firstOccurence ':'B data) + 1
                // Find index of the end of the file name by counting back from the comma
                // findIndex finds the first instance, so searching for '\"'B would find the
                // wrong place
                let finish = (firstOccurence ','B data) - 2
                splitArray start finish data

            /// Get the length of the data to be read.
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