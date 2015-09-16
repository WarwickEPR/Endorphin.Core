namespace Endorphin.Instrument.Keysight

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core

[<RequireQualifiedAccess>]
module ARB =
    /// The shortest length of time a pulse can be, measured in seconds.
    let shortestPulseDuration = (2.0e-8<s>)/3.0
    /// The default clock rate for the dual ARB system.
    let defaultClockFrequency = FrequencyInHz 150.0e6<Hz>

    /// Key for use with the dual ARB clock frequency.
    /// Command reference p.344.
    let private clockKey = ":RADIO:ARB:SCLOCK:RATE"
    /// Set the dual ARB clock frequency to the value specified.
    let setClock = IO.setFrequency clockKey
    /// Query the current value of the dual ARB clock frequency.
    let queryClock = IO.queryFrequency clockKey

    /// Key for saving header files of waveform segments in the dual ARB system.
    let private saveHeaderKey = ":RADIO:ARB:HEADER:SAVE"
    /// Save the current dual ARB settings to the header file of the currently selected waveform.
    let setHeaderFile = IO.writeKey saveHeaderKey

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
    let private setState value instrument = async {
        do! IO.setOnOffState arbStateKey instrument value
        do! IO.setOnOffState modulationStateKey instrument value
        do! IO.setOnOffState outputStateKey instrument value }

    /// Turn on the ARB generator of the instrument.
    let turnOn = setState On
    /// Turn off the ARB generator of the instrument.
    let turnOff = setState Off

    [<RequireQualifiedAccess>]
    module Trigger =
        /// The default mode for the ARB triggering system.
        let private emptyMode = ArbContinuous FreeRun

        /// The default source for the ARB triggering system.
        let private emptySource = Some (ArbExternal (ArbBnc, Some Negative, None))

        /// The default state of the ARB trigger, after a *RST command.
        let empty = ArbTrigger (emptyMode, emptySource)

        /// Set the mode of the ARB triggering system to continuous, with a given behaviour
        /// for received triggers.
        let continuous mode = ArbTrigger (ArbContinuous mode, None)

        /// Set the mode of the ARB triggering system to single triggered, with a given number
        /// of repeats per waveform, and a behaviour for subsequent received triggers.
        /// Also needs a trigger source to be set.
        let single repeats retrigger = ArbTrigger (ArbSingle (repeats, retrigger), None)

        /// Set the mode of the ARB triggering system to gate triggered, with a polarity for the
        /// trigger source.
        /// Also needs a trigger source to be set.
        let gate polarity = ArbTrigger (ArbGate polarity, None)

        /// Set the mode of the ARB triggering system to "segment advance", with the given mode
        /// for received triggers.
        /// Also needs a trigger source to be set.
        let segmentAdvace mode = ArbTrigger (ArbSegmentAdvance mode, None)

        /// Set the source of the ARB trigger to be the front-panel "Trigger" key.
        let byKey (ArbTrigger (mode, _)) = ArbTrigger (mode, Some ArbKey)

        /// Set the source of the ARB trigger to be the command bus between the computer and the machine.
        let byBus (ArbTrigger (mode, _)) = ArbTrigger (mode, Some ArbBus)

        /// Set the source of the ARB trigger to be from somewhere external.
        let byExternal connector (ArbTrigger (mode, cur)) =
            match cur with
            | Some ArbKey
            | Some ArbBus
            | None -> ArbTrigger (mode, Some <| ArbExternal (connector, None, None))
            | Some (ArbExternal (_, polarity, delay)) ->
                ArbTrigger (mode, Some <| ArbExternal (connector, polarity, delay))

        /// Set the polarity of an external ARB trigger.  Must be set after the trigger type
        /// is set to external.
        let withPolarity polarity (ArbTrigger (mode, cur)) =
            match cur with
            | Some ArbKey
            | Some ArbBus
            | None -> ArbTrigger (mode, cur)
            | Some (ArbExternal (connector, _, delay)) ->
                ArbTrigger (mode, Some <| ArbExternal (connector, polarity, delay))

        /// Set the optional delay of the ARB external trigger.  Must be set after the trigger type
        /// is set to external.
        let withDelay delay (ArbTrigger (mode, cur)) =
            match cur with
            | Some ArbKey
            | Some ArbBus
            | None -> ArbTrigger (mode, cur)
            | Some (ArbExternal (connector, polarity, _)) ->
                ArbTrigger (mode, Some <| ArbExternal (connector, polarity, delay))

        /// Key for the type of the mode of the dual ARB system's trigger.
        /// Command reference p.347.
        let private modeTypeKey = ":RADIO:ARB:TRIGGER:TYPE"

        /// Key for the type of the source of the dual ARB system's trigger.
        /// Command reference p.352.
        let private sourceTypeKey = ":RADIO:ARB:TRIGGER:SOURCE"

        /// Key for the mode of the continuous trigger of the dual ARB system.
        /// Command reference p.349.
        let private continuousModeKey = ":RADIO:ARB:TRIGGER:TYPE:CONTINUOUS"

        /// Key for the number of repeats in the dual ARB single trigger mode.
        /// Command reference p.351.
        let private singleRepeatsKey = ":RADIO:ARB:TRIGGER:TYPE:SINGLE:REPEAT"

        /// Key for the retrigger mode of the dual ARB system.
        /// Command reference p.343.
        let private retriggerModeKey = ":RADIO:ARB:RETRIGGER"

        /// Key for the polarity of the gate-type trigger of the dual ARB system.
        /// Command reference p.350.
        let private gatePolarityKey = ":RADIO:ARB:TRIGGER:TYPE:GATE"

        /// Key for the mode of the segment advance type trigger of the dual ARB system.
        /// Command reference p.350.
        let private segmentAdvanceModeKey = ":RADIO:ARB:TRIGGER:TYPE:SADVANCE"

        /// Key for the duration of the delay between receiving an external trigger and beginning
        /// playback of the waveform.
        /// Command reference p.353.
        let private delayKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:DELAY"

        /// Key for the state of the delay in an external trigger source for the dual ARB
        /// system.
        /// Command reference p.353.
        let private delayStateKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:DELAY:STATE"

        /// Key for the polarity of the external trigger source for all trigger modes except
        /// gated (which has its own polarity).
        /// Command reference p.354.
        let private polarityKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:SLOPE"

        /// Key for the physical location of an external trigger source of the dual ARB system.
        /// Command reference p.354.
        let private sourceLocationKey = ":SOURCE:RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:SOURCE"
        // This key has 3 of the word "SOURCE" in it, and all 3 are optional! But I put them in
        // because I needed entertainment.

        /// Get a machine-readable string representation of the ARB trigger mode type.
        let private modeTypeString = function
            | ArbContinuous _ -> "CONT"
            | ArbSingle _ -> "SING"
            | ArbGate _ -> "GATE"
            | ArbSegmentAdvance _ -> "SADV"

        /// Get a machine-readable string representation of the ARB trigger source type.
        let private sourceTypeString = function
            | ArbKey -> "KEY"
            | ArbBus -> "BUS"
            | ArbExternal _ -> "EXT"

        /// Convert an internal representation of the continuous type mode of the dual ARB triggering
        /// system into a machine representation.
        let private continuousModeString = function
            | FreeRun -> "FREE"
            | TriggerThenRun -> "TRIGGER"
            | ResetOnTrigger -> "RESET"

        /// Convert a machine representation of the continuous type mode of the dual ARB triggering
        /// system into an internal representation.
        let private parseContinuousMode str =
            match String.toUpper str with
            | "FREE" -> FreeRun
            | "TRIG" | "TRIGGER" -> TriggerThenRun
            | "RES" | "RESET" -> ResetOnTrigger
            | _ -> failwithf "Unexpected ARB continuous mode trigger type string: %s" str

        /// Convert an internal representation of the single trigger retrigger mode of the dual
        /// ARB trigger system into a machine representation.
        let private retriggerModeString = function
            | NoRetrigger -> "OFF"
            | BufferedRetrigger -> "ON"
            | RestartRetrigger -> "IMMEDIATE"

        /// Covnert a machine representaiton of the single trigger retrigger mode of the dual ARB
        /// trigger system into an internal representation.
        let private parseRetriggerMode str =
            match String.toUpper str with
            | "ON"  | "1" -> BufferedRetrigger
            | "OFF" | "0" -> NoRetrigger
            | "IMM" | "IMMEDIATE" -> RestartRetrigger
            | _ -> failwithf "Unexpected dual ARB retrigger string: %s" str

        /// Convert an internal representation of the segment advance type mode of the dual ARB
        /// triggering system into a machine representation.
        let private segmentAdvanceModeString = function
            | SinglePlay -> "SINGLE"
            | ContinuousPlay -> "CONTINUOUS"

        /// Convert a machine representation of the segment advance type mode of the dual ARB
        /// triggering system into a machine representation.
        let private parseSegmentAdvanceMode str =
            match String.toUpper str with
            | "SING" | "SINGLE" -> ArbSegmentAdvanceMode.SinglePlay
            | "CONT" | "CONTINUOUS" -> ContinuousPlay
            | _ -> failwithf "Unexpected ARB segment advance mode trigger type string: %s" str

        /// Convert an internal representation of the physical location of an external dual ARB
        /// trigger into a machine representation.
        let private externalConnectorString = function
            | ArbBnc -> "EPT1"
            | ArbAux -> "EPT2"

        /// Convert a machine representation of the physical location of an external dual ARB
        /// trigger into an internal representation.
        let private parseExternalConnector str =
            match String.toUpper str with
            | "EPT1" | "EPTRIGGER1" -> ArbBnc
            | "EPT2" | "EPTRIGGER2" -> ArbAux
            | _ -> failwithf "Unexpected ARB external trigger source location string: %s" str

        /// Set the type of the ARB trigger to the given type.
        let private setModeType = IO.setValueString modeTypeString modeTypeKey

        /// Set the mode of the dual ARB continuous trigger.
        let private setContinuousMode = IO.setValueString continuousModeString continuousModeKey

        /// Set the number of repeats per point in the dual ARB single trigger mode.
        let private setSingleRepeats = IO.setUint16 singleRepeatsKey

        /// Set the retrigger mode of the single trigger setting of the dual ARB.
        let private setRetriggerMode = IO.setValueString retriggerModeString retriggerModeKey

        /// Set the polarity of the gating trigger in the dual ARB system.
        let private setGatePolarity = IO.setLowHighState gatePolarityKey

        /// Set the mode of the segment advance trigger of the dual ARB system.
        let private setSegmentAdvanceMode =
            IO.setValueString segmentAdvanceModeString segmentAdvanceModeKey

        /// Set the dual ARB trigger mode to have the value given.
        let internal setMode instrument mode = async {
            do! setModeType instrument mode
            match mode with
            | ArbContinuous mode'     -> do! setContinuousMode instrument mode'
            | ArbSingle (reps, retrigger) ->
                do! setSingleRepeats instrument reps
                do! setRetriggerMode instrument retrigger
            | ArbGate polarity        -> do! setGatePolarity instrument polarity
            | ArbSegmentAdvance mode' -> do! setSegmentAdvanceMode instrument mode' }

        /// Query the currently set value of the dual ARB system triggering.
        let private queryMode instrument = async {
            let helper str =
                match String.toUpper str with
                    | "CONT" | "CONTINUOUS" ->
                        IO.queryKeyString parseContinuousMode continuousModeKey instrument
                        |> Async.map ArbContinuous
                    | "SING" | "SINGLE" ->
                        let reps = IO.queryUint16 singleRepeatsKey instrument
                        let retrigger = IO.queryKeyString parseRetriggerMode retriggerModeKey instrument
                        Async.map2 (fun a b -> ArbSingle (a, b)) reps retrigger
                    | "GATE" ->
                        IO.queryLowHighState gatePolarityKey instrument
                        |> Async.map ArbGate
                    | "SADV" | "SADVANCE" ->
                        IO.queryKeyString parseSegmentAdvanceMode segmentAdvanceModeKey instrument
                        |> Async.map ArbSegmentAdvance
                    | str -> failwithf "Unexpected ARB trigger type string: %s" str
            let! triggerType = IO.queryKeyString (fun str -> str) modeTypeKey instrument
            return! helper triggerType }

        /// Set the type of the source of the dual ARB triggering system.
        let private setSourceType =
            IO.setValueString sourceTypeString sourceTypeKey

        /// Set the physical location of the external trigger for the dual ARB system.
        let private setSourceConnector =
            IO.setValueString externalConnectorString sourceLocationKey

        /// Set the polarity of the dual ARB external trigger source.
        let private setSourcePolarity instrument polarity = async {
            match polarity with
            | Some p -> do! IO.setPolarity polarityKey instrument p
            | None -> () }

        /// Set the delay of the dual ARB trigger system for external trigges.
        let private setSourceDelay instrument delay = async {
            match delay with
            | Some d ->
                do! IO.setDuration delayKey instrument d
                do! IO.setOnOffState delayStateKey instrument On
            | None ->
                do! IO.setOnOffState delayStateKey instrument Off }

        /// Set the dual ARB trigger source.
        let internal setSource instrument src = async {
            match src with
            | Some source ->
                do! setSourceType instrument source
                match source with
                | ArbExternal (connector, polarity, delay) ->
                    do! setSourceConnector instrument connector
                    do! setSourcePolarity instrument polarity
                    do! setSourceDelay instrument delay
                | _ -> ()
            | None -> () }

        /// Query the source of the dual ARB's triggering system.
        let private querySource instrument mode = async {
            let! sourceType = IO.queryKeyString (fun str -> str) sourceTypeKey instrument
            match String.toUpper sourceType with
            | "KEY" -> return Some ArbKey
            | "BUS" -> return Some ArbBus
            | "EXT" ->
                let! connector = IO.queryKeyString parseExternalConnector sourceLocationKey instrument
                let! polarity =
                    match mode with
                    | ArbGate _ -> async { return None }
                    | _ -> Async.map Some <| IO.queryPolarity polarityKey instrument
                let! state = IO.queryOnOffState delayStateKey instrument
                let! delay =
                    match state with
                    | On -> Async.map Some <| IO.queryDuration delayKey instrument
                    | Off -> async { return None }
                return Some <| ArbExternal (connector, polarity, delay)
            | _ -> return raise << UnexpectedReply <| sprintf "Unexpected ARB trigger source string: %s" sourceType }

        /// Completely set the dual ARB system's trigger.
        let set instrument (ArbTrigger (mode, source)) = async {
            do! setMode instrument mode
            do! setSource instrument source }

        /// Query the complete settings of the dual ARB's current trigger.
        let query instrument = async {
            let! mode = queryMode instrument
            let! source = querySource instrument mode
            return ArbTrigger (mode, source) }

    module internal Encode =
        /// Create a tuple of iq, markers encoded as byte sequences.
        let toEncodedSegmentData (segment : Segment) =
            let sampleCount = int segment.SegmentLength
            let iq = Array.create (sampleCount * 4) 0uy
            let markers = Array.create sampleCount 0uy
            let mutable sampleIndex = 0
            let mutable used = 0u
            let singleIq = Array.create 4 0uy
            let mutable singleMarkers = 0uy
            for i in 0 .. (sampleCount - 1) do
                let (sample, SampleCount count) = segment.SegmentSamples.[sampleIndex]
                if used = 0u then
                    singleIq.[0 .. 3] <- Sample.iqBytes sample
                    singleMarkers     <- Markers.toByte (Sample.markers sample)
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
        let segment id segment =
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
            (asciiString <| waveformIdFilename el, asciiString reps)

        /// Encode a sequence element into the form "\"<filename>\",<reps>,<markers>"B.
        let private toEncodedSequenceElement (element : SequenceElement) =
            sprintf "%s,%u,ALL" (waveformIdFilename <| fst element) (snd element)

        /// Convert a sequence into an ASCII string of its elements.
        let internal sequenceData (SequenceType sequence) =
            sequence
            |> List.map toEncodedSequenceElement
            |> List.map (sprintf ",%s")
            |> String.concat ""

        /// Encode a whole sequence in an EncodedSequence.
        let sequence id sequence =
            sprintf "%s%s" (sequenceFileString id) (sequenceData sequence)

    module internal Decode =
        /// Convert a big-endian array of bytes into the host order.
        let private toHostOrder bytes =
            if BitConverter.IsLittleEndian then
                bytes |> Array.rev
            else
                bytes

        /// Decompress the markers back into a 4-tuple of the 4 Boolean markers.
        let private getMarkers markers =
            { M1 = Convert.ToBoolean(markers &&& 0x1uy)
              M2 = Convert.ToBoolean(markers &&& 0x2uy)
              M3 = Convert.ToBoolean(markers &&& 0x4uy)
              M4 = Convert.ToBoolean(markers &&& 0x8uy) }

        /// Decode an encoded sample back into the internal representation of a sample.
        let parseSample i q markers = { I = i; Q = q; SampleMarkers = markers }

        /// Get only the interesting bits of the datablock, removing the "#", the number of
        /// digits, and the data length.
        let private stripMetadata (data : byte array) =
            /// datablock is of form "#<digits><length><data>"
            data.[2 .. (Array.length data - 1)]

        /// Parse a waveform file into a tuple of i and q data arrays.
        let parseWaveformFile (data : byte array) =
            let data' = data |> stripMetadata
            let numSamples = (Array.length data') / 4
            let i = Array.create numSamples 0s
            let q = Array.create numSamples 0s
            let rec loop = function
                | index when index = numSamples -> (i, q)
                | index ->
                    i.[index] <-
                        BitConverter.ToInt16(toHostOrder data'.[(4 * index)     .. (4 * index + 1)], 0)
                    q.[index] <-
                        BitConverter.ToInt16(toHostOrder data'.[(4 * index + 2) .. (4 * index + 3)], 0)
                    loop (index + 1)
            loop 0

        /// Parse a marker file into an array of markers.
        let parseMarkerFile (data : byte array) =
            let data' = data |> stripMetadata
            let numSamples = (Array.length data')
            let markers = Array.create numSamples Unchecked.defaultof<Markers>
            let rec loop = function
                | index when index = numSamples -> markers
                | index ->
                    markers.[index] <- getMarkers data'.[index]
                    loop (index + 1)
            loop 0

#if DEBUG
    [<AutoOpen>]
    module internal Print =
        /// Depth of an indent.
        let indentDepth = 4

        /// Get a string of the indent level.
        let private getIndent indent = String.replicate indent " "

        /// Pretty-print out a sample.
        let printSample (indent : int) sample =
            printf "%s(%6d; %6d; %d%d%d%d)"
                (getIndent indent)
                sample.I
                sample.Q
                (Convert.ToInt32 sample.SampleMarkers.M1)
                (Convert.ToInt32 sample.SampleMarkers.M2)
                (Convert.ToInt32 sample.SampleMarkers.M3)
                (Convert.ToInt32 sample.SampleMarkers.M4)

        /// Print out a (Sample * SampleCount) tuple.
        let printSampleCount indent (smp, SampleCount count) =
            printSample indent smp
            printfn " * %d" count

        /// Pretty-print out a segment.
        let printSegment indent segment =
            segment.SegmentSamples
            |> Array.iter (printSampleCount indent)

        /// Pretty print a pending sequence.
        let rec printSequence indent segMap seqMap (SequenceType sequence) =
            let printEl = printSequenceElement indent segMap seqMap
            sequence |> List.iter printEl
        and printSequenceElement indent segMap seqMap (id, reps) =
            match id with
            | SegmentId id ->
                printfn "%s%s * %d" (getIndent indent) id reps
                printSegment (indent + indentDepth) (Map.find id segMap)
            | SequenceId id ->
                printfn "%s%s * %d" (getIndent indent) id reps
                printSequence (indent + indentDepth) segMap seqMap (Map.find id seqMap)
#endif