namespace Endorphin.Instrument.Keysight

open Endorphin.Instrument.Keysight.Endian
open System
open System.Text

module IQData =
    /// Basic data form of IQ point
    let defaultIQPoint = {
        Point.I = 0s;
        Point.Q = 0s;
        Point.Marker1 = false;
        Point.Marker2 = false;
        Point.Marker3 = false;
        Point.Marker4 = false;
        Order = LittleEndian }
    /// Set value of the I point
    let withIPoint  value point = { point with I = value }
    /// Set value of the Q point
    let withQPoint  value point = { point with Q = value }
    /// Set value of the first marker
    let withMarker1 value point = { point with Marker1 = value }
    /// Set value of the second marker
    let withMarker2 value point = { point with Marker2 = value }
    /// Set value of the third marker
    let withMarker3 value point = { point with Marker3 = value }
    /// Set value of the fourth marker
    let withMarker4 value point = { point with Marker4 = value }
    /// Set the order to interpret the bytes of the I and Q points
    let private withOrder value point = { point with Order = value }

    /// Swap endianness of an IQ point
    let private swapEndian point =
        point
        |> withIPoint (swapEndianInt16 point.I)
        |> withQPoint (swapEndianInt16 point.Q)

    /// Convert point to littleendian order regardless of beginning order
    let internal toLittleEndian point =
        if point.Order = LittleEndian then
            point
        else
            point
            |> withOrder LittleEndian
            |> swapEndian

    /// Convert point to bigendian order regardless of beginning order
    let internal toBigEndian point =
        if point.Order = BigEndian then
            point
        else
            point
            |> withOrder BigEndian
            |> swapEndian

    /// Make a marker byte out of the booleans in an IQ point
    let internal getMarkerByte point =
        ((Convert.ToByte point.Marker4) <<< 3) ||| ((Convert.ToByte point.Marker3) <<< 2) ||| ((Convert.ToByte point.Marker2) <<< 1) ||| (Convert.ToByte point.Marker1)

    let private flattenListOfArrays list = list |> List.reduce Array.append

    /// Encode a single point into its IQ data and marker data as a tuple in the order (IQ [] * Markers)
    let internal encodePoint point =
        if point.Order = LittleEndian then
            failwith "Cannot write a littleendian point to the machine"
        else
            ( (Array.append (BitConverter.GetBytes point.I) (BitConverter.GetBytes point.Q)),
              (getMarkerByte point) )

    /// Encode a sample into IQ data and marker data returned as a tuple in the order (IQ [] * Markers [])
    let internal encodeSample sample =
        // Encode individual points
        let (arrays, markers) =
            sample
            |> Array.map encodePoint
            |> Array.unzip
        // Flatten the array of IQ arrays
        let flatArray = arrays |> Array.reduce Array.append
        (flatArray, markers)

    // This function is currently far too long - perhaps should take its helper functions outside of its body to simplify
    /// Encode each waveform element into a byte array for writing
    let private encodeWaveformElement outerElement =
        let rec encode element acc =
            /// Helper function to handle repeats of single samples
            let rec sampleReps encodedSample acc reps =
                match reps with
                | 0us -> acc
                | _   -> sampleReps encodedSample (encodedSample :: acc) (reps - 1us)
            /// Helper function to (badly) handle repeats of elements
            let rec elementReps encodedElement acc reps =
                match reps with
                | 0us -> acc
                | _   ->
                    let newAcc = List.append encodedElement acc
                    elementReps encodedElement newAcc (reps - 1us)
            match element with
            | Sample (smp, reps) -> sampleReps (encodeSample smp) acc reps
            | Element (nextElement, reps) -> elementReps (encode nextElement acc) acc reps // NOT TAIL RECURSIVE
            // TODO: Need to find a way to do this without causing a stack overflow on a
            // deeply recursive type

        // Enter the encoding function, and flatten out the tuple
        let (IQ, markers) =
            encode outerElement []
            |> List.rev
            |> List.unzip
        ( flattenListOfArrays IQ, flattenListOfArrays markers )

    /// Encode a whole waveform into the required byte arrays
    let internal encodeWaveform (waveform : Waveform) =
        let (encodedIQ, encodedMarkers) =
            waveform.Elements
            |> List.map encodeWaveformElement
            |> List.unzip
        { Name = waveform.Name
          IQ = flattenListOfArrays encodedIQ
          Markers = flattenListOfArrays encodedMarkers }

    /// Make the data string, including the '#' character, the digits of length, the length and the data
    let private makeDataString (data : byte []) =
        let length = data.Length
        let digits = length.ToString().Length
        // TODO: this could probably be a choice function
        if digits >= 10 then
            failwith "Can't write that much data to the instrument in one go!"
        Array.concat
          [ "#"B
            Encoding.ASCII.GetBytes(digits.ToString())
            Encoding.ASCII.GetBytes(length.ToString()) ]

    /// Produce the full data strings necessary for writing the three different files to the machine,
    /// given the encoded waveform to extract the data from.
    let internal encodeWaveformFile (waveform : EncodedWaveform) =
        { WaveformFileString = Array.concat
            [ "\"WFM1:"B
              waveform.Name
              "\","B
              makeDataString waveform.IQ ]
          MarkerFileString = Array.concat
            [ "\"MKR1:"B
              waveform.Name
              "\",#"B
              makeDataString waveform.Markers ]
          HeaderFileString = Array.concat
            [ "\"HDR1:"B
              waveform.Name
              "\""B ] } // TODO: fix the header string