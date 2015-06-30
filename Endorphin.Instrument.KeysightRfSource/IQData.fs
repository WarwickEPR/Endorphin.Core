﻿namespace Endorphin.Instrument.Keysight

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

    /// Predefined empty type for EncodedList
    let private emptyEncodedList = { EncodedList.IQ = []; EncodedList.Markers = [] }

    /// Add a single encoded point onto the encoded sequence
    let private addEncodedPoint (total : EncodedList) (point : EncodedPoint) =
        { EncodedList.IQ      = point.IQ :: total.IQ
          EncodedList.Markers = point.Markers :: total.Markers }

    /// Add a list of encoded points onto another
    let private addEncodedList (total : EncodedList) (list : EncodedList) =
        { EncodedList.IQ      = List.append list.IQ total.IQ
          EncodedList.Markers = List.append list.Markers total.Markers }

    /// Handle the necessary number of repetitions for previously encoded samples, sequences and
    /// other lists of points.
    let private copyEncodedList (list : EncodedList) reps =
        let rec loop list acc reps =
            match reps with
            | 0us -> acc
            | _   -> loop list (addEncodedList acc list) (reps - 1us)
        loop list emptyEncodedList reps

    /// Encode a single point into its IQ data and marker data as an EncodedPoint
    let private encodePoint point =
        if point.Order = LittleEndian then
            failwith "Cannot write a littleendian point to the machine"
        else
            { EncodedPoint.IQ = Array.append (BitConverter.GetBytes point.I) (BitConverter.GetBytes point.Q)
              EncodedPoint.Markers = getMarkerByte point }

    /// Encode a sample into a whole EncodedList
    let private encodeSample sample =
        sample
        |> Array.map encodePoint
        |> Array.fold addEncodedPoint emptyEncodedList

    /// Encode a sequence of samples into an EncodedList
    let private encodeSequence sequence =
        sequence
        |> Array.map encodeSample
        |> Array.fold addEncodedList emptyEncodedList

    /// Encode a waveform element and handle its repeats
    let private encodeWaveformElement element =
        match element with
        | Sample (sample, reps) -> copyEncodedList (encodeSample sample) reps
        | Sequence (sequence, reps) -> copyEncodedList (encodeSequence sequence) reps

    /// Helper function to separate the (element, reps) tuples out into a pipeable form
    let private encodeWaveformElementReps (element, reps) =
        copyEncodedList (encodeWaveformElement element) reps

    /// Put the encoded list into the correct order, since it's built up in reverse for speed
    let private reverseEncodedList (list : EncodedList) =
        { EncodedList.IQ = List.rev list.IQ
          EncodedList.Markers = List.rev list.Markers }

    /// Flatten out the tupled lists (iq is a list of arrays) into two arrays in the form
    /// (iq [], markers [])
    let private flattenEncodedList (list : EncodedList) =
        let outIQ      = list.IQ      |> List.fold Array.append [||]
        let outMarkers = list.Markers |> List.toArray
        (outIQ, outMarkers)

    /// Encode a whole waveform and handle any repetitions of elements
    let encodeWaveform (waveform : Waveform) =
        let (iq, markers) =
            waveform.Elements
            |> List.map encodeWaveformElementReps
            |> List.fold addEncodedList emptyEncodedList
            |> reverseEncodedList
            |> flattenEncodedList
        { EncodedWaveform.Name = waveform.Name
          EncodedWaveform.IQ   = iq
          EncodedWaveform.Markers = markers }

    /// Make the data string, including the '#' character, the digits of length, the length and the data
    let private makeDataString (data : byte []) =
        let length = data.Length
        let digits = length.ToString().Length
        if digits >= 10 then
            failwith "Can't write that much data to the instrument in one go!"
        Array.concat [
            "#"B
            Encoding.ASCII.GetBytes(digits.ToString())
            Encoding.ASCII.GetBytes(length.ToString()) ]

    let private waveformFolder = "WFM1:"B
    let private markerFolder = "MKR1:"B
    let private headerFolder = "HDR1:"B

    let private fileNameString folder name = Array.concat ["\""B; folder; name; "\""B]

    let private waveformFileString name = fileNameString waveformFolder name
    let private markerFileString name = fileNameString markerFolder name
    let private headerFileString name = fileNameString headerFolder name

    let private dataStorageString fileName data = Array.concat [fileName; ","B; data]

    /// Produce the full data strings necessary for writing the three different files to the machine,
    /// given the encoded waveform to extract the data from.
    let internal encodeWaveformFile (waveform : EncodedWaveform) =
        { WaveformFileString = dataStorageString (waveformFileString waveform.Name) waveform.IQ
          MarkerFileString   = dataStorageString (markerFileString   waveform.Name) waveform.Markers
          HeaderFileString   = dataStorageString (headerFileString   waveform.Name) ""B }