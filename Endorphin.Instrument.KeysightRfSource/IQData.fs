﻿namespace Endorphin.Instrument.Keysight

open System

module IQData =
    /// Basic data form of IQ point
    let defaultIQSample = {
        Sample.I = 0s;
        Sample.Q = 0s;
        Sample.Marker1 = false;
        Sample.Marker2 = false;
        Sample.Marker3 = false;
        Sample.Marker4 = false; }
    /// Set value of the I sample
    let withISample value sample = { sample with I = value }
    /// Set value of the Q sample
    let withQSample value sample = { sample with Q = value }
    /// Set value of the first marker
    let withMarker1 value sample = { sample with Marker1 = value }
    /// Set value of the second marker
    let withMarker2 value sample = { sample with Marker2 = value }
    /// Set value of the third marker
    let withMarker3 value sample = { sample with Marker3 = value }
    /// Set value of the fourth marker
    let withMarker4 value sample = { sample with Marker4 = value }

    /// Make a marker byte out of the booleans in an IQ sample
    let internal getMarkerByte sample =
        ((Convert.ToByte sample.Marker4) <<< 3) ||| ((Convert.ToByte sample.Marker3) <<< 2) ||| ((Convert.ToByte sample.Marker2) <<< 1) ||| (Convert.ToByte sample.Marker1)

    /// Add a single encoded sample into an encoded waveform
    let private addEncodedSample (total : EncodedWaveform) (sample : EncodedSample) =
        { EncodedWaveform.Name    = total.Name
          EncodedWaveform.IQ      = sample.IQ :: total.IQ
          EncodedWaveform.Markers = sample.Markers :: total.Markers }

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
    let private encodeSample sample =
        { EncodedSample.IQ      = Array.append (toBytes sample.I) (toBytes sample.Q)
          EncodedSample.Markers = getMarkerByte sample }

    /// Encode a waveform into the necessary byte patterns
    let encodeWaveform (waveform : Waveform) =
        let emptyWaveform = { Name = waveform.Name; IQ = []; Markers = [] }
        waveform.Data
        |> Seq.map encodeSample
        |> Seq.fold addEncodedSample emptyWaveform

    /// Make the data string, including the '#' character, the digits of length, the length and the data
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

    /// Build up a full file name string for storing a file
    let private fileNameString folder name = Array.concat ["\""B; folder; name; "\""B]

    /// Total filename string for a waveform file
    let private waveformFileString name = fileNameString waveformFolder name
    /// Total filename string for a markers file
    let private markerFileString name = fileNameString markerFolder name
    /// Total filename string for a header file
    let private headerFileString name = fileNameString headerFolder name

    /// Build up a full string for data storage and location
    let private dataStorageString fileName dataString = Array.concat [fileName; ","B; dataString]

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

    /// Produce the full data strings necessary for writing the three different files to the machine,
    /// given the encoded waveform to extract the data from.
    let encodeWaveformFile (waveform : EncodedWaveform) =
        let waveformFileName = waveformFileString waveform.Name
        let markerFileName   = markerFileString   waveform.Name
        let headerFileName   = headerFileString   waveform.Name
        let waveformDataString =
            waveform.IQ
            |> reverseConcatenateToArray
            |> makeDataString
        let markerDataString =
            waveform.Markers
            |> reverseToArray
            |> makeDataString
        // TODO: fix header data string
        let headerDataString = "#10"B

        { WaveformFileString = dataStorageString  waveformFileName waveformDataString
          MarkerFileString   = dataStorageString  markerFileName   markerDataString
          HeaderFileString   = dataStorageString  headerFileName   headerDataString }