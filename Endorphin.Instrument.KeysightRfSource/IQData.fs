namespace Endorphin.Instrument.Keysight

open Endorphin.Instrument.Keysight.Endian
open System

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