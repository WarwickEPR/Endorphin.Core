namespace Endorphin.Instrument.Keysight

open System

// This should possibly be in Endorphin.Core rather than in a specific instrument

module Endian =
    (* Need these helper functions to easily specify the overload to BitConverter.GetBytes while keeping F#'s
     * type system intact. *)
    let private int16ToByte (number : int16) =
        number |> BitConverter.GetBytes
    let private int32ToByte (number : int) =
        number |> BitConverter.GetBytes
    let private int64ToByte (number : int64) =
        number |> BitConverter.GetBytes
    let private float32ToByte (number : single) =
        number |> BitConverter.GetBytes
    let private floatToByte (number : double) =
        number |> BitConverter.GetBytes

    // Functions to convert bytes into the various types using the different available BitConverter methods
    let private byteToInt16 byteArray =
        BitConverter.ToInt16 (byteArray, 0)
    let private byteToInt32 byteArray =
        BitConverter.ToInt32 (byteArray, 0)
    let private byteToInt64 byteArray =
        BitConverter.ToInt64 (byteArray, 0)
    let private byteToFloat32 byteArray =
        BitConverter.ToSingle (byteArray, 0)
    let private byteToFloat byteArray =
        BitConverter.ToDouble (byteArray, 0)

    /// Swap endianness of any of the primitive numerical types except 8-bit
    let private swapEndian convertToByte convertToType number =
        number
        |> convertToByte
        |> Array.rev // Use Array.rev instead of Array.Reverse because the latter is in place
        |> convertToType

    /// Swap endianness of a 16-bit integer
    let internal swapEndianInt16 = swapEndian int16ToByte byteToInt16
    /// Swap endianness of a 32-bit integer
    let internal swapEndianInt32 = swapEndian int32ToByte byteToInt32
    /// Swap endianness of a 64-bit integer
    let internal swapEndianInt64 = swapEndian int64ToByte byteToInt64
    /// Swap endianness of a single-precision float
    let internal swapEndianFloat32 = swapEndian float32ToByte byteToFloat32
    /// Swap endianness of a double-precision float
    let internal swapEndianFloat = swapEndian floatToByte byteToFloat

    /// Change any primitive numerical type to littleendian
    let private toLittleEndian swapEndianType number =
        if (BitConverter.IsLittleEndian) then
            number
        else
            swapEndianType number
    let private toBigEndian swapEndianType number =
        if (BitConverter.IsLittleEndian) then
            swapEndianType number
        else
            number

    /// Convert 16-bit integer to little-endian
    let internal toLittleEndianInt16 = toLittleEndian swapEndianInt16
    /// Convert 32-bit integer to little-endian
    let internal toLittleEndianInt32 = toLittleEndian swapEndianInt32
    /// Convert 64-bit integer to little-endian
    let internal toLittleEndianInt64 = toLittleEndian swapEndianInt64
    /// Convert single-precision float to little-endian
    let internal toLittleEndianFloat32 = toLittleEndian swapEndianFloat32
    /// Convert double-preicision float to little-endian
    let internal toLittleEndianFloat = toLittleEndian swapEndianFloat

    /// Convert 16-bit integer to big-endian
    let internal toBigEndianInt16 = toBigEndian swapEndianInt16
    /// Convert 32-bit integer to big-endian
    let internal toBigEndianInt32 = toBigEndian swapEndianInt32
    /// Convert 64-bit integer to big-endian
    let internal toBigEndianInt64 = toBigEndian swapEndianInt64
    /// Convert single-precision float to big-endian
    let internal toBigEndianFloat32 = toBigEndian swapEndianFloat32
    /// Convert double-precision float to big-endian
    let internal toBigEndianFloat = toBigEndian swapEndianFloat