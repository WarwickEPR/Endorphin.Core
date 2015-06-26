namespace Endorphin.Instrument.Keysight

open System

module Endian =
    (* Need these helper functions to easily specify the overload to BitConverter.GetBytes while keeping F#'s
     * type system intact. *)
    let private int16ToByte (number : int16) =
        number |> BitConverter.GetBytes
    let private int32ToByte (number : int) =
        number |> BitConverter.GetBytes
    let private int64ToByte (number : int64) =
        number |> BitConverter.GetBytes

    // Functions to convert bytes into the various types using the different available BitConverter methods
    let private byteToInt16 byteArray =
        BitConverter.ToInt16 (byteArray, 0)
    let private byteToInt32 byteArray =
        BitConverter.ToInt32 (byteArray, 0)
    let private byteToInt64 byteArray =
        BitConverter.ToInt64 (byteArray, 0)

    let private swapEndian convertToByte convertToType number =
        number
        |> convertToByte
        |> Array.rev // Use Array.rev instead of Array.Reverse because the latter is in place
        |> convertToType

    /// Swap endianness of a 16-bit integer
    let swapEndian16 = swapEndian int16ToByte byteToInt16
    /// Swap endianness of a 32-bit integer
    let swapEndian32 = swapEndian int32ToByte byteToInt32
    /// Swap endianness of a 64-bit integer
    let swapEndian64 = swapEndian int64ToByte byteToInt64

    // Helper function to convert to a specific endianness
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
    let toLittleEndian16 = toLittleEndian swapEndian16
    /// Convert 32-bit integer to little-endian
    let toLittleEndian32 = toLittleEndian swapEndian32
    /// Convert 64-bit integer to little-endian
    let toLittleEndian64 = toLittleEndian swapEndian64

    /// Convert 16-bit integer to big-endian
    let toBigEndian16 = toBigEndian swapEndian16
    /// Convert 32-bit integer to big-endian
    let toBigEndian32 = toBigEndian swapEndian32
    /// Convert 64-bit integer to big-endian
    let toBigEndian64 = toBigEndian swapEndian64