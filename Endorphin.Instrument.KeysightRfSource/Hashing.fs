﻿namespace Endorphin.Instrument.Keysight

open Endorphin.Core.CRC

module internal Hashing =
    /// Create a hash of a byte array as a hexadecimal string 16 characters long.
    let private hexHashString arr = sprintf "%016x" (crc64 arr)

    /// Get a hash of the given type, by converting it to a byte array using the given function
    /// then using some hash function on the result to get an answer.
    let hexHash toByteArray value =
        value
        |> toByteArray
        |> hexHashString