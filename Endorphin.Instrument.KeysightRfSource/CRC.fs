namespace Endorphin.Instrument.Keysight

/// Functions for calculating the CRC-64 of arrays of bytes.
/// Potentially deprecated
module CRC =
    /// Get the table entry for a given index in the 64-bit CRC lookup table of the
    /// given polynomial
    let private getCrc64TableEntry i polynomial =
        let rec loop j value =
            if j = 8 then value
            else
                if (value &&& 1uL) = 1uL then loop (j+1) ((value >>> 1) ^^^ polynomial)
                else loop (j+1) (value >>> 1)
        loop 0 (uint64 i)

    /// Create a CRC lookup table for the given polynomial
    let private createCrc64Table polynomial =
        let table = Array.create 256 0uL
        for i in 0 .. 255 do
            table.[i] <- getCrc64TableEntry i polynomial
        table

    /// A CRC-64 table using a magic ECMA-standard polynomial
    let table = createCrc64Table 0x42F0E1EBA9EA3693uL

    /// Calculate the CRC-64 of an array of bytes using the magic ECMA standardised polynomial
    let crc64 (bytes : byte []) =
        let rec loop crc i =
            if i = bytes.Length then crc
            else
                let j = bytes.[i] ^^^ (byte crc)
                let crc' = (crc >>> 8) ^^^ table.[int j]
                loop crc' (i+1)
        loop 0uL 0