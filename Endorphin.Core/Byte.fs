namespace Endorphin.Core

[<RequireQualifiedAccess>]
/// Byte-processing utilities
module Byte = 
    
    /// Returns a boolean indicating whether the n-th bit of a byte is high.
    let nthBit n byte =
        (1uy <<< n) &&& byte <> 0uy