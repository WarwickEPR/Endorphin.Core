// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
/// Byte-processing utilities
module Byte =
    /// Returns a boolean indicating whether the n-th bit of a byte is high.
    let nthBit n byte =
        (1uy <<< n) &&& byte <> 0uy
