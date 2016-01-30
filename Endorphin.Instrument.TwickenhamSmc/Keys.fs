// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.TwickenhamSmc

[<RequireQualifiedAccess>]
/// VISA command keys for instrument.
module internal Keys =
    
    /// Device output parameters query key.
    let outputParameters = "G"

    /// Device current parameters query key.
    let currentParameters = "K"
    
    /// Device operating parameters query key.
    let operatingParameters = "O"

    /// Device set-point parameters query key.
    let setPointParameters = "S"

    /// Ramp rate key.
    let rampRate = "A"

    /// Trip voltage key.
    let tripVoltage = "Y"

    /// Current direction key.
    let currentDirection = "D"

    /// Lower set-point key.
    let lowerSetPoint = "L"

    /// Upper set-point key.
    let upperSetPoint = "U"

    /// Ramp target key.
    let rampTarget = "R"

    /// Pause key.
    let pause = "P"