// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.LakeShoreTempController

[<RequireQualifiedAccess>]
/// VISA command keys for instrument.
module internal Keys =
    /// Device identity.
    let identity = "*IDN"
    
    /// Standard event status byte.
    let standardEventStatus = "*ESR"

    /// Control loop mode.
    let controlMode = "CMODE"

    /// Current heater output.
    let heaterOutput = "HTR"

    /// Manual heater output setting.
    let manualHeaterOuptut = "MOUT"

    /// PID settings.
    let pidSettings = "PID"

    /// Temperature set point.
    let setPoint = "SETP"

    /// Current temperature.
    let temperature = "KRDG"