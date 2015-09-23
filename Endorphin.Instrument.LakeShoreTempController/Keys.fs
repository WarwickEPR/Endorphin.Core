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