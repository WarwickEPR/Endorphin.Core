namespace Endorphin.Instrument.LakeShoreTempController

[<RequireQualifiedAccess>]
/// VISA command keys for instrument.
module internal Keys =
    let identity = "*IDN"
    let standardEventStatus = "*ESE"
    let controlMode = "CMODE"
    let heaterOutput = "HTR"
    let manualHeaterOuptut = "MOUT"
    let pid = "PID"
    let setPoint = "SETP"
    let temperature = "TEMP"