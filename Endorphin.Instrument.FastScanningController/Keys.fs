namespace Endorphin.Instrument.FastScanningController

[<RequireQualifiedAccess>]
/// Command keys for instrument.
module internal Keys =
    /// Dwell time
    let dwellTime = "DWELL"
    
    /// Delay time
    let delayTime = "DELAY"

    /// Number of points to upload to controller 
    let uploadNumber = "DL"

    /// Set or get a voltage
    let voltagePoint = "V"

    /// Number of points uploaded to controller
    let numberQuery = "N"

    /// Acknowledgement that path upload can begin
    let uploadAcknowledgement = "DL ACK"

    /// Acknowledgement that path upload has completed successfully
    let uploadComplete = "DL DONE"

    /// Run path
    let runPath = "RUN"

    /// Stop path
    let stopPath = "STOP"