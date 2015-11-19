namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open Endorphin.Utilities.Position.Path
open Endorphin.Utilities.Position.Point

module internal IO =
    /// Performs a query for the value corresponding to the given key and parses it with the given
    /// parsing function.
    let private queryValue parseFunc key (ScanningController (scanningController, _)) = async {
        let! _ = (sprintf "%s\n" key) |> Visa.String.query scanningController
        let! response = Visa.String.read scanningController
        return parseFunc response }
    
    /// Sets the value corresponding to the given key to the instrument after converting it to a
    /// string with the provided string conversion function.
    let private setValue stringFunc (ScanningController (scanningController, _)) value = async {
        //do! (sprintf "%s\n" (stringFunc value)) |> Visa.String.query scanningController |> Async.Ignore }
        (sprintf "%s\n" (stringFunc value)) |> Visa.String.write scanningController }

    let setDwell = setValue dwellString

    let setTriggerDelay = setValue triggerDelayString

    let getCurrentVoltages = queryValue parseVoltages "V?"

    let setCurrentVoltages = setValue voltageString

   (* let writePath (path : Path) (ScanningController (scanningController, calibration)) = async {
        let! _ = (sprintf "DL %d\n" (Array.length (points path))) |> (Visa.String.query scanningController)
        let! response = Visa.String.read scanningController
        
        if response <> "DL ACK" then
            failwith "Bad command to controller - will not acknowledge path upload"
        
        for point in points path do
            let coordinate = path |> coordinateForPoint point
            let voltages = Instrument.ScanningController.pointToVoltage coordinate calibration
            do! (sprintf "V%.4M,%.4M,%.4M\n" <| ((tfst voltages) (tsnd voltages) (ttrd voltages))) |> (Visa.String.write scanningController)}*)