namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open Endorphin.Utilities.Position.Path
open Endorphin.Utilities.Position.Point
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

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
        do! (sprintf "%s\n" (stringFunc value)) |> Visa.String.query scanningController |> Async.Ignore }

    let private writeCommand (ScanningController (scanningController, _)) command = 
        (sprintf "%s\n" command) |> Visa.String.write scanningController

    let private writePathVoltage (ScanningController (scanningController, _)) (voltages : VoltagePoint) =
        (sprintf "V%.4M,%.4M,%.4M\n" (tfst voltages / 1m<V>) (tsnd voltages / 1m<V>) (ttrd voltages / 1m<V>)) |> Visa.String.write scanningController

    let private readUploadAcknowledgement parseFunc (ScanningController (scanningController, _)) = async {
        let! response = Visa.String.read scanningController
        return parseFunc response }

    let setDwell = setValue dwellString

    let setTriggerDelay = setValue triggerDelayString

    let getCurrentVoltages = queryValue parseVoltages "V?"

    let setCurrentVoltages = setValue voltageString

    let beginUpload numberOfElements = queryValue parseUploadAcknowledgement (uploadString numberOfElements)

    let finishUpload = readUploadAcknowledgement parseUploadCompletion

    let writePath (controller : ScanningController) (path : Path)  = async {
        do! beginUpload (Array.length (points path)) controller

        for point in points path do
            let voltages = pointToVoltage controller (path |> coordinateForPoint point)
            do writePathVoltage controller voltages
            
        do! finishUpload controller }

    let runPath (controller : ScanningController) = 
        writeCommand controller "RUN"

    let stopPath (controller : ScanningController) = 
        writeCommand controller "STOP"