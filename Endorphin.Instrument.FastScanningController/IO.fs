namespace Endorphin.Instrument.FastScanningController

open Endorphin.Core
open Endorphin.Utilities.Position.Path
open Endorphin.Utilities.Position.Point
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module internal IO =
    /// Performs a query for the value corresponding to the given key and parses it with the given
    /// parsing function.
    let private queryValue parseFunc key (ScanningController (scanningController, _)) = async {
        let! response = (sprintf "%s\n" key) |> Visa.String.query scanningController
        return parseFunc response }
    
    /// Sets the value corresponding to the given key to the instrument after converting it to a
    /// string with the provided string conversion function.
    let private setValue stringFunc (ScanningController (scanningController, _)) value = async {
        (sprintf "%s\n" (stringFunc value)) |> Visa.String.write scanningController }

    let private writeCommand (ScanningController (scanningController, _)) command = async {
        (sprintf "%s\n" command) |> Visa.String.write scanningController }

    let private writePathVoltage (ScanningController (scanningController, _)) (voltages : VoltagePoint) =
        (sprintf "%s%.4M,%.4M,%.4M\n" Keys.voltagePoint (tfst voltages / 1m<V>) (tsnd voltages / 1m<V>) (ttrd voltages / 1m<V>)) |> Visa.String.write scanningController

    let private readUploadAcknowledgement parseFunc (ScanningController (scanningController, _)) = async {
        let! response = Visa.String.read scanningController
        return parseFunc response }

    let setDwell = setValue dwellString

    let setTriggerDelay = setValue triggerDelayString

    let getCurrentVoltages = queryValue parseVoltages (sprintf "%s?" Keys.voltagePoint)

    let setCurrentVoltages = setValue voltageString

    let getNumberOfPoints = queryValue parseNumberOfPoints (sprintf "%s?" Keys.numberQuery) 

    let beginUpload numberOfElements = queryValue parseUploadAcknowledgement (uploadString numberOfElements)

    let finishUpload = readUploadAcknowledgement parseUploadCompletion

    let writePath (controller : ScanningController) (path : Path)  = async {
        let numPoints = Array.length (points path)
        do! beginUpload numPoints controller

        // Write each point to the controller. A periodic delay must be added because the 
        // write speed to EEPROM on the device is slower than the baud rate of the serial connection
        // and the buffer is only approximately 8k points
        let mutable i = 0
        for point in points path do
            i <- i + 1
            let voltages = pointToVoltage controller (path |> coordinateForPoint point)
            do writePathVoltage controller voltages
            if i % 500 = 0 then
                printfn "%d/%d written" i numPoints
                if numPoints > 8000 then
                    do! Async.Sleep 500
            
        do! finishUpload controller }

    let runPath (controller : ScanningController) = async {
        do! writeCommand controller <| sprintf "%s" Keys.runPath }

    let stopPath (controller : ScanningController) = async {
        do! writeCommand controller <| sprintf "%s" Keys.stopPath }