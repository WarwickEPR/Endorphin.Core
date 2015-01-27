namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net

type SetFieldStatus =
    | PreparingToSetField
    | FinishedSettingField
    | CanceledSettingField of returnToZero : bool
    | FailedSettingField
            
type SetFieldCancellationOptions =
    { ReturnToZero : bool }

type StaticFieldWorker(magnetController : MagnetController, targetFieldIndex) =
    static let log = LogManager.GetLogger typeof<StaticFieldWorker>

    let statusChanged = new Event<SetFieldStatus>()
    let success = new Event<unit>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    
    let cancellationCapability =
        new CancellationCapability<SetFieldCancellationOptions>()
    
    let targetCurrent =
        magnetController.DeviceParameters.CurrentForIndex targetFieldIndex

    do // initialisation checks
        if abs targetCurrent > magnetController.DeviceParameters.CurrentLimit then
            failwith "Target current outside of magnet controller current limit."
    
    member __.StatusChanged = statusChanged.Publish
    member __.Success = success.Publish
    member __.Error = error.Publish
    member __.Canceled = canceled.Publish

    member __.Cancel returnToZero =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel { ReturnToZero = returnToZero }

    member this.Start() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare ramp as cancellation was already requested."

        "Static field worker preparing..." |> log.Info
        sprintf "Target field index: %d." targetFieldIndex |> log.Info

        let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

        let workflow = async {
            // define workflows which will be used to set the field
            let setStartingCurrentDirection = async {
                "Setting target current direction." |> log.Info
                syncContext.RaiseEvent statusChanged PreparingToSetField
                let! operatingParameters = magnetController.GetOperatingParametersAsync()
                if targetFieldIndex <> 0 then
                    let targetCurrentDirection = if targetFieldIndex > 0 then Forward else Reverse
                    if operatingParameters.CurrentDirection <> targetCurrentDirection then 
                        do! magnetController.RampToZeroAndSetCurrentDirectionAsync targetCurrentDirection }
            
            let setCurrentLimits = async { 
                "Setting current limits." |> log.Info
                magnetController.SetPause true 
                magnetController.SetLowerSetPointByIndex 0
                magnetController.SetUpperSetPointByIndex targetFieldIndex }

            let rampToTarget = async {
                "Ramping to target current." |> log.Info
                magnetController.SetRampTarget Upper
                magnetController.SetRampRate magnetController.DeviceParameters.RampRateLimit
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            // set up cancellation handler
            use! __ = Async.OnCancel(fun () ->
                "Cancelling ramp to target field..." |> log.Info
                if cancellationCapability.Options.ReturnToZero then 
                    "Returning to zero current..." |> log.Info
                    magnetController.BeginRampToZero()
                else 
                    "Not requested to return to zero current. Pausing magnet controller..." |> log.Info
                    magnetController.SetPause true

                "Canceled setting field." |> log.Info
                syncContext.RaiseEvent statusChanged (CanceledSettingField cancellationCapability.Options.ReturnToZero))
            
            // ramp to target
            do! setStartingCurrentDirection
            do! setCurrentLimits
            do! rampToTarget
            
            "Successfully reached target field." |> log.Info
            syncContext.RaiseEvent statusChanged FinishedSettingField }
        
        "Starting set field workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> syncContext.RaiseEvent success ()),
            (fun exn -> // error
                log.Error (sprintf "Failed to set field due to error: %A." exn, exn)
                syncContext.RaiseEvent statusChanged FailedSettingField
                syncContext.RaiseEvent error exn),
            (fun exn -> syncContext.RaiseEvent canceled exn),
            cancellationCapability.Token)