namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net

/// Indicates the status of a SetFieldWorker.
type SetFieldStatus =
    /// Indicates that the SetFieldWorker has begun ramping towards the target field.
    | SettingField
    /// Indicates that the SetFieldWorker has successfully set the target field.
    | FinishedSettingField
    /// Indicates that the SetFieldWorker workflow has been cancelled and whether the magnet controller was set to return to zero 
    /// current at the time of cancellation.
    | CanceledSettingField of exn : OperationCanceledException * returnToZero : bool
    /// Indicates that the SetFieldWorker has failed to set the requested field due to an error.
    | FailedSettingField of exn : exn

/// Specifies the cancellation options for a SetFieldWorker.
type SetFieldCancellationOptions =
    { /// Specifies whether the magnet controller should return to zero current when the SetFieldWorker workflow is canceled.
      ReturnToZero : bool }

/// Ramps the provided Twickenham magnet controller to the specified target field index at the maximum available ramp rate. The
/// target field index is given in terms of digital magnet controller output steps and is signed. Negative values correspond to
/// a reverse current polarity.
type SetFieldWorker(magnetController : MagnetController, targetFieldIndex) =
    static let log = LogManager.GetLogger typeof<SetFieldWorker> // logger

    // events
    let statusChanged = new Event<SetFieldStatus>()
    
    // cancellation capability which is used to ramping the magnet controller to the target current
    let cancellationCapability = new CancellationCapability<SetFieldCancellationOptions>()
    
    do // sanity checks
        // if the target current exceeds the magnet controller current limit, raise an excpetion
        if abs (magnetController.MagnetControllerParameters.CurrentForIndex targetFieldIndex) 
                > magnetController.MagnetControllerParameters.CurrentLimit then
            failwith "Target current outside of magnet controller current limit."
    
    /// Event fires when the SetFieldWorker status changes.
    member __.StatusChanged =
        Observable.CreateFromStatusEvent(
            statusChanged.Publish, // listen to the status event
            ((=) FinishedSettingField), // when the status indicates that the ramp has finished, raise the OnCompleted event on the observable
            (fun status -> 
                // if a status indicates that the stream has failed or was canceled, raise the OnError event
                match status with
                | FailedSettingField exn -> Some exn
                | CanceledSettingField (exn, _) -> Some (exn :> exn)
                | _ -> None ))

    /// Cancels setting the magnet controller to the target value with the specified cancellation options.
    member __.Cancel returnToZero =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel { ReturnToZero = returnToZero }

    /// Starts the workflow, ramping the magnet controller to the requested target output index.
    member __.Start() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare ramp as cancellation was already requested."

        "Static field worker preparing..." |> log.Info
        sprintf "Target field index: %d." targetFieldIndex |> log.Info

        // define the workflow to set the required output index
        let workflow = async {

            // define a workflow which will set the flip magnet controller current direction if this is required to reach the
            // target output index
            let setStartingCurrentDirection = async {
                "Setting target current direction." |> log.Info
                // fire an event indicating that the SetFieldWorker is setting the magnet controller output index
                statusChanged.Trigger SettingField
                let! operatingParameters = magnetController.GetOperatingParametersAsync()
                if targetFieldIndex <> 0 then
                    let targetCurrentDirection = if targetFieldIndex > 0 then Forward else Reverse
                    if operatingParameters.CurrentDirection <> targetCurrentDirection then 
                        magnetController.BeginRampToZeroAtMaximumRampRate()
                        do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync targetCurrentDirection }
            
            // define a workflow which will set the current limits on the magnet controller so that the upper set point corresponds
            // to the target output index
            let setCurrentLimits = async { 
                "Setting current limits." |> log.Info
                magnetController.SetPause true 
                // set the lower set point to the minimum value so it is not lower than the upper set point value, which would
                // prevent the upper set point from being set
                magnetController.SetLowerSetPointByIndex 0 
                magnetController.SetUpperSetPointByIndex targetFieldIndex }

            // define a workflow which will ramp to the upper set point at the maximum ramp rate
            let rampToTarget = async {
                "Ramping to target current." |> log.Info
                magnetController.SetRampTarget Upper
                magnetController.SetMaximumRampRate()
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            // if cancellation occurs during the workflow
            use! __ = Async.OnCancel(fun () ->
                // ramp to zero current or pause the magnet controller according to the cancellation options
                "Cancelling ramp to target field..." |> log.Info
                if cancellationCapability.Options.ReturnToZero then 
                    "Returning to zero current..." |> log.Info
                    magnetController.BeginRampToZeroAtMaximumRampRate()
                else 
                    "Not requested to return to zero current. Pausing magnet controller..." |> log.Info
                    magnetController.SetPause true)
            
            // run the workflows defined above
            do! setStartingCurrentDirection
            do! setCurrentLimits
            do! rampToTarget }
        
        "Starting set field workflow." |> log.Info
        // start the workflow on the thread pool with the specified continuations
        Async.StartWithContinuations(
            workflow,
            (fun () -> // success
                "Successfully reached target field." |> log.Info
                statusChanged.Trigger FinishedSettingField),
            (fun exn -> // error
                log.Error (sprintf "Failed to set field due to error: %A." exn, exn)
                statusChanged.Trigger (FailedSettingField exn)),
            (fun exn -> // cancellation
                "Canceled setting field." |> log.Info
                statusChanged.Trigger (CanceledSettingField (exn, cancellationCapability.Options.ReturnToZero))),
            cancellationCapability.Token)