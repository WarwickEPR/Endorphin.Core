namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net

type Ramp = {
    startingFieldIndex : int
    finalFieldIndex : int
    rampRateIndex : int
    returnToZero : bool }

type RampStatus =
    | PreparingRamp
    | ReadyToRamp of CurrentDirection
    | ChangingCurrentDirection
    | ReadyToContinue of CurrentDirection // after crossing zero current
    | Ramping of CurrentDirection
    | FinishedRamp
    | CanceledRamp of returnToZero : bool
    | FailedRamp

type RampCancellationOptions = {
    returnToZero : bool }

type RampWorker(magnetController : MagnetController, ramp) =
    static let log = LogManager.GetLogger typeof<RampWorker>

    let statusChanged = new Event<RampStatus>()
    let success = new Event<unit>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    
    let readyToStart = new ManualResetHandle(false)
    let readyToContinue = new ManualResetHandle(false)
    let cancellationCapability =
        new CancellationCapability<RampCancellationOptions>()
    
    let currentDirection index =
        match index with
        | c when c > 0 -> Forward
        | c when c < 0 -> Reverse
        | _ -> failwith "Magnet controller direction for zero current is arbitrary."

    let startingCurrentDirection = 
        match (ramp.startingFieldIndex, ramp.finalFieldIndex) with
        | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
        | (0, f) -> currentDirection f
        | (s, _) -> currentDirection s
        
    let finalCurrentDirection =
        match (ramp.startingFieldIndex, ramp.finalFieldIndex) with
        | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
        | (s, 0) -> currentDirection s
        | (_, f) -> currentDirection f

    let startingCurrent =
        magnetController.DeviceParameters.CurrentForIndex (ramp.startingFieldIndex)

    let finalCurrent =
        magnetController.DeviceParameters.CurrentForIndex (ramp.finalFieldIndex)

    let upperCurrentIndex = 
        max (abs ramp.startingFieldIndex) (abs ramp.finalFieldIndex)

    let lowerCurrentIndex = 
        min (abs ramp.startingFieldIndex) (abs ramp.finalFieldIndex)

    let startingRampTarget =
        match (abs ramp.startingFieldIndex, abs ramp.finalFieldIndex) with
        | (0, _) -> Zero
        | (s, f) when s <= f -> Lower
        | _ -> Upper

    let finalRampTarget = 
        match (abs ramp.startingFieldIndex, abs ramp.finalFieldIndex) with
        | (_, 0) -> Zero
        | (s, f) when f >= s -> Upper
        | _ -> Lower

    do // initialisation checks
        if abs startingCurrent > magnetController.DeviceParameters.currentLimit then
            failwith "Ramp starting current outside of magnet controller current limit."
        if abs finalCurrent > magnetController.DeviceParameters.currentLimit then
            failwith "Ramp final current outside of magnet controller current limit."
        if ramp.rampRateIndex < 0 || ramp.rampRateIndex >= List.length magnetController.DeviceParameters.AvailableCurrentRampRates then
            failwith "Ramp rate index outside of available ramp rate range."
    
    member __.StatusChanged = statusChanged.Publish
    member __.Success = success.Publish
    member __.Error = error.Publish
    member __.Canceled = canceled.Publish

    member __.Cancel returnToZero =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel { returnToZero = returnToZero }
        // continue the workflow if it is currently waiting
        readyToStart.Set() |> ignore
        readyToContinue.Set() |> ignore

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.SetReadyToContinue()
        this.Prepare()

    member __.SetReadyToStart() =
        "Setting ramp worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member __.SetReadyToContinue() =
        "Setting ramp worker ready to continue." |> log.Info
        readyToContinue.Set() |> ignore

    member __.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare ramp as cancellation was already requested."

        "Ramp worker preparing..." |> log.Info
        sprintf "Starting current direction: %A." startingCurrentDirection |> log.Info
        sprintf "Final current direction: %A." finalCurrentDirection |> log.Info
        sprintf "Starting current: %08.4f A." (float startingCurrent) |> log.Info
        sprintf "Final current: %08.4f A." (float finalCurrent) |> log.Info
        sprintf "Starting ramp target: %A." startingRampTarget |> log.Info
        sprintf "Final ramp target: %A." finalRampTarget |> log.Info
        sprintf "Ramp rate index: %d." ramp.rampRateIndex |> log.Info
        sprintf "Return to zero: %A." ramp.returnToZero |> log.Info

        let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

        let workflow = async {
            // define workflows which will be used to perform the ramp
            let setStartingCurrentDirection initialState = async {
                "Setting starting current direction." |> log.Info
                if not (initialState.operatingParameters.currentDirection = startingCurrentDirection)
                then do! magnetController.RampToZeroAndSetCurrentDirectionAsync startingCurrentDirection }
            
            let setCurrentLimits initialState = async { 
                "Setting current limits." |> log.Info
                magnetController.SetPause true 
                let setLowerLimit() = magnetController.SetLowerSetPointByIndex lowerCurrentIndex
                let setUpperLimit() = magnetController.SetUpperSetPointByIndex upperCurrentIndex
                
                // If the initial lower current limit is larger than the new upper current limit for the ramp,
                // then the lower current limit must be changed first or the update to the upper current limit 
                // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
                // than the initial lower current limit) is smaller than the new lower current limit, then the
                // upper current limit must be changed first.
                if initialState.setPointParameters.lowerSetPoint >= (max startingCurrent finalCurrent) then
                    setLowerLimit() ; setUpperLimit()
                else 
                    setUpperLimit() ; setLowerLimit() }

            let rampToInitialCurrent = async {
                "Ramping to initial current." |> log.Info
                magnetController.SetRampTarget startingRampTarget
                magnetController.SetRampRate (magnetController.DeviceParameters.rampRateLimit) 
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            let prepareForRamp = async {
                "Preparing for ramp..." |> log.Info
                syncContext.RaiseEvent statusChanged PreparingRamp
                let! initialState = magnetController.GetAllParametersAsync()
                do! setStartingCurrentDirection initialState
                do! setCurrentLimits initialState 
                do! rampToInitialCurrent
                magnetController.SetRampRateByIndex (ramp.rampRateIndex) }

            let awaitReadyForRamp = async {
                "Waiting for ready-to-start signal..." |> log.Info
                syncContext.RaiseEvent statusChanged (ReadyToRamp startingCurrentDirection)
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore
                "Received ready-to-start signal." |> log.Info }

            let awaitReadyToContinue = async {
                "Waiting for ready-to-continue signal..." |> log.Info
                syncContext.RaiseEvent statusChanged (ReadyToContinue finalCurrentDirection)
                do! Async.AwaitWaitHandle readyToContinue |> Async.Ignore
                "Ready ready-to-continue signal." |> log.Info }
            
            let performRamp = async { 
                "Performing ramp..." |> log.Info
                if not (startingCurrentDirection = finalCurrentDirection) then
                    "Ramp range goes through zero current. Ramping to zero current to change current direction." |> log.Info
                    syncContext.RaiseEvent statusChanged (Ramping startingCurrentDirection)
                    magnetController.SetRampTarget Zero
                    do! magnetController.WaitToReachTargetAsync()
                    syncContext.RaiseEvent statusChanged ChangingCurrentDirection
                    do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync finalCurrentDirection 
                    "Reached zero current and changed current direction." |> log.Info
                    do! awaitReadyToContinue
                
                "Ramping to final ramp target." |> log.Info
                syncContext.RaiseEvent statusChanged (Ramping finalCurrentDirection)
                magnetController.SetRampTarget finalRampTarget  
                do! magnetController.WaitToReachTargetAsync() 
                "Reached final ramp target." |> log.Info } 

            // set up cancellation handler
            use! __ = Async.OnCancel(fun () ->
                "Cancelling ramp..." |> log.Info
                if cancellationCapability.Options.returnToZero then 
                    "Returning to zero current..." |> log.Info
                    magnetController.BeginRampToZero() 
                else 
                    "Not requested to return to zero current. Pausing magnet controller..." |> log.Info
                    magnetController.SetPause true

                "Ramp Canceled." |> log.Info
                syncContext.RaiseEvent statusChanged (CanceledRamp cancellationCapability.Options.returnToZero))
            
            // perform ramp
            do! prepareForRamp
            do! awaitReadyForRamp
            do! performRamp
            if ramp.returnToZero then 
                do! magnetController.RampToZeroAsync()
            
            "Ramp completed successfully." |> log.Info
            syncContext.RaiseEvent statusChanged FinishedRamp }
        
        "Starting ramp workflow." |> log.Info
        Async.StartWithContinuations(
            workflow,
            (fun () -> syncContext.RaiseEvent success ()),
            (fun exn -> // error
                log.Error (sprintf "Ramp failed due to error: %A." exn, exn)
                syncContext.RaiseEvent statusChanged FailedRamp
                syncContext.RaiseEvent error exn),
            (fun exn -> syncContext.RaiseEvent canceled exn),
            cancellationCapability.Token)
