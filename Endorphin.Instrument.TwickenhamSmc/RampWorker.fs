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
    | Preparing
    | ReadyToRamp
    | Ramping
    | Finished
    | Canceled of returnToZero : bool

type RampCancellationOptions = {
    returnToZero : bool }

type RampWorker(magnetController : MagnetController, ramp) =
    static let log = LogManager.GetLogger typeof<RampWorker>

    let statusChanged = new Event<RampStatus>()
    let error = new Event<Exception>()
    let cancelling = new Event<OperationCanceledException>()
    let completed = new Event<unit>()
    let workerFinished = new Event<unit>()
    
    let readyToStart = new ManualResetHandle(false)
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

    let floorCurrentLimitForIndex index =
        float (magnetController.CurrentForIndex index)
        |> (*) (10.0 ** float magnetController.SetPointDecimalPlaces)
        |> floor
        |> (*) (1.0<A> * 10.0 ** float -magnetController.SetPointDecimalPlaces)

    let ceilCurrentLimitForIndex index =
        float (magnetController.CurrentForIndex index)
        |> (*) (10.0 ** float magnetController.SetPointDecimalPlaces)
        |> ceil
        |> (*) (1.0<A> * 10.0 ** float -magnetController.SetPointDecimalPlaces)

    let startingField =
        if ramp.startingFieldIndex < ramp.finalFieldIndex then
            floorCurrentLimitForIndex ramp.startingFieldIndex
        else
            ceilCurrentLimitForIndex ramp.startingFieldIndex

    let finalField =
        if ramp.startingFieldIndex < ramp.finalFieldIndex then
            ceilCurrentLimitForIndex ramp.finalFieldIndex
        else
            floorCurrentLimitForIndex ramp.finalFieldIndex

    let upperCurrentLimit = 
        max (abs startingField) (abs finalField)

    let lowerCurrentLimit = 
        min (abs startingField) (abs finalField)

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
        if abs startingField > magnetController.CurrentLimit then
            failwith "Ramp starting current outside of magnet controller current limit."
        if abs finalField > magnetController.CurrentLimit then
            failwith "Ramp final current outside of magnet controller current limit."
        if ramp.rampRateIndex < 0 || ramp.rampRateIndex >= Seq.length magnetController.AvailableCurrentRampRates then
            failwith "Ramp rate index outside of available ramp rate range."
    
    member this.StatusChanged = statusChanged.Publish
    member this.Error = error.Publish
    member this.Cancelling = cancelling.Publish
    member this.Completed = completed.Publish

    member this.Cancel(returnToZero) =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel { returnToZero = returnToZero }
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting ramp worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare ramp as cancellation was already requested."

        "Ramp worker preparing..." |> log.Info
        (sprintf "Starting current direction: %A." startingCurrentDirection) |> log.Info
        (sprintf "Final current direction: %A." finalCurrentDirection) |> log.Info
        (sprintf "Lower current limit: %07.3f A." (float lowerCurrentLimit)) |> log.Info
        (sprintf "Upper current limit: %07.3f A." (float upperCurrentLimit)) |> log.Info
        (sprintf "Starting ramp target: %A." startingRampTarget) |> log.Info
        (sprintf "Final ramp target: %A." finalRampTarget) |> log.Info
        (sprintf "Ramp rate index: %d." ramp.rampRateIndex) |> log.Info
        (sprintf "Return to zero: %A." ramp.returnToZero) |> log.Info

        let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

        let workflow =         
            let setStartingCurrentDirection initialState = async {
                "Setting starting current direction." |> log.Info
                if not (initialState.operatingParameters.currentDirection = startingCurrentDirection)
                then do! magnetController.RampToZeroAndSetCurrentDirectionAsync startingCurrentDirection }
            
            let setCurrentLimits initialState = async { 
                "Setting current limits." |> log.Info
                magnetController.SetPause true 
                let setLowerLimit = fun () -> magnetController.SetLowerSetPoint lowerCurrentLimit
                let setUpperLimit = fun () -> magnetController.SetUpperSetPoint upperCurrentLimit 
                
                // If the initial lower current limit is larger than the new upper current limit for the ramp,
                // then the lower current limit must be changed first or the update to the upper current limit 
                // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
                // than the initial lower current limit) is smaller than the new lower current limit, then the
                // upper current limit must be changed first.
                if initialState.setPointParameters.lowerLimit >= upperCurrentLimit then
                    setLowerLimit() ; setUpperLimit()
                else 
                    setUpperLimit() ; setLowerLimit() }

            let rampToInitialCurrent = async {
                "Ramping to initial current." |> log.Info
                magnetController.SetRampTarget startingRampTarget
                magnetController.SetRampRate (magnetController.RampRateLimit) 
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            let prepareForRamp = async {
                "Preparing for ramp..." |> log.Info
                syncContext.RaiseEvent statusChanged Preparing
                let! initialState = magnetController.GetAllParametersAsync()
                do! setStartingCurrentDirection initialState
                do! setCurrentLimits initialState 
                do! rampToInitialCurrent
                magnetController.SetRampRateByIndex (ramp.rampRateIndex) }

            let awaitReadyForRamp = async {
                "Waiting for ready-for-ramp signal..." |> log.Info
                syncContext.RaiseEvent statusChanged ReadyToRamp
                do! Async.AwaitWaitHandle(readyToStart) |> Async.Ignore
                "Received ready-for-ramp signal." |> log.Info }
            
            let performRamp = async { 
                "Performing ramp..." |> log.Info
                syncContext.RaiseEvent statusChanged Ramping
                if not (startingCurrentDirection = finalCurrentDirection) then
                    "Ramp range goes through zero current. Ramping to zero current to change current direction." |> log.Info
                    magnetController.SetRampTarget Zero 
                    do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync finalCurrentDirection 
                    "Reached zero current and changed current direction." |> log.Info
                
                "Ramping to final ramp target." |> log.Info
                magnetController.SetRampTarget finalRampTarget  
                do! magnetController.WaitToReachTargetAsync() 
                "Reached final ramp target." |> log.Info } 

            let cancelRamp () = 
                async {
                    "Cancelling ramp..." |> log.Info
                    if cancellationCapability.Options.returnToZero then 
                        "Returning to zero current..." |> log.Info
                        magnetController.InitiateRampToZero() 
                    else 
                        "Not requested to return to zero current. Pausing ramp..." |> log.Info
                        magnetController.SetPause(true)


                    "Worker finished with cancellation." |> log.Info
                    syncContext.RaiseEvent statusChanged (Canceled cancellationCapability.Options.returnToZero)
                    syncContext.RaiseEvent workerFinished () }
                |> Async.Start
            
            async {
                use! cancelHandler = 
                    Async.OnCancel(cancelRamp)

                try
                    do! prepareForRamp
                    do! awaitReadyForRamp
                    do! performRamp
                    if ramp.returnToZero then 
                        do! magnetController.RampToZeroAsync()
                
                finally
                    "Worker finished ramp." |> log.Info 
                    syncContext.RaiseEvent workerFinished () }
        
        async {
            "Listening for worker finished event." |> log.Info
            let! waitToFinish =
                workerFinished.Publish
                |> Async.AwaitEvent
                |> Async.StartChild
            
            "Starting ramp workflow." |> log.Info
            Async.StartWithContinuations(
                workflow,
                (fun () -> syncContext.RaiseEvent statusChanged Finished),
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> syncContext.RaiseEvent cancelling exn),
                cancellationCapability.Token)
            
            "Waiting for worker to finish." |> log.Info
            do! waitToFinish 
            "Ramp completed." |> log.Info
            syncContext.RaiseEvent completed () }
        |> Async.Start