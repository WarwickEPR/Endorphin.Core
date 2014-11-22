namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core.Utils
open Microsoft.FSharp.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
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
    | Cancelling of returnToZero : bool

type RampCancellationCapability() =
    static let log = LogManager.GetLogger typeof<RampCancellationCapability>

    let cancellationCapability = new CancellationTokenSource()
    let returnToZeroAfterCancellation = ref false
    let started = ref false
    let cancelling = ref false

    member this.ReturnToZero = !returnToZeroAfterCancellation
    member this.IsCancellationRequested = cancellationCapability.IsCancellationRequested

    member this.Cancel(returnToZero) =
        if !cancelling then
            failwith "Ramp is already stopping."
        if not !started then
            failwith "Cannot cancel ramp before it has been initiated."
        "Cancelling ramp token." |> log.Info
        cancelling := true
        returnToZeroAfterCancellation := returnToZero
        cancellationCapability.Cancel()

    member this.StartUsingToken() =
        if !started then
            failwith "Cannot reuse ramp token."
        "Started using ramp token." |> log.Info
        started := true 
        cancellationCapability.Token

type RampWorker(magnetController : MagnetController, ramp) =
    static let log = LogManager.GetLogger typeof<RampWorker>

    let statusChanged = new Event<RampStatus>()
    let error = new Event<Exception>()
    let canceled = new Event<OperationCanceledException>()
    let completed = new Event<unit>()
    
    let readyToStart = new ManualResetEvent(false)
    let cancellationCapability = new RampCancellationCapability()

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
    
    [<CLIEvent>]
    member this.StatusChanged = statusChanged.Publish

    [<CLIEvent>]
    member this.Error = error.Publish

    [<CLIEvent>]
    member this.Canceled = canceled.Publish

    [<CLIEvent>]
    member this.Completed = completed.Publish

    member this.Cancel(returnToZero) =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel(returnToZero)
        readyToStart.Set() |> ignore // continue the workflow if it is currently waiting

    member this.PrepareAndStart() =
        this.SetReadyToStart()
        this.Prepare()

    member this.SetReadyToStart() =
        "Setting ramp worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    member this.Prepare() =
        "Ramp worker preparing..." |> log.Info
        (sprintf "Starting current direction: %A." startingCurrentDirection) |> log.Info
        (sprintf "Final current direction: %A." finalCurrentDirection) |> log.Info
        (sprintf "Lower current limit: %07.3f A." (float lowerCurrentLimit)) |> log.Info
        (sprintf "Upper current limit: %07.3f A." (float upperCurrentLimit)) |> log.Info
        (sprintf "Starting ramp target: %A." startingRampTarget) |> log.Info
        (sprintf "Final ramp target: %A." finalRampTarget) |> log.Info
        (sprintf "Ramp rate index: %d." ramp.rampRateIndex) |> log.Info
        (sprintf "Return to zero: %A." ramp.returnToZero) |> log.Info

        let syncContext = SynchronizationContext.CaptureCurrent()

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
                syncContext.RaiseEvent statusChanged ReadyToRamp
                "Waiting for ready-for-ramp signal..." |> log.Info
                let! ready = Async.AwaitWaitHandle(readyToStart)
                "Received ready-for-ramp signal." |> log.Info
                ready |> ignore }
            
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
                "Cancelling ramp." |> log.Info
                if not cancellationCapability.ReturnToZero then 
                    "Not requested to return to zero current. Pausing ramp..." |> log.Info
                    magnetController.SetPause(true)
                else 
                    "Returning to zero current..." |> log.Info
                    magnetController.InitiateRampToZero()

            async {
                use! cancelHandler = Async.OnCancel(cancelRamp)

                do! prepareForRamp
                do! awaitReadyForRamp
                do! performRamp
                if ramp.returnToZero then 
                    do! magnetController.RampToZeroAsync()

                if not cancellationCapability.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged Finished }
        
        "Starting ramp workflow." |> log.Info
        Async.StartWithContinuations
            (workflow,
                (fun () -> syncContext.RaiseEvent completed ()), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> 
                    syncContext.RaiseEvent statusChanged (Cancelling cancellationCapability.ReturnToZero)
                    syncContext.RaiseEvent canceled exn),
                cancellationCapability.StartUsingToken())