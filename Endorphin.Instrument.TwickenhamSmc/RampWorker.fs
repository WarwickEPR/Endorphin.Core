namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core.Utils
open Microsoft.FSharp.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Reactive.Linq

type Ramp = {
    startingFieldIndex : int
    finalFieldIndex : int
    rampRateIndex : int
    returnToZero : bool }

type RampStatus =
    | PreparingForRamp
    | ReadyToRamp
    | PerformingRamp
    | FinishedRamp
    | CancellingRamp

type RampCancellationCapability() =
    let cancellationCapability = new CancellationTokenSource()
    let returnToZeroAfterCancellation = ref false
    let canCancel = ref false

    member this.ReturnToZero = !returnToZeroAfterCancellation
    member this.IsCancellationRequested = cancellationCapability.IsCancellationRequested

    member this.Cancel(returnToZero) =
        if not !canCancel then
            failwith "Cannot cancel ramp before it has been initiated."
        returnToZeroAfterCancellation := returnToZero
        cancellationCapability.Cancel()

    member this.StartUsingToken() =
        canCancel := true 
        cancellationCapability.Token

type RampWorker(ramp, magnetController : MagnetController) = 
    let statusChanged = new Event<RampStatus>()
    let error = new Event<Exception> ()
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
        cancellationCapability.Cancel(returnToZero)
        readyToStart.Set() |> ignore // continue the workflow if it is waiting
        let syncContext = SynchronizationContext.CaptureCurrent()
        syncContext.RaiseEvent statusChanged CancellingRamp

    member this.PrepareAndStart() =
        readyToStart.Set() |> ignore
        this.Prepare()

    member this.SetReadyToStart() =
        readyToStart.Set() |> ignore

    member this.Prepare() =
        let syncContext = SynchronizationContext.CaptureCurrent()

        let workflow =         
            let setStartingCurrentDirection initialState = async {
                if not (initialState.operatingParameters.currentDirection = startingCurrentDirection)
                then do! magnetController.RampToZeroAndSetCurrentDirectionAsync startingCurrentDirection }
            
            let setCurrentLimits initialState = async { 
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
                magnetController.SetRampTarget startingRampTarget
                magnetController.SetRampRate (magnetController.RampRateLimit) 
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            let prepareForRamp = async {
                if not cancellationCapability.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged PreparingForRamp
                let! initialState = magnetController.GetAllParametersAsync()
                do! setStartingCurrentDirection initialState
                do! setCurrentLimits initialState 
                do! rampToInitialCurrent
                magnetController.SetRampRateByIndex (ramp.rampRateIndex) }

            let awaitReadyForRamp = async {
                if not cancellationCapability.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged ReadyToRamp
                let! ready = Async.AwaitWaitHandle(readyToStart)
                ready |> ignore }
            
            let performRamp = async { 
                if not cancellationCapability.IsCancellationRequested then
                    syncContext.RaiseEvent statusChanged PerformingRamp
                if not (startingCurrentDirection = finalCurrentDirection) then 
                    magnetController.SetRampTarget Zero 
                    do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync finalCurrentDirection 
                
                magnetController.SetRampTarget finalRampTarget  
                do! magnetController.WaitToReachTargetAsync() }

            let cancelRamp () =
                // note: ! is not the logical not operator in F#
                if not cancellationCapability.ReturnToZero then magnetController.SetPause(true)
                else magnetController.InitiateRampToZero()

            async {
                try
                    use! cancelHandler = Async.OnCancel(cancelRamp)
                    do! prepareForRamp
                    do! awaitReadyForRamp
                    do! performRamp
                    if ramp.returnToZero then 
                        do! magnetController.RampToZeroAsync()

                    if not cancellationCapability.IsCancellationRequested then
                        syncContext.RaiseEvent statusChanged FinishedRamp
                
                finally
                    syncContext.RaiseEvent completed () }
            
        Async.StartWithContinuations
            (workflow,
                (ignore), // no continuation unless there is an error or cancellation
                (fun exn -> syncContext.RaiseEvent error exn),
                (fun exn -> syncContext.RaiseEvent canceled exn),
                cancellationCapability.StartUsingToken())