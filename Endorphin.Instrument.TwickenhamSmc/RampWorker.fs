namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Reactive.Linq
open log4net

/// Defines the parameters of a Twickenham magnet controller ramp.
type Ramp =
    { /// Specifies the (signed) starting field output index in terms of digital magnet controller output steps. A negative
      /// value corresponds to reverse current.
      StartingFieldIndex : int
      /// Specifies the (signed) final field output index in terms of digital magnet controller output steps. A negative
      /// value corresponds to reverse current.
      FinalFieldIndex : int
      /// Specifies the magnet controller ramp rate during the ramp by its index in the available calibrated ramp rates.
      RampRateIndex : int
      /// Specifies whether the magnet controller should be set to return to zero current once the ramp is complete.
      ReturnToZero : bool }

    /// Gives the magnet controller CurrentDirection for a given magnet controller output index. Positive values correspond
    /// to forward current, and negative values correspond to reverse. Zero current is ambiguous.
    static member private CurrentDirection index =
        match index with
        | c when c > 0 -> Forward
        | c when c < 0 -> Reverse
        | _ -> failwith "Magnet controller direction for zero current is arbitrary."

    /// Gives the starting current direction for the ramp.
    member ramp.StartingCurrentDirection = 
        match (ramp.StartingFieldIndex, ramp.FinalFieldIndex) with
        | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
        | (0, f) -> Ramp.CurrentDirection f // use the current direction of the final field index if the starting one is zero
        | (s, _) -> Ramp.CurrentDirection s // use the current direction of the starting field index otherwise
       
    /// Gives the final current direction for the ramp.
    member ramp.FinalCurrentDirection =
        match (ramp.StartingFieldIndex, ramp.FinalFieldIndex) with
        | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
        | (s, 0) -> Ramp.CurrentDirection s // use the current direction of the starting field index if the final one is zero
        | (_, f) -> Ramp.CurrentDirection f // use the current direction of the final field index otherwise

    /// Gives the current index which should be set for the magnet controller upper current set point.
    member ramp.UpperCurrentIndex = 
        max (abs ramp.StartingFieldIndex) (abs ramp.FinalFieldIndex)

    /// Gives the current index which should be set for the magnet controller lower current set point.
    member ramp.LowerCurrentIndex = 
        min (abs ramp.StartingFieldIndex) (abs ramp.FinalFieldIndex)

    /// Gives the magnet controller ramp target corresponding to the starting current index.
    member ramp.StartingRampTarget =
        match (abs ramp.StartingFieldIndex, abs ramp.FinalFieldIndex) with
        | (0, _) -> Zero
        | (s, f) when s <= f -> Lower
        | _ -> Upper

    /// Gives the magnet controller ramp target corresponding to the final current index.
    member ramp.FinalRampTarget = 
        match (abs ramp.StartingFieldIndex, abs ramp.FinalFieldIndex) with
        | (_, 0) -> Zero
        | (s, f) when f >= s -> Upper
        | _ -> Lower

    /// Gives an integer value of +/- 1 corresponding to the sign of the starting current for the ramp.
    member ramp.StartingCurrentSign =
        match ramp.StartingCurrentDirection with
        | Forward -> 1
        | Reverse -> -1

    /// Gives an integer value of +/- 1 corresponding to the sign of the final current for the ramp.
    member ramp.FinalCurrentSign =
        match ramp.FinalCurrentDirection with
        | Forward -> 1
        | Reverse -> -1

/// Indicates the status of a RampWorker.
type RampStatus =
    /// Indicates that the RampWorker has started preparing for the magnet controller ramp by ramping to the intial current
    /// at the maximum ramp rate.
    | PreparingRamp
    /// Indicates that the RampWorker is waiting the ready-to-start signal at the starting current index with the specified
    /// current direction.
    | ReadyToRamp of CurrentDirection
    /// Indicates the RampWorker has reached the zero current ramp target in order to change current direction of the magnet
    /// controller before proceeding to the final ramp target.
    | ChangingCurrentDirection
    /// Indicates that the RampWorker has changed current direction to the specified value and is ready to to proceed to the
    /// final ramp target after receiving the ready-to-continue signal.
    | ReadyToContinue of CurrentDirection
    /// Indiciates that the RampWorker is ramping the magnet controller with the specified current direction.
    | Ramping of CurrentDirection
    /// Indicates that the RampWorker has succesfully finished the magnet controller ramp.
    | FinishedRamp
    /// Indicates that the RampWorker has been canceled and whether the magnet controller was set to return to zero field at
    /// the time of cancellation.
    | CanceledRamp of exn : OperationCanceledException * returnToZero : bool
    /// Indicates that the RampWorker has failed to perform the ramp due to an error.
    | FailedRamp of exn : exn

/// Specifies the cancellation options given when cancelling a RampWorker ramp.
type RampCancellationOptions =
    { /// Inidicates whether the magnet controller should be set to return to zero current after cancelling the ramp.
      ReturnToZero : bool }

/// Executes a magnet controller ramp workflow with the specified ramp parameters on the given Twickenham magnet controller.
type RampWorker(magnetController : MagnetController, ramp) =
    static let log = LogManager.GetLogger typeof<RampWorker> // logger.

    // events
    let statusChanged = new Event<RampStatus>()
    
    // capture the current synchronisation context so that events can be fired on the UI thread or thread pool accordingly
    let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

    // handle to indicate that the ramp should start once it is prepared
    let readyToStart = new ManualResetHandle(false)
    // handle to indicate that the ramp should continue after changing current polarity at zero current
    let readyToContinue = new ManualResetHandle(false)
    // cancellation capability which is used to stop the magnet controller ramp
    let cancellationCapability = new CancellationCapability<RampCancellationOptions>()
    
    // compute the starting and final currents in amps from their respective output indecies
    let startingCurrent = magnetController.MagnetControllerParameters.CurrentForIndex ramp.StartingFieldIndex
    let finalCurrent = magnetController.MagnetControllerParameters.CurrentForIndex ramp.FinalFieldIndex

    do // sanity checks
        // raise an exception if the ramp has zero steps
        if ramp.StartingFieldIndex = ramp.FinalFieldIndex then
            failwith "Ramp starting and final current are the same."
        // raise an exception if the starting current, final current or ramp rate are out of bounds
        if abs startingCurrent > magnetController.MagnetControllerParameters.CurrentLimit then
            failwith "Ramp starting current outside of magnet controller current limit."
        if abs finalCurrent > magnetController.MagnetControllerParameters.CurrentLimit then
            failwith "Ramp final current outside of magnet controller current limit."
        if ramp.RampRateIndex < 0 || ramp.RampRateIndex >= Seq.length magnetController.MagnetControllerParameters.AvailableCurrentRampRates then
            failwith "Ramp rate index outside of available ramp rate range."
    
    /// Event fires when the RampWorker status changes. Events are fired on the System.Threading.SynchronizationContext which
    /// instantiated the worker.
    member __.StatusChanged = 
        Observable.CreateFromEvent(
            statusChanged.Publish,
            // fire OnCompleted when the ramp finishes
            ((=) FinishedRamp), 
            // fire OnError when the ramp fails or is canceled
            (fun status ->
                match status with
                | FailedRamp exn -> Some exn
                | CanceledRamp (exn, _) -> Some (exn :> exn)
                | _ -> None))
            .ObserveOn(syncContext)

    /// Cancels the magnet controller ramp, indicating whether the magnet controller should be set to return to zero current.
    member __.Cancel returnToZero =
        "Ramp worker stopping..." |> log.Info
        cancellationCapability.Cancel { ReturnToZero = returnToZero }
        // continue the workflow if it is currently waiting
        readyToStart.Set() |> ignore
        readyToContinue.Set() |> ignore

    /// Initiates the magnet controller ramp and sets the ready-to-start and ready-to-contine flags.
    member rampWorker.PrepareAndStart() =
        rampWorker.SetReadyToStart()
        rampWorker.SetReadyToContinue()
        rampWorker.Prepare()

    /// Sets the ready-to-start flag, indicating the ramp should start once it is prepared. The ready-to-continue flag will
    /// also need to be set before the ramp can finish if a current direction change occurs during the ramp.
    member __.SetReadyToStart() =
        "Setting ramp worker ready to start." |> log.Info
        readyToStart.Set() |> ignore

    /// Sets the ready-to-continue flag, indicating the RampWorker should continue ramping once it has changed the magnet
    /// controller current polarity at zero current.
    member __.SetReadyToContinue() =
        "Setting ramp worker ready to continue." |> log.Info
        readyToContinue.Set() |> ignore

    /// Initiates the RampWorker workflow, preparing the magnet controller to the initial current. The ready-to-start flag
    /// will need to be set before the ramp begins.
    member __.Prepare() =
        if cancellationCapability.IsCancellationRequested then
            failwith "Cannot prepare ramp as cancellation was already requested."

        "Ramp worker preparing..." |> log.Info
        sprintf "Starting current direction: %A." ramp.StartingCurrentDirection |> log.Info
        sprintf "Final current direction: %A." ramp.FinalCurrentDirection |> log.Info
        sprintf "Starting current: %08.4f A." (float startingCurrent) |> log.Info
        sprintf "Final current: %08.4f A." (float finalCurrent) |> log.Info
        sprintf "Starting ramp target: %A." ramp.StartingRampTarget |> log.Info
        sprintf "Final ramp target: %A." ramp.FinalRampTarget |> log.Info
        sprintf "Ramp rate index: %d." ramp.RampRateIndex |> log.Info
        sprintf "Return to zero: %A." ramp.ReturnToZero |> log.Info

        // define the ramp workflow
        let workflow = async {
            
            // define a workflow which will ramp the magnet controller to zero current and change the current direction if the
            // present current direction is not the initial current direction for the ramp
            let setStartingCurrentDirection initialState = async {
                "Setting starting current direction." |> log.Info
                if initialState.OperatingParameters.CurrentDirection <> ramp.StartingCurrentDirection then
                    magnetController.BeginRampToZeroAtMaximumRampRate()
                    do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync ramp.StartingCurrentDirection }
            
            // define a workflow which will set lower and upper current limits
            let setCurrentLimits initialState = async { 
                "Setting current limits." |> log.Info
                magnetController.SetPause true 
                let setLowerLimit() = magnetController.SetLowerSetPointByIndex ramp.LowerCurrentIndex
                let setUpperLimit() = magnetController.SetUpperSetPointByIndex ramp.UpperCurrentIndex
                
                // If the initial lower current limit is larger than the new upper current limit for the ramp,
                // then the lower current limit must be changed first or the update to the upper current limit 
                // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
                // than the initial lower current limit) is smaller than the new lower current limit, then the
                // upper current limit must be changed first.
                if initialState.SetPointParameters.LowerSetPoint >= (max startingCurrent finalCurrent) then
                    setLowerLimit() ; setUpperLimit()
                else 
                    setUpperLimit() ; setLowerLimit() }

            // define a workflow which will ramp the magnet controller to the initial current once the correct current direction
            // and the current limits have been set
            let rampToInitialCurrent = async {
                "Ramping to initial current." |> log.Info
                magnetController.SetRampTarget ramp.StartingRampTarget
                magnetController.SetMaximumRampRate() 
                magnetController.SetPause false
                do! magnetController.WaitToReachTargetAsync() } 
            
            // define a workflow which will ramp to the initial magnet controller current for the ramp
            let prepareForRamp = async {
                "Preparing for ramp..." |> log.Info

                // raise an event to indicate that the RampWorker is preparing
                statusChanged.Trigger PreparingRamp
                let! initialState = magnetController.GetAllParametersAsync()
                do! setStartingCurrentDirection initialState
                do! setCurrentLimits initialState 
                do! rampToInitialCurrent
                magnetController.SetRampRateByIndex (ramp.RampRateIndex) }

            // define a workflow which will wait for the ready-to-start flag to be set at the starting current 
            let awaitReadyToStart = async {
                "Waiting for ready-to-start signal..." |> log.Info
                // raise an event to indicate that the magnet controller has prepared to the initial current and is waiting for 
                // the ready-to-start signal to begin the ramp
                statusChanged.Trigger (ReadyToRamp ramp.StartingCurrentDirection)
                do! Async.AwaitWaitHandle readyToStart |> Async.Ignore
                "Received ready-to-start signal." |> log.Info }

            // define a workflow which will wait for the ready-to-continue flag to be set after changing current direction
            let awaitReadyToContinue = async {
                "Waiting for ready-to-continue signal..." |> log.Info
                // raise an event to indicate that the magnet controller has changed current direction at zero current and is
                // waiting for the ready-to-continue signal before ramping to the final current target
                statusChanged.Trigger (ReadyToContinue ramp.FinalCurrentDirection)
                do! Async.AwaitWaitHandle readyToContinue |> Async.Ignore
                "Ready ready-to-continue signal." |> log.Info }
            
            // define a workflow which will perform the magnet controller ramp
            let performRamp = async {
                // wait for the ready-to-start flag to be set
                do! awaitReadyToStart
                "Performing ramp..." |> log.Info

                // if a current direction change is required during the ramp
                if ramp.StartingCurrentDirection <> ramp.FinalCurrentDirection then
                    "Ramp range goes through zero current. Ramping to zero current to change current direction." |> log.Info
                    // raise an event that the magnet controller is ramping with the starting current direction and ramp to zero
                    statusChanged.Trigger (Ramping ramp.StartingCurrentDirection)
                    magnetController.SetRampTarget Zero
                    do! magnetController.WaitToReachTargetAsync()

                    // raise an event that the magnet controller is about to change current direction and perform the change
                    statusChanged.Trigger ChangingCurrentDirection
                    do! magnetController.WaitToReachZeroAndSetCurrentDirectionAsync ramp.FinalCurrentDirection 
                    "Reached zero current and changed current direction." |> log.Info

                    // wait for the ready-to-continue flag to be set
                    do! awaitReadyToContinue
                
                "Ramping to final ramp target." |> log.Info
                // raise an event to indicate that the magnet controller is ramping with the final current direction and ramp to
                // the final ramp target
                statusChanged.Trigger (Ramping ramp.FinalCurrentDirection)
                magnetController.SetRampTarget ramp.FinalRampTarget  
                do! magnetController.WaitToReachTargetAsync() 
                "Reached final ramp target." |> log.Info } 

            // if cancellation occurs
            use! __ = Async.OnCancel(fun () ->
                "Cancelling ramp..." |> log.Info
                // ramp to zero or pause the magnet controller according to the cancellation options
                if cancellationCapability.Options.ReturnToZero then 
                    "Returning to zero current..." |> log.Info
                    magnetController.BeginRampToZeroAtMaximumRampRate() 
                else 
                    "Not requested to return to zero current. Pausing magnet controller..." |> log.Info
                    magnetController.SetPause true)
            
            // run the workflows defined above
            do! prepareForRamp
            do! performRamp

            // if the ramp is set to return to zero, then start ramping to zero current
            if ramp.ReturnToZero then 
                magnetController.BeginRampToZeroAtMaximumRampRate() }
        
        "Starting ramp workflow." |> log.Info
        // start the ramp workflow on the thread pool with the specified continuations
        Async.StartWithContinuations(
            workflow,
            (fun () -> // success
                "Ramp completed successfully." |> log.Info
                statusChanged.Trigger FinishedRamp),
            (fun exn -> // error
                log.Error (sprintf "Ramp failed due to error: %A." exn, exn)
                statusChanged.Trigger (FailedRamp exn)),
            (fun exn -> // cancellation
                "Ramp Canceled." |> log.Info
                statusChanged.Trigger (CanceledRamp (exn, cancellationCapability.Options.ReturnToZero))),
            cancellationCapability.Token)