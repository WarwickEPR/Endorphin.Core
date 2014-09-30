module MagnetRampManager

open Units
open Microsoft.FSharp.Control
open MagnetController
open System
open System.Threading
open System.Reactive.Linq
open System.Reactive.Subjects
open FSharp.Control.Observable

/// <summary>
/// Record type describing the parameters of a ramp. Note that the starting and final currents
/// are signed.
/// </summary>
type Ramp = 
    { /// <summary>Signed starting current for the ramp in A.</summary>
      startingCurrent : float<A> // in A signed
      /// <summary>Signed final current for the ramp in A.</summary>
      finalCurrent : float<A> // in A, signed
      /// <summary>Signed ramp rate for the ramp in A/s.</summary>
      rampRate : float<A/s> // in A/s
      /// <summary>Indicates whether the ramp should return to zero once the ramp is finished.</summary>
      returnToZero : bool
      /// <summary>
      /// An observable which indicates whether the requester of the ramp is ready for the ramp to
      /// begin. This can be used to initialise some data acquisition once the ramp is prepared.
      /// </summary>
      readyForRamp : IObservable<bool> }

/// <summary>
/// Discriminated union type describing the possible values for the status of a ramp operation.
/// </summary>
type RampStatus =
    | Initialising
    | PreparingForRamp
    | ReadyToRamp
    | PerformingRamp
    | FinishedRamp
    | CancelledRamp    

/// <summary>
/// Discriminated union type describing the possible commands which can be sent to a magnet ramp
/// manager.
/// </summary>
type Command =
    | PerformRamp of Ramp * AsyncReplyChannel<IObservable<RampStatus>>
    | CancelRamp of bool

/// <summary>
/// Actor mailbox for a magnet ramp manager which performs ramp operations on a Twickenham
/// superconducting magnet controller.
/// </summary>
/// <param name="magnetController">The magnet controller mailbox.</param>
let magnetRampManagerMailbox (magnetController : MailboxProcessor<MagnetController.Command>) = 
    fun (mailbox : MailboxProcessor<Command>) ->

        /// <summary>
        /// Returns a <see cref="MagnetController.CurrentDirection" /> object for a signed current.
        /// If the current is zero, the direction cannot be the determined and the function throws
        /// an exception.
        /// </summary>
        /// <param name="current">The signed current value.</param>
        let currentDirection current =
            match current with
            | c when c > 0.0<A> -> Forward
            | c when c < 0.0<A> -> Reverse
            | _ -> failwith "Magnet controller direction for zero current is arbitrary."
        
        /// <summary>
        /// Determines the starting current direction for a <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let startingCurrentDirection ramp = 
            match (ramp.startingCurrent, ramp.finalCurrent) with
            | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
            | (0.0<A>, f) -> currentDirection f
            | (s, _) -> currentDirection s
        
        /// <summary>
        /// Determines the final current direction for a <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let finalCurrentDirection ramp =
            match (ramp.startingCurrent, ramp.finalCurrent) with
            | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
            | (s, 0.0<A>) -> currentDirection s
            | (_, f) -> currentDirection f

        /// <summary>
        /// Determines the (unsigned) upper current limit for a <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let upperCurrentLimit ramp = max (abs ramp.startingCurrent) (abs ramp.finalCurrent)

        /// <summary>
        /// Determines the (unsigned) lower current limit for a <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let lowerCurrentLimit ramp = min (abs ramp.startingCurrent) (abs ramp.finalCurrent)

        /// <summary>
        /// Determines the starting <see cref="MagnetController.RampTarget" /> for a
        /// <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let startingRampTarget ramp =
            match (abs ramp.startingCurrent, abs ramp.finalCurrent) with
            | (0.0<A>, _) -> Zero
            | (s, f) when s <= f -> Lower
            | _ -> Upper

        /// <summary>
        /// Determines the starting <see cref="MagnetController.RampTarget" /> for a
        /// <see cref="MagnetRampManager.Ramp" />.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        let finalRampTarget ramp = 
            match (abs ramp.startingCurrent, abs ramp.finalCurrent) with
            | (_, 0.0<A>) -> Zero
            | (s, f) when f >= s -> Upper
            | _ -> Lower
        
        /// <summary>
        /// Returns an asynchronous workflow which performs a <see cref="MagnetRampManager.Ramp" />
        /// and relays <see cref="MagnetRampManager.RampStatus" /> information via an observer.
        /// </summary>
        /// <param name="ramp">The ramp.</param>
        /// <param name="statusObserver">The <see cref="MagnetRampManager.RampStatus" /> observer.</param>
        let asyncRamp ramp (statusObserver : IObserver<RampStatus>) = 
            
            /// <summary>
            /// An asynchronous workflow which sets the starting current direction, ramping the magnet
            /// controller to zero current if necessary.
            /// </summary>
            /// <param name="initialState">The <see cref="MagnetController.State" /> before the ramp was
            /// started.</param>
            let setStartingCurrentDirection initialState = async {
                if not (initialState.operatingParameters.currentDirection = startingCurrentDirection ramp)
                then do! rampToZeroAndSetCurrentDirection (startingCurrentDirection ramp) magnetController }
            
            /// <summary>
            /// An asynchronous workflow which sets the upper and lower current limits of the the magnet
            /// controller as required by the ramp.
            /// </summary>
            /// <param name="initialState">The <see cref="MagnetController.State" /> before the ramp was
            /// started.</param>
            let setCurrentLimits initialState = async { 
                magnetController.Post <| SetPause(true)
                let setLowerLimit = fun () -> magnetController.Post <| SetLowerSetPoint (lowerCurrentLimit ramp)
                let setUpperLimit = fun () -> magnetController.Post <| SetUpperSetPoint (upperCurrentLimit ramp)
                
                // If the initial lower current limit is larger than the new upper current limit for the ramp,
                // then the lower current limit must be changed first or the update to the upper current limit 
                // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
                // than the initial lower current limit) is smaller than the new lower current limit, then the
                // upper current limit must be changed first.
                if initialState.setPointParameters.lowerLimit >= upperCurrentLimit ramp
                then setLowerLimit() ; setUpperLimit()
                else setUpperLimit() ; setLowerLimit() }
            
            /// <summary>
            /// An asynchronous workflow which ramps the magnet controller to the iniital current once the
            /// correct current direction and current limits are set.
            /// </summary>
            let rampToInitialCurrent = async {
                magnetController.Post <| SetPause(true)
                magnetController.Post <| SetRampTarget(startingRampTarget ramp)
                setMaximumRampRate magnetController
                magnetController.Post <| SetPause(false)
                do! waitToReachTarget magnetController } 
            
            /// <summary>
            /// An asynchronous workflow which prepares the magnet controller for the ramp.
            /// </summary>        
            let prepareForRamp = async { 
                statusObserver.OnNext(PreparingForRamp)
                let! initialState = getState magnetController
                do! setStartingCurrentDirection initialState
                do! setCurrentLimits initialState 
                do! rampToInitialCurrent }

            /// <summary>
            /// An asynchronous workflow waits for the observable represented by the readyForRamp field on 
            /// the <see cref="MagnetRampManager.Ramp" /> to become true.  
            /// </summary> 
            let awaitReadyForRamp = async {
                statusObserver.OnNext(ReadyToRamp)
                let! ready = ramp.readyForRamp
                             |> Observable.filter (fun ready -> ready)
                             |> Async.AwaitObservable
                ready |> ignore }
            
            /// <summary>
            /// An asynchronous workflow which performs the ramp once it has been prepared, ramping through
            /// zero if needed
            /// </summary> 
            let performRamp = async { 
                statusObserver.OnNext(PerformingRamp)
                magnetController.Post <| SetRampRate(ramp.rampRate)
                if not (startingCurrentDirection ramp = finalCurrentDirection ramp)
                then magnetController.Post <| SetRampTarget(Zero)
                     do! prepareToFlipCurrentDirection magnetController
                     magnetController.Post <| SetCurrentDirection(finalCurrentDirection ramp)
                
                magnetController.Post <| SetRampTarget(finalRampTarget ramp) 
                do! waitToReachTarget magnetController }

            async {
                // if the ramp is cancelled, relay this via the status observer.
                use! cancelHandler = Async.OnCancel(fun () -> statusObserver.OnNext(CancelledRamp)) 

                try do! prepareForRamp
                    do! awaitReadyForRamp
                    do! performRamp

                    if ramp.returnToZero
                    then do! rampToZero magnetController
                    statusObserver.OnNext(FinishedRamp)
                finally statusObserver.OnCompleted() }

        /// <summary>
        /// Returns an asynchronous workflow for the the mailbox behaviour in the waiting state.
        /// </summary>
        let rec waiting() = async {
            let! message = mailbox.Receive()
            match message with
            | PerformRamp(ramp, replyChannel) -> 
                let rampStatus = new BehaviorSubject<RampStatus>(Initialising)
                replyChannel.Reply(rampStatus.AsObservable())
                do! ramping ramp rampStatus
            | CancelRamp(_) -> failwith "Attempted to cancel a magnet controller ramp when none is in progress" }

        /// <summary>
        /// Returns an asynchronous workflow for the the mailbox behaviour in the ramping state.
        /// </summary>
        /// <param name="ramp">The ramp which is being performed.</param>
        /// <param name="rampStatus">The <see cref="BehaviourSubject<RampStatus>" used to relay
        /// status information to the requester via an observable.</param>
        and ramping ramp rampStatus = async {
            use rampCts = new CancellationTokenSource()
            let rampCompleted = rampStatus.TakeLast(1)
            let rampCancelled = Observable.filter (fun s -> s = CancelledRamp) rampStatus

            let stopRamp returnToZero = 
                if returnToZero
                then Async.RunSynchronously(rampToZero magnetController)
                else magnetController.Post <| SetPause(true)
            
            /// <summary>
            /// Handles a receive message to cancel the ramp in progress, ramping to zero current if requested.
            /// </summary>
            let receiveCancelMessage _ = 
                let message = Async.RunSynchronously(mailbox.Receive())
                match message with 
                | CancelRamp(returnToZero) -> 
                    Observable.add (fun _ -> stopRamp returnToZero) rampCancelled
                    rampCts.Cancel()
                | _ -> failwith "Attempted to start a magnet controller ramp when one is already in progress"

            // create a subscription which will check the agent mailbox for cancellation requests every
            // 100ms until the ramp is completed
            use _ = Observable.Interval(TimeSpan.FromMilliseconds(100.0)) // every 100ms
                              .TakeUntil(rampCompleted) // until ramp is completed
                              .Select(fun _ -> mailbox.CurrentQueueLength) // check the mailbox queue length
                              .Where(fun queueLength -> not (queueLength = 0)) // if it is non-zero
                              .Take(1) // then the first time this occurs
                    |> Observable.subscribe receiveCancelMessage // call the receiveCancelMessage function
            
            // run the asyncRamp computation expression, and ignore exceptions due to cancellation
            try Async.RunSynchronously(asyncRamp ramp rampStatus, -1, rampCts.Token)
            with :? OperationCanceledException -> ()

            // return to the waiting state
            printfn "Going back to the waiting state!"
            do! waiting() }

        waiting ()