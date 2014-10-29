namespace Endorphin.Instrument.TwickenhamSmc

open Microsoft.FSharp.Control
open System
open System.Threading
open System.Reactive.Linq
open System.Reactive.Subjects
open FSharp.Control.Observable
open TwickenhamSmcFSharp

/// <summary>
/// Record type describing the parameters of a ramp. Note that the starting and final currents
/// are signed.
/// </summary>
type Ramp = 
    { /// <summary>Signed starting current for the ramp in A.</summary>
      startingCurrentInAmps : float
      /// <summary>Signed final current for the ramp in A.</summary>
      finalCurrentInAmps : float
      /// <summary>Signed ramp rate for the ramp in A/s.</summary>
      rampRateInAmpsPerSec : float
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
    /// <summary>
    /// Causes the magnet controller ramp manager to perform a ramp with the specified parameters and returns
    /// a <see cref="System.IObservable" /> which provides ramp status notifications while the ramp is being
    /// performed.
    /// </summary>
    | PerformRamp of ramp : Ramp * replyChannel : AsyncReplyChannel<IObservable<RampStatus>>
    /// <summary>
    /// Cancels a ramp which is in progress with a parameter indicating whether the magnet controller should
    /// return to zero current.
    /// </summary>
    | CancelRamp of returnToZero : bool


type MagnetRampAgent(magnetController : MagnetController) =

    /// <summary>
    /// Actor mailbox for a magnet ramp manager which performs ramp operations on a Twickenham
    /// superconducting magnet controller.
    /// </summary>
    /// <param name="magnetController">The magnet controller mailbox.</param>
    let mailboxProcessor = 
        (fun (mailbox : MailboxProcessor<Command>) ->

            /// <summary>
            /// Returns a <see cref="MagnetController.CurrentDirection" /> object for a signed current.
            /// If the current is zero, the direction cannot be the determined and the function throws
            /// an exception.
            /// </summary>
            /// <param name="current">The signed current value.</param>
            let currentDirection current =
                match current with
                | c when c > 0.0 -> Forward
                | c when c < 0.0 -> Reverse
                | _ -> failwith "Magnet controller direction for zero current is arbitrary."
        
            /// <summary>
            /// Determines the starting current direction for a <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let startingCurrentDirection ramp = 
                match (ramp.startingCurrentInAmps, ramp.finalCurrentInAmps) with
                | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
                | (0.0, f) -> currentDirection f
                | (s, _) -> currentDirection s
        
            /// <summary>
            /// Determines the final current direction for a <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let finalCurrentDirection ramp =
                match (ramp.startingCurrentInAmps, ramp.finalCurrentInAmps) with
                | (s, f) when s = f -> failwith "Ramp starting and final current are the same."
                | (s, 0.0) -> currentDirection s
                | (_, f) -> currentDirection f

            /// <summary>
            /// Determines the (unsigned) upper current limit for a <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let upperCurrentLimitInAmps ramp = max (abs ramp.startingCurrentInAmps) (abs ramp.finalCurrentInAmps)

            /// <summary>
            /// Determines the (unsigned) lower current limit for a <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let lowerCurrentLimitInAmps ramp = min (abs ramp.startingCurrentInAmps) (abs ramp.finalCurrentInAmps)

            /// <summary>
            /// Determines the starting <see cref="MagnetController.RampTarget" /> for a
            /// <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let startingRampTarget ramp =
                match (abs ramp.startingCurrentInAmps, abs ramp.finalCurrentInAmps) with
                | (0.0, _) -> Zero
                | (s, f) when s <= f -> Lower
                | _ -> Upper

            /// <summary>
            /// Determines the starting <see cref="MagnetController.RampTarget" /> for a
            /// <see cref="MagnetRampManager.Ramp" />.
            /// </summary>
            /// <param name="ramp">The ramp.</param>
            let finalRampTarget ramp = 
                match (abs ramp.startingCurrentInAmps, abs ramp.finalCurrentInAmps) with
                | (_, 0.0) -> Zero
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
                    setPause true magnetController
                    let setLowerLimit = fun () -> setLowerSetPoint (lowerCurrentLimitInAmps ramp) magnetController
                    let setUpperLimit = fun () -> setUpperSetPoint (upperCurrentLimitInAmps ramp) magnetController
                
                    // If the initial lower current limit is larger than the new upper current limit for the ramp,
                    // then the lower current limit must be changed first or the update to the upper current limit 
                    // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
                    // than the initial lower current limit) is smaller than the new lower current limit, then the
                    // upper current limit must be changed first.
                    if initialState.setPointParameters.lowerLimitInAmps >= upperCurrentLimitInAmps ramp
                    then setLowerLimit() ; setUpperLimit()
                    else setUpperLimit() ; setLowerLimit() }
            
                /// <summary>
                /// An asynchronous workflow which ramps the magnet controller to the iniital current once the
                /// correct current direction and current limits are set.
                /// </summary>
                let rampToInitialCurrent = async {
                    setRampTarget (startingRampTarget ramp) magnetController
                    setRampRateInAmpsPerSec magnetController.MaximumRampRateInAmpsPerSec magnetController
                    setPause false magnetController
                    do! waitToReachTarget magnetController } 
            
                /// <summary>
                /// An asynchronous workflow which prepares the magnet controller for the ramp.
                /// </summary>        
                let prepareForRamp = async { 
                    statusObserver.OnNext(PreparingForRamp)
                    let! initialState = magnetControllerState magnetController
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
                    setRampRateInAmpsPerSec ramp.rampRateInAmpsPerSec magnetController
                    if not (startingCurrentDirection ramp = finalCurrentDirection ramp)
                    then setRampTarget Zero magnetController
                         do! rampToZeroAndSetCurrentDirection (finalCurrentDirection ramp)  magnetController
                
                    setRampTarget (finalRampTarget ramp) magnetController 
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
                    rampStatus.AsObservable() |> replyChannel.Reply
                    return! ramping ramp rampStatus
                | CancelRamp(_) -> 
                    // it's possible that the message arrives just after a ramp has finished so just ignore it
                    return! waiting() }

            /// <summary>
            /// Returns an asynchronous workflow for the the mailbox behaviour in the ramping state.
            /// </summary>
            /// <param name="ramp">The ramp which is being performed.</param>
            /// <param name="rampStatus">The <see cref="BehaviourSubject<RampStatus>" used to relay
            /// status information to the requester via an observable.</param>
            and ramping ramp rampStatus = async {
                do! 
                    use rampCts = new CancellationTokenSource()
                    use mailboxCts = new CancellationTokenSource()
                    use cancelMailboxToken = 
                        rampStatus.IgnoreElements()
                        |> Observable.subscribe (fun _ -> mailboxCts.Cancel())
                    
                    let stopRamp returnToZero = async { 
                        if returnToZero
                        then do! rampToZero magnetController
                        else setPause true magnetController }
            
                    /// <summary>
                    /// Handles a receive message to cancel the ramp in progress, ramping to zero current if requested.
                    /// </summary>
                    let rec receiveCancelMessage() = async {
                        if (not mailboxCts.IsCancellationRequested)
                        then if mailbox.CurrentQueueLength <> 0
                             then let! message = mailbox.Receive()
                                  match message with 
                                  | CancelRamp(returnToZero) -> 
                                      rampStatus
                                      |> Observable.filter(fun status -> status = CancelledRamp)
                                      |> Observable.add (fun _ -> Async.Start(stopRamp returnToZero))
                                      rampCts.Cancel()
                                  | _ -> failwith "Attempted to start a magnet controller ramp when one is already in progress"
                             else do! Async.Sleep(100)
                                  do! receiveCancelMessage() }
            
                    // run the asyncRamp computation expression
                    Async.Start(asyncRamp ramp rampStatus, rampCts.Token)

                    // return to the waiting state once the ramp is complete
                    receiveCancelMessage()
                return! waiting() }

            waiting ())
        |> MailboxProcessor.Start

    member this.PerformRamp(ramp) =
        fun replyChannel -> PerformRamp(ramp, replyChannel)
        |> mailboxProcessor.PostAndReply
    
    member this.CancelRamp(returnToZero) =
        CancelRamp(returnToZero)
        |> mailboxProcessor.Post

module MagnetRampFSharp =
    let performRamp ramp (magnetRampAgent : MagnetRampAgent) =
        magnetRampAgent.PerformRamp(ramp)

    let cancelRamp returnToZero (magnetRampAgent : MagnetRampAgent) =
        magnetRampAgent.CancelRamp(returnToZero)