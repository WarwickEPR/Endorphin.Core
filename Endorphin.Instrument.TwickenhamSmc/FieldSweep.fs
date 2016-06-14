// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.TwickenhamSmc

open System.Threading

open Endorphin.Core

/// Functions related to performing a magnetic field sweep over a specified range with the magnet
/// controller.
module FieldSweep = 
    
    /// Defines the parameters for a magnetic field sweep.
    type FieldSweepParameters =
        private { InitialStepIndex : uint16
                  FinalStepIndex   : uint16
                  CurrentDirection : CurrentDirection
                  RampRateIndex    : int }
      
    /// Indicates progress as a magnet controller performs a magnetic field sweep.
    type FieldSweepStatus =
        | PreparingSweep
        | PreparedToSweep
        | Sweeping
        | FinishedSweep
        | CancelledSweep

    /// Contains the result of a magnetic field sweep, indicating whether the it was completed
    /// successfully, failed or cancelled.
    type FieldSweepResult =
        | SweepCompleted
        | SweepError of exn
        | SweepCancelled

    /// Defines a magnetic field sweep which is associated with a magnet controller. This is
    /// created after the connection to the magnet controller has been established.
    type FieldSweep =
        private { Parameters       : FieldSweepParameters
                  MagnetController : MagnetController
                  ReadyToSweep     : ManualResetEvent
                  StatusChanged    : NotificationEvent<FieldSweepStatus> }

    /// Represents a handle to a magnetic field sweep which is currently being performed by a
    /// magnet controller. This can be used to wait for the field sweep to complete.
    type FieldSweepHandle =
        private { FieldSweep : FieldSweep
                  WaitHandle : Async<FieldSweepResult> }
    
    /// Functions for specifying magnetic field sweep parameters.
    module Parameters = 

        /// Creates a magnetic field sweep
        let create currentDirection initialStepIndex finalStepIndex rampRateIndex =
            if initialStepIndex = finalStepIndex then
                invalidArg "finalStepIndex" (sprintf "Final step index cannot be equal to initial step index: %d." finalStepIndex)

            { InitialStepIndex = initialStepIndex
              FinalStepIndex   = finalStepIndex
              CurrentDirection = currentDirection
              RampRateIndex    = rampRateIndex }

        /// Returns modified magnetic field sweep parameters with the specified ramp rate index.
        let withRampRate rampRateIndex parameters =
            { parameters with RampRateIndex = rampRateIndex }

        /// Returns the current direction of the magnetic field sweep parameters.
        let currentDirection parameters = parameters.CurrentDirection

        /// Returns the initial digital output step index of the magnetic field sweep parameters.
        let initialStepIndex parameters = parameters.InitialStepIndex

        /// Returns the initial magnetic field corresponding to the magnetic field sweep
        /// parameters on the given magnet controller.
        let initialField magnetController parameters =
            (currentDirection parameters, initialStepIndex parameters)
            |> MagnetController.Output.MagneticField.fromStepIndex magnetController
        
        /// Returns the initial output current corresponding to the magnetic field sweep
        /// parameters on the given magnet controller.
        let initialCurrent magnetController =
            initialStepIndex >> MagnetController.Output.Current.fromStepIndex magnetController
           
        /// Returns the initial monitoring shunt voltage corresponding to the magnetic field
        /// sweep parameters on the given magnet controller.
        let initialShuntVoltage magnetController =
            initialStepIndex >> MagnetController.Output.ShuntVoltage.fromStepIndex magnetController

        /// Returns the final digital output step index of the magnetic field sweep parameters.
        let finalStepIndex parameters = parameters.FinalStepIndex

        /// Returns the final magnetic field corresponding to the magnetic field sweep parameters
        /// on the given magnet controller.
        let finalField magnetController parameters =
            (currentDirection parameters, finalStepIndex parameters)
            |> MagnetController.Output.MagneticField.fromStepIndex magnetController

        /// Returns the final output current corresponding to the magnetic field sweep parameters
        /// on the given magnet controller.
        let finalCurrent magnetController =
            finalStepIndex >> MagnetController.Output.Current.fromStepIndex magnetController

        /// Returns the final monitoring shunt voltage corresponding to the magnetic field sweep
        /// parameters on the given magnet controller.
        let finalShuntVoltage magnetController =
            finalStepIndex >> MagnetController.Output.ShuntVoltage.fromStepIndex magnetController

        /// Returns the number of digital output steps in the magnetic field sweep defined by the
        /// given sweep parameters.
        let numberOfSteps parameters = 
            match (initialStepIndex parameters, finalStepIndex parameters) with
            | (i, f) when f > i -> f - i
            | (i, f)            -> i - f 

        /// Returns the difference between initial and final output current in the magnetic field
        /// sweep defined by the sweep parameters on the given magnet controller.
        let currentChange magnetController parameters =
            numberOfSteps parameters
            |> MagnetController.Output.Current.fromStepIndex magnetController

        /// Returns the difference between initial and final magnetic field in the sweep defined
        /// by the sweep parameters on the given magnet controller.
        let fieldChange magnetController parameters =
            (currentChange magnetController parameters)
            * (MagnetController.Output.MagneticField.linearCoefficient magnetController)

        /// Returns the initial magnet controller ramp target in a magnetic field sweep defined
        /// by the given parameters.
        let internal initialRampTarget parameters = 
            match (initialStepIndex parameters, finalStepIndex parameters) with
            | (i, _) when i = 0us -> Zero
            | (i, f) when i < f   -> Lower
            | (_, _)              -> Upper

        /// Returns the final magnet controller ramp target in a magnetic field sweep defined by
        /// the given parameters.
        let internal finalRampTarget parameters =
            match (initialStepIndex parameters, finalStepIndex parameters) with
            | (_, f) when f = 0us -> Zero
            | (i, f) when i < f   -> Upper
            | (_, _)              -> Lower

        /// Returns the lower digital output step index in a magnetic field sweep defined by the
        /// given parameters.
        let internal lowerStepIndex parameters = min (initialStepIndex parameters) (finalStepIndex parameters)

        /// Returns the upper digital output step index in a magnetic field sweep defined by the
        /// given pararmeters.
        let internal upperStepIndex parameters = max (initialStepIndex parameters) (finalStepIndex parameters)

        /// Returns the ramp rate index used while ramping the magnet controller between the
        /// initial and final ramp targets for the sweep defined by the given parameters. Note
        /// that the maximum available ramp rate on the magnet controller is used while preparing
        /// the sweep.
        let rampRateIndex target = target.RampRateIndex
            
        /// Returns the ramp rate used while ramping the magnet controller between the initial and
        /// final ramp targets for the sweep defined by the given parameters. Note that the
        /// maximum available ramp rate on the magnet controller is used while preparing the sweep.
        let rampRate magnetController =
            rampRateIndex >> MagnetController.Ramp.Rate.fromIndex magnetController

        /// Returns the estimated duration for the magnetic field sweep on the given magnet
        /// controller, not including preparation time.
        let duration magnetController parameters =
            (currentChange magnetController parameters)
            / (rampRate magnetController parameters)
    
    /// Creates a magnetic field sweep for the given magnet controller with the specified
    /// parameters.
    let create magnetController parameters =
        { Parameters       = parameters
          MagnetController = magnetController
          ReadyToSweep     = new ManualResetEvent(false)
          StatusChanged    = new NotificationEvent<_>() }
    
    /// Returns the parameters for a field sweep.
    let parameters sweep = sweep.Parameters

    /// Returns an observable which fires when the magnetic field sweep status changes.
    let status sweep =
        sweep.StatusChanged.Publish
        |> Observable.fromNotificationEvent

    /// Sets the magnet controller set-points and ramp rate in preparation for a magnetic field
    /// sweep, leaving the device in a paused state.
    let private setUp sweep = async {
        sweep.StatusChanged.Trigger (Next PreparingSweep)
        let magnetController = sweep.MagnetController

        do! MagnetController.Ramp.setPause magnetController true
        do! MagnetController.Ramp.Rate.setToMaximum magnetController
             
        let lowerStepIndex = Parameters.lowerStepIndex sweep.Parameters
        let upperStepIndex = Parameters.upperStepIndex sweep.Parameters

        // If the initial lower current limit is larger than the new upper current limit for the ramp,
        // then the lower current limit must be changed first or the update to the upper current limit 
        // will be ignored. Similarly, if the initial upper current limit (which is inevitably larger
        // than the initial lower current limit) is smaller than the new lower current limit, then the
        // upper current limit must be changed first.
        let! setPointParameters = MagnetController.querySetPointParameters magnetController
        let stepIndex = MagnetController.Output.Current.toStepIndex magnetController setPointParameters.LowerSetPoint
        if stepIndex >= Parameters.upperStepIndex sweep.Parameters then
            do! MagnetController.Output.Current.setLowerSetPointIndex magnetController lowerStepIndex
            do! MagnetController.Output.Current.setUpperSetPointIndex magnetController upperStepIndex
        else
            do! MagnetController.Output.Current.setUpperSetPointIndex magnetController upperStepIndex
            do! MagnetController.Output.Current.setLowerSetPointIndex magnetController lowerStepIndex }
    
    /// Ramps the magnet controller to the initial ramp target at the maximum available
    /// ramp rate in preparation for a magnetic field sweep. Leaves the magnet controlelr
    /// prepared to sweep with the ramp rate specified by the field sweep parameters.
    let private rampToInitialTarget sweep = async {
        let magnetController = sweep.MagnetController

        let! operatingParameters = MagnetController.queryOperatingParameters magnetController
        if Parameters.currentDirection sweep.Parameters <> operatingParameters.CurrentDirection then
            do! MagnetController.Ramp.setTarget magnetController Zero
            do! MagnetController.Ramp.setPause magnetController false
            do! MagnetController.Ramp.waitToReachZero magnetController
            do! MagnetController.Output.setDirection magnetController (Parameters.currentDirection sweep.Parameters)
            
        do! MagnetController.Ramp.setTarget magnetController (Parameters.initialRampTarget sweep.Parameters)
        do! MagnetController.Ramp.setPause magnetController false
        do! MagnetController.Ramp.waitToReachTarget magnetController
        do! MagnetController.Ramp.Rate.setByIndex magnetController (Parameters.rampRateIndex sweep.Parameters)
                
        sweep.StatusChanged.Trigger (Next PreparedToSweep) } 
    
    /// Waits for the ready-to-sweep flag to be set on the magnetic field sweep, then the
    /// magnet controller to the final ramp target.
    let private rampToFinalTarget sweep = async {
        do! Async.AwaitWaitHandle sweep.ReadyToSweep |> Async.Ignore
        sweep.StatusChanged.Trigger (Next Sweeping)
        do! MagnetController.Ramp.setTarget sweep.MagnetController (Parameters.finalRampTarget sweep.Parameters)
        do! MagnetController.Ramp.waitToReachTarget sweep.MagnetController } 

    /// Performs a magnetic field sweep, leaving the magnet controller in a paused state if
    /// cancellation occurs.
    let private performSweep sweep = async {
        use! __ = Async.OnCancel(fun () -> MagnetController.Ramp.setPause sweep.MagnetController true |> Async.StartImmediate)
        do! setUp sweep
        do! rampToInitialTarget sweep
        do! rampToFinalTarget sweep }

    /// Prepares a magnetic field sweep with the given cancellation token. The ready-to-start
    /// flag has to be set before the sweep will proceed to the final ramp target once prepared.
    /// If the token is cancelled, then the sweep will be aborted and the magnet controller will
    /// be left in a paused state. To ensure that the sweep is stopped cleanly, no othercancellation 
    /// compensations should be registered with this cancellation token which will post commands
    /// to the magnet controlller. Returns a handle which can be used to wait for the sweep to
    /// be completed asynchronously.
    let prepareWithCancellationToken sweep cancellationToken =
        let resultChannel = new ResultChannel<_>()

        Async.StartWithContinuations(
            performSweep sweep,
            (fun () -> 
                sweep.StatusChanged.Trigger (Next <| FinishedSweep)
                sweep.StatusChanged.Trigger Completed
                resultChannel.RegisterResult SweepCompleted),
            (fun exn ->
                sweep.StatusChanged.Trigger (Error exn)
                resultChannel.RegisterResult (SweepError exn)),
            (fun _ -> 
                sweep.StatusChanged.Trigger (Next <| CancelledSweep)
                sweep.StatusChanged.Trigger Completed
                resultChannel.RegisterResult SweepCancelled),
            cancellationToken)

        { FieldSweep = sweep
          WaitHandle = resultChannel.AwaitResult () }

    /// Performs a magnetic field sweep with the default cancellation token. The ready-to-start flag has 
    /// to be set before the sweep will proceed to the final ramp target once prepared. If the ability to
    /// abort the sweep with a cancellation token is required then use prepareWithCancellationToken
    /// instead. Returns a handle which can be used to wait for the sweep to be completed asynchronously.
    let prepare sweep = 
        prepareWithCancellationToken sweep Async.DefaultCancellationToken

    /// Performs a magnetic field sweep as a child (sharing the same cancellation token) to the current
    /// asynchronous workflow. The ready-to-start flag has  to be set before the sweep will proceed to the
    /// final ramp target once prepared. If the token is cancelled, then the sweep will be aborted and the
    /// magnet controller will be left in a paused state. To ensure that the sweep is stopped cleanly, no
    /// other cancellation compensations should be registered with this cancellation token which will post
    /// commands to the magnet controlller. Returns a handle which can be used to wait for the sweep to be
    /// completed asynchronously.
    let prepareAsChild sweep = async {
        let! ct = Async.CancellationToken
        return prepareWithCancellationToken sweep ct }
    
    /// Sets the ready-to-sweep flag on the magnetic field sweep so that it will proceed to the
    /// final ramp target once prepared.
    let setReadyToSweep sweep =
        sweep.ReadyToSweep.Set() |> ignore

    /// Asynchronously waits for the magnetic field sweep workflow associated with the handle to
    /// complete with success, failure or cancellation.
    let waitToFinish sweepHandle = sweepHandle.WaitHandle

    /// Asynchronously waits for the magnetic field sweep to be prepared at the initial field.
    let waitToPrepare sweep = 
        status sweep
        |> Observable.filter ((=) PreparedToSweep)
        |> Async.AwaitObservable
        |> Async.Ignore
