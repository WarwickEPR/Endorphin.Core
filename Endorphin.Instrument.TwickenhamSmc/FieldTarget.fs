namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core

/// Functions related to ramping a magnet controller to a specified field target.
module FieldTarget =

    /// Defines the parameters for a target magnetic field.
    type FieldTargetParameers =
        private { TargetStepIndex  : uint16
                  CurrentDirection : CurrentDirection
                  RampRateIndex    : int }
       
    /// Indicates progress as a magnet controller ramps towards a target magnetic field.
    type FieldTargetStatus =
        | PreparingTarget
        | RampingToTarget
        | FinishedRampToTarget
        | CancelledRampToTarget

    /// Contains the result of ramping a magnet controller to a target magnetic field indicating
    /// whether the target was reached successfully, failed or was cancelled.
    type FieldTargetResult =
        | TargetReached
        | TargetError of exn
        | TargetCancelled
    
    /// Defines a magnetic field target which is associated with a magnet controller. This is
    /// created after the connection to the magnet controller has been established.
    type FieldTarget =
        private { Parameters       : FieldTargetParameers
                  MagnetController : MagnetController
                  StatusChanged    : NotificationEvent<FieldTargetStatus> }

    /// Represents a handle to a magnetic field target to which a magnet controller has been set
    /// to ramp. This can be used to wait for the field target to be reached.
    type FieldTargetHandle =
        private { FieldTarget : FieldTarget
                  WaitHandle  : Async<FieldTargetResult> }
    
    /// Functions for specifying magnetic field target parameters.
    module Parameters =

        /// Defines magnetic field target parameters.
        let create currentDirection stepIndex rampRateIndex =
            { TargetStepIndex  = stepIndex
              CurrentDirection = currentDirection
              RampRateIndex    = rampRateIndex }

        /// Returns modified magnetic field target parameters with the specified ramp rate index.
        let withRampRateIndex rampRateIndex parameters =
            { parameters with RampRateIndex = rampRateIndex }
            
        /// Returns the current direction of the given magnetic field target parameters.
        let currentDirection parameters = parameters.CurrentDirection

        /// Returns the digital output step index of the given magnetic field target parameters.
        let stepIndex parameters = parameters.TargetStepIndex

        /// Returns the magnetic field corresponding to the field target parameters on the given
        /// magnet controller.
        let field magnetController parameters =
            (currentDirection parameters, stepIndex parameters)
            |> MagnetController.Output.MagneticField.fromStepIndex magnetController
           
        /// Returns the output current corresponding to the field target parameters on the given
        /// magnet controller.    
        let current magnetController =
            stepIndex >> MagnetController.Output.Current.fromStepIndex magnetController
        
        /// Returns the monitoring shunt voltage readout corresponding to the field target
        /// parameters on the given magnet controller.
        let shuntVoltage magnetController =
            stepIndex >> MagnetController.Output.ShuntVoltage.fromStepIndex magnetController

        /// Returns the ramp rate index corresponding to the magnetic field target parameters.
        let rampRateIndex parameters = parameters.RampRateIndex
        
        /// Returns the ramp rate corresponding to the magnetic field target parameters on the
        /// given magnet controller.
        let rampRate magnetController =
            rampRateIndex >> MagnetController.Ramp.Rate.fromIndex magnetController
    
    /// Creates a magnetic field target for the given magnet controller with the specified
    /// parameters.
    let create magnetController parameters =
        { Parameters       = parameters
          MagnetController = magnetController
          StatusChanged    = new NotificationEvent<_>() }

    /// Returns an observable which fires when the magnetic field target status changes.
    let status target =
        target.StatusChanged.Publish
        |> Observable.fromNotificationEvent

    /// Sets the magnet controller set-points and ramp rate in preparation for ramping to a
    /// magnetic field, leaving the device in a paused state.
    let private prepare target = async {
        target.StatusChanged.Trigger (Next <| PreparingTarget)
        let magnetController = target.MagnetController
        
        // first pause the magnet controller so that the settings can be set without causing the field to ramp
        do! MagnetController.Ramp.setPause magnetController true
        do! MagnetController.Ramp.Rate.setByIndex magnetController (target.Parameters |> Parameters.rampRateIndex)
             
        // set the lower set point to the minimum value so it is not lower than the upper set point value, which would
        // prevent the upper set point from being set
        do! MagnetController.Output.Current.setLowerSetPointIndex magnetController 0us
        do! MagnetController.Output.Current.setUpperSetPointIndex magnetController (target.Parameters |> Parameters.stepIndex) }
    
    /// Proceeds to the magnetic field target once the magnet controller settings are prepared.
    let private proceedToTarget target = async {
        target.StatusChanged.Trigger (Next <| RampingToTarget)
        let magnetController = target.MagnetController

        // change the current polarity if necessary
        let! operatingParameters = MagnetController.queryOperatingParameters magnetController
        if (target.Parameters |> Parameters.currentDirection) <> operatingParameters.CurrentDirection then
            do! MagnetController.Ramp.setTarget magnetController Zero
            do! MagnetController.Ramp.setPause magnetController false
            do! MagnetController.Ramp.waitToReachZero magnetController 
            do! MagnetController.Output.setDirection magnetController (target.Parameters |> Parameters.currentDirection)
        
        // set the ramp target, unpause and wait to reach it
        do! MagnetController.Ramp.setTarget magnetController Upper
        do! MagnetController.Ramp.setPause magnetController false
        do! MagnetController.Ramp.waitToReachTarget magnetController } 
    
    /// Ramps to the specified magnetic field target, leaving the magnet controller in a paused
    /// state if cancellation occurs.
    let private goToTarget target = async {
        use! __ = Async.OnCancel(fun () -> MagnetController.Ramp.setPause target.MagnetController true |> Async.StartImmediate)
        do! prepare target
        do! proceedToTarget target }

    /// Ramps to a magnetic field target with the given cancellation token. If the token is
    /// cancelled, then the ramp will be aborted and the magnet controller will be left in a
    /// paused state. To ensure that the ramp is stopped cleanly, no other cancellation 
    /// compensations should be registered with this cancellation token which will post commands
    /// to the magnet controlller. Returns a handle which can be used to wait for the target to
    /// be reached asynchronously.
    let goWithCancellationToken target cancellationToken =
        let resultChannel = new ResultChannel<_>()
        let statusChanged target = target.StatusChanged

        Async.StartWithContinuations(
            goToTarget target,
            (fun () ->  // success
                target.StatusChanged.Trigger (Next <| FinishedRampToTarget)
                target.StatusChanged.Trigger Completed
                resultChannel.RegisterResult TargetReached),
            (fun exn -> // error
                target.StatusChanged.Trigger (Error exn)
                resultChannel.RegisterResult (TargetError exn)),
            (fun _ -> // cancellation
                target.StatusChanged.Trigger (Next <| CancelledRampToTarget)
                target.StatusChanged.Trigger Completed
                resultChannel.RegisterResult TargetCancelled),
            cancellationToken)

        { FieldTarget = target
          WaitHandle  = resultChannel.AwaitResult () }

    /// Ramps to a magnetic field target with the default cancellation token. If the ability to
    /// abort the ramp with a cancellation token is required then use goWithCancellationToken
    /// instead. Returns a handle which can be used to wait for the target to be reached
    /// asynchronously.
    let go target = goWithCancellationToken target Async.DefaultCancellationToken

    /// Ramps to a magnetic field target as a child (sharing the same cancellation token) to the
    /// current asynchronous workflow. If the token is cancelled, then the ramp will be aborted
    /// and the magnet controller will be left in a paused state. To ensure that the ramp is
    /// stopped cleanly, no other cancellation  compensations should be registered with this
    /// cancellation token which will post commands to the magnet controlller. Returns a handle
    /// which can be used to wait for the target to be reached asynchronously.
    let goAsChild target = async {
        let! ct = Async.CancellationToken
        return goWithCancellationToken target ct }

    /// Asynchronously waits for the magnetic field target workflow associated with the handle to
    /// complete with success, failure or cancellation.
    let waitToFinish targetHandle = targetHandle.WaitHandle