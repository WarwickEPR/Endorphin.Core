namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open log4net
open Microsoft.FSharp.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NationalInstruments.VisaNS
open System
open System.Reactive.Linq

/// Represents the set of messages which can be sent to a Twickenham MagnetController agent mailbox. This is only used internally in
/// this assembly. The functionality is exposed by the MagnetController class members which queue messages to the agent mailbox and, 
/// where appropriate, return asynchronous workflows to await the agent's response.
type internal Message = 
    | GetOutputParameters of replyChannel : AsyncReplyChannel<OutputParameters>
    | GetCurrentParameters of replyChannel : AsyncReplyChannel<CurrentParameters>
    | GetOperatingParameters of replyChannel : AsyncReplyChannel<OperatingParameters>
    | GetSetPointParameters of replyChannel : AsyncReplyChannel<SetPointParameters>
    | SetRampRate of rampRate: float<A/s>
    | SetTripVoltage of tripVoltage : float<V>
    | SetCurrentDirection of currentDirection : CurrentDirection
    | SetLowerSetPoint of lowerSetPoint : float<A>
    | SetUpperSetPoint of upperSetPoint : float<A>
    | SetRampTarget of rampTarget : RampTarget
    | SetPause of pause : bool
    | ReleaseSession 

/// Provides a control interface for sending messages to a Twickenham magnet controller device once a connection has been opened by
/// instantiating a MagnetControllerSession object. This control interface is instantiated by requesting control of the hardware from
/// that object. Internally, the control interface uses a MailboxProcessor agent to send commands to the hardware one at a time. The
/// members queue messages to the mailbox and return and, where appropriate, return asynchronous workflows to await the agent's response.
type MagnetController
   (session : MessageBasedSession, 
    magnetControllerParams : MagnetControllerParameters) =
    static let log = LogManager.GetLogger typeof<MagnetController> // logger
    
    // events
    let agentFailed = new Event<exn>() // fires when the agent encounters an error, incidcating that current and future messages will be failed
    let agentFailedReplay = agentFailed.Publish.FirstAsync().Replay() // replays the error message if the agent has crashed
    let sessionReleased = new Event<unit>() // fires when the MagnetControllerSession is released and the agent is shut down
    
    // start a MailboxProcessor which will handle commands and requests sequentially and communicate with the magnet controller hardware
    let agent = Agent.Start(fun mailbox -> 
        
        // define the message processing loop workflow
        let rec loop() = async {
            try


                sprintf "(Re)entering Twickenham SMC %s agent loop." session.ResourceName |> log.Info

                let! message = mailbox.Receive() // read the next message in the queue or asynchronously wait for one
                sprintf "Twickenham SMC %s received message %A." session.ResourceName message |> log.Info 
            
                // process the message according to its type
                match message with
                
                // Release the MagnetControllerSession

                | ReleaseSession ->
                    sprintf "Twickenham SMC %s releasing session." session.ResourceName |> log.Info

                    // if there are unprocessed messages in the queue, raise an exception
                    if mailbox.CurrentQueueLength <> 0 then
                        failwithf "Twickenham SMC %s received ReleaseSession message when message queue is non-empty." session.ResourceName

                    // raise an event to indicate that the MagnetController has released the session and terminate the workflow
                    sessionReleased.Trigger ()
                    return () // no continuation, so no further messages will be processed

                // Requests

                | GetOutputParameters replyChannel ->
                    // query the hardware with the appropriate command string and wait for a response asynchronously
                    sprintf "Twickenham SMC %s query hardware output parameters." session.ResourceName |> log.Info
                    let! response = session.QuerryAsync "G\r\n" 
                    sprintf "Twickenham SMC %s received response from hardware: %s." session.ResourceName response |> log.Info
                    let outputParameters = OutputParameters.Parse response // parse the response

                    sprintf "Twickenham SMC %s replying to output parameter request with\n%A." session.ResourceName outputParameters |> log.Info
                    outputParameters |> replyChannel.Reply // reply to the request

                | GetCurrentParameters replyChannel ->
                    // query the hardware with the appropriate command string and wait for a response asynchronously
                    sprintf "Twickenham SMC %s query hardware current parameters." session.ResourceName |> log.Info
                    let! response = session.QuerryAsync "K\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." session.ResourceName response |> log.Info
                    let currentParameters = CurrentParameters.Parse response // parse the response
                
                    sprintf "Twickenham SMC %s replying to currenet parameter request with\n%A." session.ResourceName currentParameters |> log.Info
                    currentParameters |> replyChannel.Reply // reply to the request

                | GetOperatingParameters replyChannel ->
                    // query the hardware with the appropriate command string and wait for a response asynchronously
                    sprintf "Twickenham SMC %s query hardware operating parameters." session.ResourceName |> log.Info
                    let! response = session.QuerryAsync "O\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." session.ResourceName response |> log.Info
                    let operatingParameters = OperatingParameters.Parse response // parse the response

                    sprintf "Twickenham SMC %s replying to operating parameter request with\n%A." session.ResourceName operatingParameters |> log.Info
                    operatingParameters |> replyChannel.Reply // reply to the request

                | GetSetPointParameters replyChannel ->
                    // query the hardware with the appropriate command string and wait for a response asynchronously
                    sprintf "Twickenham SMC %s query hardware set point parameters." session.ResourceName |> log.Info
                    let! response = session.QuerryAsync "S\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." session.ResourceName response |> log.Info
                    let setPointParameters = SetPointParameters.Parse response // parse the response

                    sprintf "Twickenham SMC %s replying to set-point parameter request with\n%A." session.ResourceName setPointParameters |> log.Info
                    setPointParameters |> replyChannel.Reply // reply to the request
                
                // Commands

                | SetRampRate rampRate -> 
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting ramp rate to %08.5f A/s." session.ResourceName (float rampRate) |> log.Info
                    do! session.WriteAsync (sprintf "A%08.5f" (float rampRate))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly

                | SetTripVoltage tripVoltage -> 
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting trip voltage to %04.1f V." session.ResourceName (float tripVoltage) |> log.Info
                    do! session.WriteAsync (sprintf "Y%04.1f" (float tripVoltage))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly

                | SetCurrentDirection currentDirection -> 
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting current direction %A." session.ResourceName currentDirection |> log.Info
                    do! session.WriteAsync (sprintf "D%s" (currentDirection.CommandString()))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly
                    
                | SetLowerSetPoint lowerSetPoint ->
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting lower set point to %08.4f." session.ResourceName (float lowerSetPoint) |> log.Info
                    do! session.WriteAsync (sprintf "L%s" (magnetControllerParams.FormatSetPoint lowerSetPoint))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly

                | SetUpperSetPoint upperSetPoint -> 
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting upper set point to %08.4f." session.ResourceName (float upperSetPoint) |> log.Info
                    do! session.WriteAsync (sprintf "U%s" (magnetControllerParams.FormatSetPoint upperSetPoint))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly

                | SetRampTarget rampTarget -> 
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting ramp target to %A current limit." session.ResourceName rampTarget |> log.Info
                    do! session.WriteAsync (sprintf "R%s" (rampTarget.CommandString()))
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly

                | SetPause pause ->
                    // format the ramp rate appropriately and write it to the hardware
                    sprintf "Twickenham SMC %s setting pause %s." session.ResourceName (if pause then "on" else "off") |> log.Info 
                    do! session.WriteAsync (if pause then "P1" else "P0")
                    do! Async.Sleep 1000 // sleep for 1000 ms as the magnet controller firmware crashes if commands are sent too quickly
            
                // continue processing messages
                return! loop ()
            
            with
                | exn -> // if an error occurs, trigger the failed event and terminate the agent
                    log.Error (sprintf "Twickenham SMC %s failed due to error %A." session.ResourceName exn, exn)
                    agentFailed.Trigger exn } 

        // conenct agentFailedReplay to the underlying observable
        agentFailedReplay.Connect() |> ignore
        // initialse the agent message processing loop
        loop())

    /// Event indiciating that the MagnetControllerSession has been released. 
    member __.SessionReleased =
        Observable.Create(fun (observer : IObserver<unit>) ->
            let completedSub = sessionReleased.Publish.Subscribe(fun () -> observer.OnNext() ; observer.OnCompleted())
            let failedSub = agentFailedReplay.Subscribe(fun exn -> observer.OnError exn)

            { new IDisposable with
                member __.Dispose() = 
                    completedSub.Dispose()
                    failedSub.Dispose() })

    /// Magnet controller hardware parameters. Provides various utility functions for determining the field in terms of current, 
    /// readout shunt voltage in terms of current, etc.
    member __.MagnetControllerParameters = magnetControllerParams

    // Public members for sending agent commands and requests

    interface IDisposable with
        /// Releases the MagnetController session.
        member __.Dispose () =
            ReleaseSession |> agent.Post

    /// Asynchronously requests the magnet controller output parameters.
    member __.GetOutputParametersAsync () =
        GetOutputParameters
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Asynchronously requests the magnet controller current parameters.
    member __.GetCurrentParametersAsync () =
        GetCurrentParameters
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Asynchronously requests the magnet controller operating parameters.
    member __.GetOperatingParametersAsync () =
        GetOperatingParameters
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Asynchronously requests the magnet controller set point parameters.
    member __.GetSetPointParametersAsync () =
        GetSetPointParameters
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Posts a message to the magnet controller to set the nearset available calibrated ramp rate to the requested value in amps
    /// per second. If the ramp rate exceeds the magnet controller ramp rate limit, then the largest available value will be set. 
    member __.SetRampRate rampRate =
        // raise an exception if the requested ramp rate is negative
        if rampRate < 0.0<A/s> then
            failwith "Cannot set magnet controller ramp rate to a negative value."

        SetRampRate (magnetControllerParams.NearestCalibratedCurrentRampRate rampRate)
        |> agent.Post

    /// Posts a message to the magnet controller to set the calibrated ramp rate by index.
    member magnetController.SetRampRateByIndex index =
        magnetControllerParams.CurrentRampRateForIndex index
        |> magnetController.SetRampRate

    /// Posts a message to the magnet controller to set the largest avialbe ramp rate.
    member __.SetMaximumRampRate() =
        SetRampRate (magnetControllerParams.AvailableCurrentRampRates |> Seq.last)
        |> agent.Post

    /// Posts a message to the magnet controller to set the requested trip voltage in volts.
    member __.SetTripVoltage tripVoltage =
        // if the trip voltage exceeds the trip voltage limit or is negative, raise an exception
        if tripVoltage > magnetControllerParams.TripVoltageLimit then 
            failwithf "Cannot set magnet controller trip voltage greater than %A V." magnetControllerParams.TripVoltageLimit
        if tripVoltage < 0.0<V> then
            failwith "Cannot set magnet controller trip voltage to a negative value."

        SetTripVoltage tripVoltage
        |> agent.Post

    /// Posts a message to the magnet controller to set the requested current direction. Note that this will fail unless the magnet
    /// controller output current, as given in OutputParameters, is zero. It is insufficient to check that the magnet controller has
    /// reached its RampTarget at zero.
    member __.SetCurrentDirection currentDirection =
        SetCurrentDirection currentDirection
        |> agent.Post

    /// Posts a message to the magnet controller to set the lower current set point to the requested value in amps.
    member __.SetLowerSetPoint lowerSetPoint =
        // if the current exceeds the current limit or is negative, raise an exception
        if lowerSetPoint > magnetControllerParams.CurrentLimit then
            failwithf "Cannot set magnet controller current limit to a value greater than %A A." magnetControllerParams.CurrentLimit
        if lowerSetPoint < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value. Ramp to zero current and change current direction."

        SetLowerSetPoint lowerSetPoint
        |> agent.Post

    /// Posts a message to the magnet controller to set the lower current set point by the magnet controller digital output step index.
    member magnetController.SetLowerSetPointByIndex currentIndex =
        currentIndex
        |> magnetController.MagnetControllerParameters.CurrentForIndex
        |> magnetController.SetLowerSetPoint

    /// Posts a message to the maget controller to set the upper current set point to the requested value in amps.
    member __.SetUpperSetPoint upperSetPoint =
        // if the current exceeds the current limit or is negative, raise an exception
        if upperSetPoint > magnetControllerParams.CurrentLimit then
            failwithf "Cannot set magnet controller current limit to a value greater than %A A." magnetControllerParams.CurrentLimit
        if upperSetPoint < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value. Ramp to zero current and change current direction."
        
        SetUpperSetPoint upperSetPoint
        |> agent.Post
    
    /// Posts a message to the magnet controller to set the upper current set point by the magnet controller digital output step index.
    member magnetController.SetUpperSetPointByIndex currentIndex =
        currentIndex
        |> magnetController.MagnetControllerParameters.CurrentForIndex
        |> magnetController.SetUpperSetPoint

    /// Posts a message to the magnet controller to set the requested ramp target.
    member __.SetRampTarget rampTarget =
        SetRampTarget rampTarget
        |> agent.Post

    /// Posts a message to the magnet controller to set the ramp pause on or off.
    member __.SetPause pause =
        SetPause pause
        |> agent.Post
        
    // Useful workflows, requests and commands which send multiple messages
    
    /// Asynchronously requests all parameters describing the magnet controller state.
    member magnetController.GetAllParametersAsync() = 
        async {
            let! setPointParams = magnetController.GetSetPointParametersAsync()
            let! outputParams = magnetController.GetOutputParametersAsync()
            let! operatingParams = magnetController.GetOperatingParametersAsync()
            let! currentParams = magnetController.GetCurrentParametersAsync()

            return { SetPointParameters = setPointParams
                     OutputParameters = outputParams
                     OperatingParameters = operatingParams 
                     CurrentParameters = currentParams } } 
    
    /// Asynchronously wait for the magnet controller to reach its target.
    member magnetController.WaitToReachTargetAsync() =
        // recursively check if the ramp target has been reached, and loop if not
        let rec waitToReachTarget() = async {
            let! currentParams = magnetController.GetCurrentParametersAsync()
            if not currentParams.ReachedTarget then
                do! waitToReachTarget() }

        waitToReachTarget()
    
    /// Asynchronously wait for the magnet controller to reach zero current and set the current direction.
    member magnetController.WaitToReachZeroAndSetCurrentDirectionAsync currentDirection =
        // recursively check if the output current is zero amps, and loop if not
        let rec waitToReachZero() = async {
            let! outputParams = magnetController.GetOutputParametersAsync()
            if outputParams.OutputCurrent <> 0.0<A> then 
                do! waitToReachZero() }
        
        // wait to reach zero and then set the  current direction
        async {
            do! waitToReachZero()
            magnetController.SetCurrentDirection currentDirection }

    /// Posts messages to the magnet controller to initiate a ramp to zero current at maximum ramp rate.
    member magnetController.BeginRampToZeroAtMaximumRampRate() =
        magnetController.SetRampTarget Zero
        magnetController.SetMaximumRampRate()
        magnetController.SetPause false