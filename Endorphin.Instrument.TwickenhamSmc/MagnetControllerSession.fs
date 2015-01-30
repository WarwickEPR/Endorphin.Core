namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Control
open NationalInstruments.VisaNS
open log4net

/// Represents the set of messages which can be sent to a MagnetControllerSession agent mailbox. This is only used internally in 
/// this assembly. The functionality is exposed by the MagnetControllerSession class members which queue messages to the agent 
/// mailbox and return asynchronous workflows to await the agent's response.
type internal SessionCommand =
    | RequestControl of AsyncReplyChannel<MagnetController>
    | CloseSession of AsyncReplyChannel<unit>

/// Represents a session to a Twickenham magnet controller. A connection is opened upon instantiation and closed when the
/// CloseSessionAsync workflow is run. Run the RequestControlAsync workflow to obtain control of the hardware via a
/// MagnetController object. Events are fired on the System.Threading.SynchronizationContext on which intialization takes
/// place.
type MagnetControllerSession(visaAddress, magnetControllerParams : MagnetControllerParameters) =
    static let log = LogManager.GetLogger typeof<MagnetControllerSession> // logger

    // capture the current synchronisation context so that events can be fired on the UI thread or thread pool accordingly
    let syncContext = System.Threading.SynchronizationContext.CaptureCurrent()

    // start an agent which will process session messages
    let agent = Agent.Start(fun (mailbox : Agent<SessionCommand>) ->

        // define the startup workflow
        let rec start() = async {
            sprintf "Creating VISA session for Twickenham SMC at %s." visaAddress |> log.Info
            
            try
                // open a NationalInstruments.VisaNS.MessageBasedSession with the specified VISA address
                let session = visaAddress |> ResourceManager.GetLocalManager().Open :?> MessageBasedSession
                sprintf "Session created successfully." |> log.Info
                do! Async.Sleep 1000 // sleep for 1000 ms before sending any commands, as the device firmware can otherwise crash
                
                // proceed to processing session messages with the loop workflow using the opened session
                return! loop session
            with
                | exn -> // if an error occurs, log it and raise an exception
                    log.Error("Failed to create session.", exn)
                    raise exn }
        
        // define the session loop workflow
        and loop session = async {
            sprintf "Twickenham SMC session %s waiting for session command." visaAddress |> log.Info
            
            let! message = mailbox.Receive() // read the next message in the queue or asynchronously wait for one
            sprintf "Twickenham SMC session %s received session command %A." visaAddress message |> log.Info
            
            // process the message according to its type
            match message with

            | RequestControl replyChannel ->

                sprintf "Creating controller for Twickenham SMC session %s." visaAddress |> log.Info
                
                // create a MagnetController control interface instance with the VISA session and hardware parameters
                let magnetController = new MagnetController(session, magnetControllerParams, syncContext)

                // create a child workflow which will wait for the PicoScope5000 object to release the session
                let! waitForSessionReleased = 
                    magnetController.SessionReleased
                    |> Async.AwaitEvent
                    |> Async.StartChild

                magnetController |> replyChannel.Reply // reply to the request

                sprintf "Waiting for controller to release Twickenham SMC session %s." visaAddress |> log.Info
                do! waitForSessionReleased // wait for the session to be released by the MagnetController object

                sprintf "Controler released Twickenham SMC session %s." visaAddress |> log.Info
                return! loop session // process the next message in the mailbox using the session loop workflow
            
            | CloseSession replyChannel ->                
                sprintf "Closing Twickenham SMC session %s." visaAddress |> log.Info

                // close the connection to the hardware
                session.Dispose()

                // if there are still unprocessed messages in the mailbox then raise an exception
                if mailbox.CurrentQueueLength <> 0 then
                    let closeError = sprintf "Closed Twicenham SMC %s session with %d pending requests." visaAddress mailbox.CurrentQueueLength
                    log.Error closeError
                    failwith closeError
                
                sprintf "Successfully closed Twickenham SMC session %s." visaAddress |> log.Info
                replyChannel.Reply() // reply to the request to indicate that the session has been successfully closed
                
                // terminate the workflow
                return () // no continuation, so no further messages will be processed
                }

        // initialise the session agent using the startup workflow
        start())

    /// Magnet controller hardware parameters. Provides various utility functions for determining the field in terms of current, 
    /// readout shunt voltage in terms of current, etc.
    member __.MagnetControllerParameters = magnetControllerParams
    
    /// Asynchronously request control of the session to the hardware. If the session is already in use, the request will only complete
    /// once the MagnetController object releases the session by calling IDisposable.Dispose.
    member __.RequestControlAsync() =
        agent.PostAndAsyncReply RequestControl

    /// Asynchronously causes the connection to the hardware to be closed. If the session is already in use, the request will only complete
    /// once the PicoScope5000 object releases the session by calling IDisposable.Dispose. The workflow will complete once the session has
    /// been closed.
    member __.CloseSessionAsync() =
        agent.PostAndAsyncReply CloseSession