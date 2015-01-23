namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Microsoft.FSharp.Control
open NationalInstruments.VisaNS
open log4net

type SessionCommand =
    | RequestControl of AsyncReplyChannel<MagnetController>
    | CloseSession of AsyncReplyChannel<unit>

type MagnetControllerSession(visaAddress, deviceParameters : MagnetControllerParameters) =
    static let log = LogManager.GetLogger typeof<MagnetControllerSession>

    let agent = Agent.Start(fun (mailbox : Agent<SessionCommand>) ->
        let rec start() = async {
            sprintf "Creating VISA session for Twickenham SMC at %s." visaAddress |> log.Info
            
            try
                let session = visaAddress |> ResourceManager.GetLocalManager().Open :?> MessageBasedSession
                sprintf "Session created successfully." |> log.Info
                // sleep for 1s before sending any commands because communications can otherwise crash
                // (seems to be an issue with the magnet controller hardware)
                do! Async.Sleep(1000)
                return! loop session
            with
                | exn -> 
                    log.Error("Failed to create session.", exn)
                    raise exn }

        and loop session = async {
            sprintf "Twickenham SMC session %s waiting for session command." visaAddress |> log.Info
            
            let! message = mailbox.Receive()
            sprintf "Twickenham SMC session %s received session command %A." visaAddress message |> log.Info
            
            match message with
            | RequestControl replyChannel ->
                sprintf "Creating controller for Twickenham SMC session %s." visaAddress |> log.Info
                let magnetController = new MagnetController(session, deviceParameters)
                let! waitForSessionReleased = 
                    magnetController.SessionReleased
                    |> Async.AwaitEvent
                    |> Async.StartChild
                magnetController |> replyChannel.Reply

                sprintf "Waiting for controller to release Twickenham SMC session %s." visaAddress |> log.Info
                do! waitForSessionReleased

                sprintf "Controler released Twickenham SMC session %s." visaAddress |> log.Info
                return! loop session
            
            | CloseSession replyChannel ->                
                sprintf "Closing Twickenham SMC session %s." visaAddress |> log.Info
                session.Dispose()

                if mailbox.CurrentQueueLength <> 0 then
                    let closeError = sprintf "Closed Twicenham SMC session %s with pending requests." visaAddress
                    log.Error closeError
                    failwith closeError
                
                sprintf "Successfully closed Twickenham SMC session %s." visaAddress |> log.Info
                replyChannel.Reply() }

        start())

    member __.DeviceParameters = deviceParameters
    
    member __.RequestControlAsync() =
        agent.PostAndAsyncReply RequestControl

    member __.CloseSessionAsync() =
        agent.PostAndAsyncReply CloseSession