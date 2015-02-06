namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open log4net
open System.Reactive.Linq
open System.Text

/// Represents the set of messages which can be sent to a PicoScope5000Session agent mailbox. This is only used internally in this
/// assembly. The functionality is exposed by the PicoScope5000Session class members which queue messages to the agent mailbox and
/// return asynchronous workflows to await the agent's response.
type internal SessionMessage =
    | Connect of AsyncReplyChannel<unit>
    | RequestControl of AsyncReplyChannel<PicoScope5000>
    | CloseSession of AsyncReplyChannel<unit>

/// Represents a session to a PicoScope 5000 series device. A connection is opened upon instantiation and closed when the
/// CloseSessionAsync workflow is run. Run the RequestControlAsync workflow to obtain control of the hardware via a PicoScope5000
/// object. Instantiate with a specified serial to connect to a particular device or leave this unspecified to connect to the first
/// available device. The vertical resolution of the device defaults to 8 bits if none is specified.
type PicoScope5000Session(initSerial, resolution) =
    static let log = LogManager.GetLogger typeof<PicoScope5000Session> // logger

    // events
    let agentFailed = new Event<exn>() // fires when the agent encounters an error, incidcating that current and future messages will be failed
    let agentFailedReplay = agentFailed.Publish.FirstAsync().Replay() // replays the error message if the agent has crashed

    // start an agent which will process session messages
    let agent = Agent.Start(fun (mailbox : Agent<SessionMessage>) ->
        
        // define the startup workflow
        let rec start() = async {
            try
                let! message = mailbox.Receive() // read the next message in the queue or asynchronously wait for one

                // expecting connect message first so raise an exception if anything else occurs.
                let replyChannel =
                    match message with
                    | Connect replyChannel -> replyChannel
                    | _ -> failwithf "PicoScope sesion agent received unexpected message %A before connecting." message

                if initSerial = null then // connect to the first available PicoScope 5000 device
                    "Starting PicoScope session agent for first available device." |> log.Info
                else /// connect to a specific device otherwise
                    sprintf "Starting PicoScope session agent for device with serial number %s." initSerial |> log.Info

                // open a connection to the hardware and get a handle which can be used to send further messages to the hardware
                let handle =
                    let mutable localHandle = 0s
                    let status = Api.OpenUnit(&localHandle, initSerial, resolution)

                    match status with

                    | PicoStatus.Ok -> // if the device connects successfully with the power supply connected 
                        sprintf "Successfully started session agent for PicoScope %s with mains power." initSerial |> log.Info
                        localHandle // then use the handle written by the driver to localHandle 

                    | PicoStatus.PowerSupplyNotConnected -> // if mails power is not connected, raise an exception (USB power not yet supported)
                        failwithf "Connected to PicoScope %s but device is not mains poewred. USB power is not currently supported." initSerial

                    | _ -> // raise an exception if a connection was not opened
                        raise (status.Exception "Open unit")

                // get the device serial
                let serial =
                    if initSerial <> null then 
                        initSerial // use the initialisation serial if one was specified
                    else
                        // if none was specified then obtain it using the PicoScope API
                        let resultLength = 32s
                        let localSerial = new StringBuilder(int resultLength)
                        let mutable requiredLength = 0s
                        let status = Api.GetUnitInfo(handle, localSerial, resultLength, &requiredLength, PicoInfo.SerialNumber)

                        // if the API returns an error status then raise an exception
                        if status.ErrorMessage.IsSome then
                            failwithf "Failed to retrieve PicoScope serial number during initialisation due to error: %s." status.Message

                        sprintf "Successfully retreived serial number %s for PicoScope." (localSerial.ToString()) |> log.Info
                        localSerial.ToString() // use the serial number returned by the PicoScope driver

                // get the device model number
                let modelNumber =
                    // obtain the model number using the PicoScope APi 
                    let resultLength = 32s
                    let localModelNumber = new StringBuilder(int resultLength)
                    let mutable requiredLength = 0s
                    let status = Api.GetUnitInfo(handle, localModelNumber, resultLength, &requiredLength, PicoInfo.ModelNumber)

                    // if the API returns an error status then raise an exception
                    if status.ErrorMessage.IsSome then
                        failwithf "Failed to retrieve PicoScope model number during initialisation due to error: %s." status.Message

                    sprintf "Successfully retreived model number %s for PicoScope %s." (localModelNumber.ToString()) serial |> log.Info
                    localModelNumber.ToString() // use the model numbmer returned by the PicoScope driver

                replyChannel.Reply()

                // proceed to processing session messages with the loop workflow using the obtained session parameters
                return! loop {
                    Handle = handle
                    SerialNumber = serial
                    Resolution = resolution
                    ModelNumber = modelNumber }
            
            with
                | exn -> // if an error occurs, trigger the failed event and terminate the agent
                    log.Error((sprintf "PicoScope %s session failed during startup due to error: %s" initSerial exn.Message), exn)
                    agentFailed.Trigger exn }

        // define the session loop workflow
        and loop sessionParams = async {
            try
                sprintf "PicoScope session %s waiting for session command." sessionParams.SerialNumber |> log.Info
            
                let! message = mailbox.Receive() // read the next message in the queue or asynchronously wait for one
                sprintf "PicoScope session %s received session command %A." sessionParams.SerialNumber message |> log.Info
            
                // process the message according to its type
                match message with

                | RequestControl replyChannel ->
                    sprintf "Creating PicoScope agent for session %s." sessionParams.SerialNumber |> log.Info
                
                    // create a PicoScope5000 control interface instance with the device session parameters
                    let picoScope = new PicoScope5000(sessionParams)

                    // create a child workflow which will wait for the PicoScope5000 object to release the session
                    let! waitForSessionReleased =
                        picoScope.SessionReleased
                        |> Async.AwaitObservable
                        |> Async.StartChild

                    picoScope |> replyChannel.Reply // reply to the request

                    sprintf "Waiting for PicoScope agent to release session %s." sessionParams.SerialNumber |> log.Info
                    do! waitForSessionReleased // wait for the session to be released by the PicoScope5000 object

                    sprintf "PicoScope agent released session %s." sessionParams.SerialNumber |> log.Info
                    return! loop sessionParams // process the next message in the mailbox using the session loop workflow
            
                | CloseSession replyChannel ->                
                    sprintf "Closing PicoScope session %s." sessionParams.SerialNumber |> log.Info

                    // close the connection to the hardware
                    let status = Api.CloseUnit(sessionParams.Handle)

                    // if an error occurs while closing the connection then raise an exception
                    if status.ErrorMessage.IsSome then
                        raise (status.Exception "Close unit")

                    // if there are still unprocessed messages in the mailbox then raise an exception
                    if mailbox.CurrentQueueLength <> 0 then
                        failwithf "Closed PicoScope %s session with %d pending requests." sessionParams.SerialNumber mailbox.CurrentQueueLength

                    sprintf "Successfully closed connection to PicoScope %s." sessionParams.SerialNumber |> log.Info
                    replyChannel.Reply() // reply to the request to indicate that the session has been successfully closed
                
                    // terminate the workflow
                    return () // no continuation, so no further messages will be processed

                | Connect _ ->
                    //  close the connection to the hardware as this is an unexpected request
                    let _ = Api.CloseUnit(sessionParams.Handle)

                    // raise an exception to indicate that an error occured
                    failwithf  "Closed PicoScope %s session with %d pending requests as an attempt was made to connect after a connection was already established."
                        sessionParams.SerialNumber mailbox.CurrentQueueLength
            with
                | exn -> // if an error occurs, trigger the failed event and terminate the agent
                    log.Error((sprintf "PicoScope %s session failed during startup due to error: %s" initSerial exn.Message), exn)
                    agentFailed.Trigger exn }

        // conenct agentFailedReplay to the underlying observable
        agentFailedReplay.Connect() |> ignore
        // initialise the session agent using the startup workflow
        start())

    /// Connects to the first available PicoScope 5000 series device with the default 8 bit vertical resolution.
    new() = PicoScope5000Session(null, Resolution._8bit)

    /// Connects to the PicoScope 5000 series device with the specified serial number with the default 8 bit vertical resolution.
    new(initSerial) = PicoScope5000Session(initSerial, Resolution._8bit)

    /// Connects to the first avialbe PicoScope 5000 series device with the specified vertical resolution.
    new(resolution) = PicoScope5000Session(null, resolution)
    
    /// Asynchronously establish a connection to the PicoScope hardware. This should be performed before reqeuesting control of the sesion.
    member __.ConnectAsync() =
        Connect
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Asynchronously request control of the session to the hardware. If the session is already in use, the request will only complete
    /// once the PicoScope5000 object releases the session by calling IDisposable.Dispose.
    member __.RequestControlAsync() =
        RequestControl
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Asynchronously causes the connection to the hardware to be closed. If the session is already in use, the request will only complete
    /// once the PicoScope5000 object releases the session by calling IDisposable.Dispose. The workflow will complete once the session has
    /// been closed.
    member __.CloseSessionAsync() =
        CloseSession
        |> agent.PostAndAsyncReplyFailable agentFailedReplay

    /// Returns a sequence of serial numbers for connected PicoScope 5000 series devices.
    static member GetConnectedDeviceSerials() =
        "Getting list of connected PicoScope devices." |> log.Info

        // get the list from the PicoScope API
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = Api.EnumerateUnits(&count, serials, &stringLength)

        match status with // check the response PicoStatus 

        | PicoStatus.Ok -> // return the sequence of serial numbers if devices were found
            let deviceSerials = serials.ToString().Split(",".ToCharArray())
            sprintf "Found connected PicoScopes with serials: %A." deviceSerials |> log.Info
            deviceSerials |> Array.toSeq 
        
        | PicoStatus.NotFound -> // return an empty sequence if none were found
            "No devices found." |> log.Info
            Seq.empty 

        | status -> // raise an exception if an error occured
            let exn = PicoException(status.Message, status, "GetConnectedDeviceSerials")
            log.Error (sprintf "Error while getting list of connected devices: %s." status.Message, exn)
            raise exn
