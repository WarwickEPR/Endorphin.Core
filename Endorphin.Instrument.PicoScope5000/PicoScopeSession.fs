namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Errors
open System.Text
open log4net

type SessionCommand =
    | RequestControl of AsyncReplyChannel<PicoScope5000>
    | CloseSession of AsyncReplyChannel<unit>

type PicoScope5000Session(initSerial, resolution) =
    static let log = LogManager.GetLogger typeof<PicoScope5000Session>

    let agent = Agent.Start(fun (mailbox : Agent<SessionCommand>) ->
        let rec start() = async {
            if initSerial = null then
                "Starting PicoScope session agent for first available device." |> log.Info
            else
                sprintf "Starting PicoScope session agent for device with serial number %s." initSerial |> log.Info

            let handle = 
                let mutable localHandle = 0s
                let status = Api.OpenUnit(&localHandle, initSerial, resolution)
                match status with
                | PicoStatus.Ok | PicoStatus.PowerSupplyConnected ->
                    sprintf "Successfully started session agent for PicoScope %s with mains power." initSerial |> log.Info
                    localHandle
                | PicoStatus.PowerSupplyNotConnected ->
                    let error = sprintf "Connected to PicoScope %s but device is not mains poewred. USB power is not currently supported." initSerial
                    log.Error error
                    failwith error
                | _ ->
                    let exn = PicoException(messageForStatus status, status, "Open unit")
                    let error = sprintf "Failed to start agent for PicoScope due to error: %s" (messageForStatus status)
                    log.Error(error, exn)
                    raise exn
            
            let serial =
                if initSerial = null then
                    let resultLength = 32s
                    let localSerial = new StringBuilder(int resultLength)
                    let mutable requiredLength = 0s
                    let status = Api.GetUnitInfo(handle, localSerial, resultLength, &requiredLength, PicoInfo.SerialNumber)
                    let error = errorMessage status
                    if error.IsSome then
                        let initError = sprintf "Failed to retrieve PicoScope serial number during initialisation due to error: %s." error.Value
                        log.Error initError
                        failwith initError
                    sprintf "Successfully retreived serial number %s for PicoScope." (localSerial.ToString()) |> log.Info
                    localSerial.ToString()
                else initSerial

            let modelNumber =
                let resultLength = 32s
                let localModelNumber = new StringBuilder(int resultLength)
                let mutable requiredLength = 0s
                let status = Api.GetUnitInfo(handle, localModelNumber, resultLength, &requiredLength, PicoInfo.ModelNumber) 
                let error = errorMessage status
                if error.IsSome then
                    let initError = sprintf "Failed to retrieve PicoScope model number during initialisation due to error: %s." error.Value
                    log.Error initError
                    failwith initError
                sprintf "Successfully retreived model number %s for PicoScope %s." (localModelNumber.ToString()) serial |> log.Info
                localModelNumber.ToString()
            
            let numberOfInputs = int (modelNumber.[1].ToString()) // number of input channels is the second digit in the model number
            let inputChannels =
                match numberOfInputs with
                | 2 -> Set.ofList [ Channel.A ; Channel.B ]
                | 4 -> Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ]
                | _ -> 
                    let initError = sprintf "PicoScope %s has unexpected model number." serial
                    log.Error initError
                    failwith initError
            sprintf "PicoScope %s has input channels: %A." serial inputChannels |> log.Info

            return! loop {
                handle = handle
                serial = serial
                resolution = resolution
                inputChannels = inputChannels } }

        and loop session = async {
            sprintf "PicoScope session %s waiting for session command." (session.serial) |> log.Info
            
            let! message = mailbox.Receive()
            sprintf "PicoScope session %s received session command %A." (session.serial) message |> log.Info
            
            match message with
            | RequestControl replyChannel ->
                sprintf "Creating PicoScope agent for session %s." (session.serial) |> log.Info
                let picoScope = new PicoScope5000(session)
                let! waitForSessionReleased =
                    picoScope.SessionReleased
                    |> Async.AwaitEvent
                    |> Async.StartChild
                picoScope |> replyChannel.Reply

                sprintf "Waiting for PicoScope agent to release session %s." (session.serial) |> log.Info
                do! waitForSessionReleased

                sprintf "PicoScope agent released session %s." (session.serial) |> log.Info
                return! loop session
            
            | CloseSession replyChannel ->                
                sprintf "Closing PicoScope session %s." (session.serial) |> log.Info
                let status = Api.CloseUnit(session.handle)
                let error = errorMessage status
                if error.IsSome then
                    let closeError = sprintf "PicoScope session %s failed to close due to error: %s." session.serial error.Value
                    log.Error closeError
                    failwith closeError

                if mailbox.CurrentQueueLength <> 0 then
                    let closeError = sprintf "Closed PicoScope %s session with %d pending requests." session.serial mailbox.CurrentQueueLength
                    log.Error closeError
                    failwith closeError

                sprintf "Successfully closed connection to PicoScope %s." session.serial |> log.Info
                replyChannel.Reply() }

        start())

    new() = PicoScope5000Session(null, Resolution._8bit) // first available device, default resolution
    new(initSerial) = PicoScope5000Session(initSerial, Resolution._8bit) // specific serial number, default resolution
    new(resolution) = PicoScope5000Session(null, resolution) // first available device, specific resolution

    member __.RequestControlAsync() =
        RequestControl
        |> agent.PostAndAsyncReply

    member __.CloseSessionAsync() =
        CloseSession
        |> agent.PostAndAsyncReply

    static member GetConnectedDeviceSerials() =
        "Getting list of connected PicoScope devices." |> log.Info
        let mutable count = 0s
        let mutable stringLength = 32s
        let serials = new StringBuilder(int stringLength)
        let status = Api.EnumerateUnits(&count, serials, &stringLength) 
        match status with
        | PicoStatus.Ok ->
            let deviceSerials = serials.ToString().Split(",".ToCharArray())
            sprintf "Found connected PicoScopes with serials: %A." deviceSerials |> log.Info
            deviceSerials
        | PicoStatus.NotFound -> 
            "No devices found." |> log.Info
            Array.empty
        | status -> 
            let exn = PicoException(messageForStatus status, status, "GetConnectedDeviceSerials")
            log.Error (sprintf "Error while getting list of connected devices: %s." (messageForStatus status), exn)
            raise exn
