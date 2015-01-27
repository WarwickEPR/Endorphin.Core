﻿namespace Endorphin.Instrument.PicoScope5000

open Endorphin.Core
open Errors
open System.Text
open log4net
open System

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
                    let error = new Exception(sprintf "Connected to PicoScope %s but device is not mains poewred. USB power is not currently supported." initSerial)
                    log.Error (error.Message, error)
                    raise error
                | _ ->
                    let exn = PicoException(messageForStatus status, status, "Open unit")
                    let error = sprintf "Failed to start agent for PicoScope due to error: %s" (messageForStatus status)
                    log.Error (error, exn)
                    raise exn
            
            let serial =
                if initSerial = null then
                    let resultLength = 32s
                    let localSerial = new StringBuilder(int resultLength)
                    let mutable requiredLength = 0s
                    let status = Api.GetUnitInfo(handle, localSerial, resultLength, &requiredLength, PicoInfo.SerialNumber)
                    let error = errorMessage status
                    if error.IsSome then
                        let initError = new Exception(sprintf "Failed to retrieve PicoScope serial number during initialisation due to error: %s." error.Value)
                        log.Error (initError.Message, initError)
                        raise initError
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
                    let initError = new Exception(sprintf "Failed to retrieve PicoScope model number during initialisation due to error: %s." error.Value)
                    log.Error (initError.Message, initError)
                    raise initError
                sprintf "Successfully retreived model number %s for PicoScope %s." (localModelNumber.ToString()) serial |> log.Info
                localModelNumber.ToString()
            
            let numberOfInputs = int (modelNumber.[1].ToString()) // number of input channels is the second digit in the model number
            let inputChannels =
                match numberOfInputs with
                | 2 -> Set.ofList [ Channel.A ; Channel.B ]
                | 4 -> Set.ofList [ Channel.A ; Channel.B ; Channel.C ; Channel.D ]
                | _ -> 
                    let initError = new Exception(sprintf "PicoScope %s has unexpected model number %s." serial modelNumber)
                    log.Error (initError.Message, initError)
                    raise initError
            sprintf "PicoScope %s has input channels: %A." serial inputChannels |> log.Info

            return! loop {
                Handle = handle
                Serial = serial
                Resolution = resolution
                InputChannels = inputChannels } }

        and loop session = async {
            sprintf "PicoScope session %s waiting for session command." (session.Serial) |> log.Info
            
            let! message = mailbox.Receive()
            sprintf "PicoScope session %s received session command %A." (session.Serial) message |> log.Info
            
            match message with
            | RequestControl replyChannel ->
                sprintf "Creating PicoScope agent for session %s." (session.Serial) |> log.Info
                let picoScope = new PicoScope5000(session)
                let! waitForSessionReleased =
                    picoScope.SessionReleased
                    |> Async.AwaitEvent
                    |> Async.StartChild
                picoScope |> replyChannel.Reply

                sprintf "Waiting for PicoScope agent to release session %s." (session.Serial) |> log.Info
                do! waitForSessionReleased

                sprintf "PicoScope agent released session %s." (session.Serial) |> log.Info
                return! loop session
            
            | CloseSession replyChannel ->                
                sprintf "Closing PicoScope session %s." (session.Serial) |> log.Info
                let status = Api.CloseUnit(session.Handle)
                let error = errorMessage status
                if error.IsSome then
                    let closeError = new Exception(sprintf "PicoScope session %s failed to close due to error: %s." session.Serial error.Value)
                    log.Error (closeError.Message, closeError)
                    raise closeError

                if mailbox.CurrentQueueLength <> 0 then
                    let closeError = new Exception(sprintf "Closed PicoScope %s session with %d pending requests." session.Serial mailbox.CurrentQueueLength)
                    log.Error (closeError.Message, closeError)
                    raise closeError

                sprintf "Successfully closed connection to PicoScope %s." session.Serial |> log.Info
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
