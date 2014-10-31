namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core.Utils
open Microsoft.FSharp.Control
open NationalInstruments.VisaNS
open System

/// <summary>
/// Discriminated union type describing one of the two possible current directions. Forward current
/// typically opposes the main magnetic field.
/// </summary>
type CurrentDirection = 
    | Forward
    | Reverse

/// <summary>
/// Discriminated union type describing the possible ramp targets for the magnet controller.
/// </summary>
type RampTarget = 
    | Zero
    | Lower
    | Upper

/// <summary>
/// Record containing the data returned by the magnet controller when output parameters are requested.
/// </summary>
type OutputParameters = 
    { /// <summary>Magnet controller output current in A.</summary>
      outputCurrentInAmps : float
      /// <summary>Magnet controller output voltage in V.</summary>
      outputVoltageInVolts : float
      /// <summary>Magnet controller ramp target.</summary>
      rampTarget : RampTarget }

/// <summary>
/// Record containing the data returned by the magnet controller when current parrameters are requested.
/// </summary>
type CurrentParameters = 
    { /// <summary>Magnet controller ramp target.</summary>
      rampTarget : RampTarget
      /// <summary>Boolean indicating whether the magnet controller has reached its target current.</summary>
      reachedTarget : bool
      /// <summary>Boolean indicating whether the magnet controller is paused.<summary>
      isPaused : bool }

/// <summary>Record containing the data returned by the magnet controller when operating parameters
/// are requested.</summary>
type OperatingParameters = 
    { /// <summary>Magnet controller ramp rate in A/s.</summary>
      rampRateInAmpsPerSec : float
      /// <summary>Magnet controller current direction.</summary>
      currentDirection : CurrentDirection }

type DeviceParameters = 
    { zeroCurrentFieldInMillitesla : float 
      fieldCalibrationInMilliteslaPerAmp : float
      rampRateLimitInAmpsPerSec : float
      tripVoltageLimitInVolts : float
      maximumCurrentInAmps : float
      currentLimitInAmps : float
      shuntCalibrationInVoltsPerAmp : float 
      outputResolutionInBits : int }

/// <summary>
/// Record containing the data returned by the magnet controller when set point parameters are requested.
/// </summary>
type SetPointParameters = 
    { /// <summary>Magnet controller lower current limit in A.</summary>
      lowerLimitInAmps : float
      /// <summary>Magnet controller upper current limit in A.</summary>
      upperLimitInAmps : float
      /// <summary>Magnet controller trip voltage in V.</summary>
      tripVoltageInVolts : float }

/// <summary>
/// Record type representing the complete magnet controller state at a given time.
/// </summary>
type State = 
    { setPointParameters : SetPointParameters
      outputParameters : OutputParameters
      operatingParameters : OperatingParameters
      currentParameters : CurrentParameters }

/// <summary>
/// Discriminated union type representing the possible commands to the magnet controller which may
/// require a response. If a response is required, a strongly-typed reply channel is contained in
/// the command.
/// </summary>
type Command = 
    /// <summary>Gets output current, voltage and ramp target in a <see cref="MagnetController.OutputParameters" /> record.</summary>
    | GetOutputParameters of replyChannel : AsyncReplyChannel<OutputParameters>
    /// <summary>Gets ramp target, target reached indicator, pause status in a <see cref="MagnetController.CurrentParameters" /> record.</summary>
    | GetCurrentParameters of replyChannel : AsyncReplyChannel<CurrentParameters>
    /// <summary>Gets ramp rate and current direction in a <see cref="MagnetController.OperatingParameters" /> record.</summary>
    | GetOperatingParameters of replyChannel : AsyncReplyChannel<OperatingParameters>
    /// <summary>Gets output lower and upper voltage limits and trip voltage in a <see cref="MagnetController.SetPointParameters" /> record.</summary>
    | GetSetPointParameters of replyChannel : AsyncReplyChannel<SetPointParameters>
    /// <summary>Sets the magnet controller ramp rate in A/s.</summary>
    | SetRampRate of rampRateInAmpsPerSec: float
    /// <summary>Sets the magnet controller trip voltage in V.</summary>
    | SetTripVoltage of tripVoltageInVolts : float
    /// <summary>Sets the magnet controller current direction.</summary>
    | SetCurrentDirection of currentDirection : CurrentDirection
    /// <summary>Sets the lower current set-point of the magnet controller in A.</summary>
    | SetLowerSetPoint of lowerCurrentLimitInAmps : float
    /// <summary>Sets the uppper current set-point of the magnet controller in A.</summary>
    | SetUpperSetPoint of upperCurrentLimitInAmps : float
    /// <summary>Sets the magnet controller ramp target to either zero or the upper or lower current limit.</summary>
    | SetRampTarget of rampTarget : RampTarget
    /// <summary>Enables or disables the ramp pause on the magnet controller.</summary>
    | SetPause of pause : bool
    /// <summary>Wait for a response on this channel before disposing the NI VISA session to ensure clean closure.</summary>
    | PrepareToCloseSession of replyChannel : AsyncReplyChannel<unit> 


type MagnetController(visaAddress, deviceParameters) =
    let session = 
        visaAddress
        |> ResourceManager.GetLocalManager().Open :?> MessageBasedSession
    
    /// <summary>
    /// Actor mailbox which controls a Twickenham superconducting magnet controller, according to the
    /// <see cref="MagnetController.Command" /> messages sent to it and responds via an AsyncReplyChannel
    /// if needed.
    /// </summary>
    /// <param name="session">A <see cref="NationalInstruments.VisaNS.MessageBasedSession" /> object with
    /// an established connection to the magnet controller hardware.</param>
    let mailboxProcessor = 
        (fun (mailbox : MailboxProcessor<Command>) -> 
            // Note: the following functions are defined within the mailbox function definition for
            // encapsulation. They do not need to be visible to users of the MagnetController actor.

            /// <summary>
            /// Parses a single character string as returned by the magnet controller hardware to give the 
            /// <see cref="MagnetController.CurrentDirection" />.
            /// </summary>
            let (|ParseCurrentDirection|_|) = 
                function 
                | "0" -> Some(Forward)
                | "1" -> Some(Reverse)
                | _ -> None
        
            /// <summary>
            /// Parses a single character string as returned by the magnet controller hardware to give the 
            /// <see cref="MagnetController.RampTarget" />.
            /// <summary>
            let (|ParseRampTarget|_|) = 
                function 
                | "0" -> Some(Zero)
                | "1" -> Some(Lower)
                | "2" -> Some(Upper)
                | _ -> None
        
            /// <summary>
            /// Parses the output parameter string returned by the magnet controller hardware using a regular
            /// expression to return the <see cref="MagnetController.OutputParameters" /> containing the
            /// output current, voltage and ramp target.
            /// </summary> 
            /// <param name="response">Response string from the magnet controller hardware.</param>
            let parseOutputParameters response = 
                // regex for string format "Isnnn.nnnVsnn.nRd[A/V]" where s is +/-, n can be any digit
                // and d is 0, 1 or 2   
                let regex = @"\GI([\+\-]\d{3}\.\d{3})V([\+\-]\d{2}.\d)R([012])[AV]\s$"
                match response with
                | ParseRegex regex [ ParseFloat i; ParseFloat v; ParseRampTarget r ] ->
                    { outputCurrentInAmps = i
                      outputVoltageInVolts = v
                      rampTarget = r }
                | _ -> failwith "Invalid magnet controller output parameter string"
        
            /// <summary>
            /// Parses the current parameter string returned by the magnet controller hardware using a regular
            /// expression to return the <see cref="MagnetController.CurrentPararmeters" /> containing the 
            /// ramp target and flags indicating whether the target has been reached and whether the magnet
            /// controller is paused.
            /// </summary>
            /// <param name="response">Response string from the magnet controller hardware.</param>
            let parseCurrentParameters response = 
                // regex for string format "RdMdPdXdHdZ0.00EddQsnnn.nnn" where d can take specific digit
                // values in each instance, s is +/- and n can be any digit
                let regex = @"\GR([012])M([01])P([01])X[0-5]H[012]Z0\.00E[0-3][0-7]Q[\+\-\s]\d{3}\.\d{3}\s$"
                match response with
                | ParseRegex regex [ ParseRampTarget r; ParseIntegerBool m; ParseIntegerBool p ] ->
                    { rampTarget = r 
                      reachedTarget = m
                      isPaused = p }
                | _ -> failwith "Invalid magnet controller current parameter string"

            /// <summary>
            /// Parses the operating parameter string returned by the magnet controller hardware using a regular
            /// expression to return the <see cref="MagnetController.OperatingParameters" /> containing ramp
            /// rate and current direction.
            /// </summary>
            /// <param name="response">Response string from the magnet controller hardware.</param>
            let parseOperatingParameters response = 
                // regex for string format "Ann.nnnnnDdTdBdWnnn.C0.nnnnnn" where n can be any digit and
                // d can be 0 or 1 in each case 
                let regex = @"\GA(\d{2}\.\d{5})D([01])T[01]B[01]W\d{3}\.C0\.\d{6}\s$"
                match response with
                | ParseRegex regex [ ParseFloat a; ParseCurrentDirection d ] ->
                    { rampRateInAmpsPerSec = a
                      currentDirection = d }
                | _ -> failwith "Invalid magnet controller operating parameter string"
        
            /// <summary>
            /// Parses the set point parameter string returned by the magnet controller hardware using a regular
            /// expression to return the <see cref="MagnetController.SetPointParameters" /> containing lower
            /// and upper current limit and trip voltage.
            /// </summary>
            /// <param name="response">Response string from the magnet controller hardware.</param>
            let parseSetPointParameters response = 
                // regex for string format "TdUnnn.nnnnLnnn.nnnYnn.n" where d can be 0 or 1 and n can be
                // any digit in each case
                let regex = @"\GT[01]U(\d{3}\.\d{3})L(\d{3}\.\d{3})Y(\d{2}\.\d)\s$"
                match response with
                | ParseRegex regex [ ParseFloat u; ParseFloat l; ParseFloat y ] ->
                    { lowerLimitInAmps = l 
                      upperLimitInAmps = u
                      tripVoltageInVolts = y }
                | _ -> failwith "Invalid magnet controller set point parameter string"
        
            /// <summary>
            /// Builds the command string to be sent to the magnet controller hardware for a
            /// <see cref="MagnetController.Command" />.
            /// </summary> 
            let buildCommand command = 
                let terminationCharacters = "\r\n"
                let instruction = 
                    match command with
                    | GetOutputParameters(_) -> "G" 
                    | GetCurrentParameters(_) -> "K"
                    | GetOperatingParameters(_) -> "O"
                    | GetSetPointParameters(_) -> "S"
                    | SetRampRate(rate) -> (float rate) |> sprintf "A%08.5f"
                    | SetTripVoltage(voltage) -> (float voltage) |> sprintf "Y%04.1f"
                    | SetCurrentDirection(direction) -> 
                        match direction with
                        | Forward -> "D0"
                        | Reverse -> "D1"
                    | SetLowerSetPoint(lower) -> (float lower) |> sprintf "L%07.3f"
                    | SetUpperSetPoint(upper) -> (float upper) |> sprintf "U%07.3f"
                    | SetRampTarget(target) -> 
                        match target with
                        | Zero -> "R0"
                        | Lower -> "R1"
                        | Upper -> "R2"
                    | SetPause(pause) -> 
                        if pause then "P1"
                        else "P0"
                    | PrepareToCloseSession(_) -> failwith "PrepareToCloseSession is not a command which requires a message to be sent to the hardware"
                instruction + terminationCharacters

            /// <summary>
            /// Partial active pattern which matches <see cref="MagnetController.Command"> objects
            /// which represent a request and builds a reply channel function which parses the instrument
            /// response string for the command and sends it via the strongly-typed reply channel.
            /// </summary>
            let (|Request|_|) command =
                match command with
                | GetOutputParameters(channel) -> Some(parseOutputParameters >> channel.Reply)
                | GetCurrentParameters(channel) -> Some(parseCurrentParameters >> channel.Reply)
                | GetOperatingParameters(channel) -> Some(parseOperatingParameters >> channel.Reply)
                | GetSetPointParameters(channel) -> Some(parseSetPointParameters >> channel.Reply)
                | _ -> None

            /// <summary>
            /// Returns an async computation expression defining the magnet controller's message handling
            /// behaviour.
            /// </summary>
            let rec loop() = async {
                let! command = mailbox.Receive()
                match command with
                | Request replyChannel ->
                    let! response = buildCommand command |> querySession session
                    response |> replyChannel
                | PrepareToCloseSession(replyChannel) ->
                    do! Async.Sleep(1000)
                    replyChannel.Reply()
                | _ ->
                    do! buildCommand command |> writeToSesiion session
                    do! Async.Sleep(1000)

                return! loop() }

            and start() = async {
                // sleep for 1s before sending any commands because communications can otherwise crash
                do! Async.Sleep(1000)
                return! loop() }

            // initialse the actor state
            start())
        |> MailboxProcessor.Start

    member internal this.getStateAsync() = 
        async {
            let! setPointParams = GetSetPointParameters |> mailboxProcessor.PostAndAsyncReply
            let! outputParams = GetOutputParameters |> mailboxProcessor.PostAndAsyncReply
            let! operatingParams = GetOperatingParameters |> mailboxProcessor.PostAndAsyncReply
            let! currentParams = GetCurrentParameters |> mailboxProcessor.PostAndAsyncReply

            return { setPointParameters = setPointParams
                     outputParameters = outputParams
                     operatingParameters = operatingParams 
                     currentParameters = currentParams } } 

    member internal this.rampToZeroAsync() = 
        async { 
            SetRampTarget(Zero) |> mailboxProcessor.Post
            SetRampRate(this.RampRateLimitInAmpsPerSec) |> mailboxProcessor.Post
            SetPause(false) |> mailboxProcessor.Post }

    member internal this.waitToReachTargetAsync() =
        let rec loop() = async {
            let! currentParams = mailboxProcessor.PostAndAsyncReply(GetCurrentParameters)
            if not currentParams.reachedTarget 
            then do! loop() }
        loop()

    member internal this.prepareToChangeCurrentDirectionAsync() =
        let rec loop() = async {
            let! outputParams = mailboxProcessor.PostAndAsyncReply(GetOutputParameters)
            if not (outputParams.outputCurrentInAmps = 0.0)
            then do! loop() }
        loop()

    member internal this.rampToZeroAndSetCurrentDirectionAsync currentDirection = 
        async {
            do! this.rampToZeroAsync()
            do! this.prepareToChangeCurrentDirectionAsync()
            SetCurrentDirection(currentDirection) |> mailboxProcessor.Post }

    member this.Post(message) = mailboxProcessor.Post(message)
    member this.PostAndReply(buildMessage) = mailboxProcessor.PostAndReply(buildMessage)
    member this.PostAndAsyncReply(buildMessage) = mailboxProcessor.PostAndAsyncReply(buildMessage)

    interface IDisposable with
        member this.Dispose() =
            PrepareToCloseSession
            |> mailboxProcessor.PostAndReply
            session.Dispose()

    member this.RampRateLimitInAmpsPerSec = deviceParameters.rampRateLimitInAmpsPerSec
    member this.TripVoltageLimitInVolts = deviceParameters.tripVoltageLimitInVolts
    member this.MaximumCurrentInAmps = deviceParameters.maximumCurrentInAmps
    member this.CurrentLimitInAmps = deviceParameters.currentLimitInAmps
    member this.ZeroFieldCurrentInMillitesla = deviceParameters.zeroCurrentFieldInMillitesla
    member this.FieldCalibrationInMilliteslaPerAmp = deviceParameters.fieldCalibrationInMilliteslaPerAmp
    member this.OutputResolutionInBits = deviceParameters.outputResolutionInBits
    member this.ShuntCalibrationInVoltsPerAmp = deviceParameters.shuntCalibrationInVoltsPerAmp

    member this.GetOutputParameters() =
        GetOutputParameters
        |> mailboxProcessor.PostAndReply

    member this.GetOutputParametersAsync() =
        GetOutputParameters
        |> mailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetCurrentParameters() =
        GetCurrentParameters
        |> mailboxProcessor.PostAndReply

    member this.GetCurrentParametersAsync() =
        GetCurrentParameters
        |> mailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetOperatingParameters() =
        GetOperatingParameters
        |> mailboxProcessor.PostAndReply

    member this.GetOperatingParametersAsync() =
        GetOperatingParameters
        |> mailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.GetSetPointParameters() =
        GetSetPointParameters
        |> mailboxProcessor.PostAndReply

    member this.GetSetPointParametersAsync() =
        GetSetPointParameters
        |> mailboxProcessor.PostAndAsyncReply
        |> Async.StartAsTask

    member this.SetRampRate(rampRateInAmpsPerSec) =
        if rampRateInAmpsPerSec > this.RampRateLimitInAmpsPerSec then 
            failwith (String.Format("Cannot set magnet controller ramp rate greater than {0} A/s.", this.RampRateLimitInAmpsPerSec))
        if rampRateInAmpsPerSec < 0.0 then
            failwith "Cannot set magnet controller ramp rate to a negative value."

        SetRampRate(rampRateInAmpsPerSec)
        |> mailboxProcessor.Post

    member this.SetTripVoltage(tripVoltageInVolts) =
        if tripVoltageInVolts > this.TripVoltageLimitInVolts then 
            failwith (String.Format("Cannot set magnet controller trip voltage greater than {0} V.", this.TripVoltageLimitInVolts))
        if tripVoltageInVolts < 0.0 then
            failwith "Cannot set magnet controller trip voltage to a negative value."

        SetTripVoltage(tripVoltageInVolts)
        |> mailboxProcessor.Post

    member this.SetCurrentDirection(currentDirection) =
        SetCurrentDirection(currentDirection)
        |> mailboxProcessor.Post

    member this.SetLowerSetPoint(lowerCurrentLimitInAmps) =
        if lowerCurrentLimitInAmps > this.CurrentLimitInAmps then
            failwith (String.Format("Cannot set magnet controller current limit to a value greater than {0} A.", this.CurrentLimitInAmps))
        if lowerCurrentLimitInAmps < 0.0 then
            failwith "Cannot set magnet controller current limit to a negative value"

        SetLowerSetPoint(lowerCurrentLimitInAmps)
        |> mailboxProcessor.Post

    member this.SetUpperSetPoint(upperCurrentLimitInAmps) =
        if upperCurrentLimitInAmps > this.CurrentLimitInAmps then
            failwith (String.Format("Cannot set magnet controller current limit to a value greater than {0} A.", this.CurrentLimitInAmps))
        if upperCurrentLimitInAmps < 0.0 then
            failwith "Cannot set magnet controller current limit to a negative value"
        
        SetUpperSetPoint(upperCurrentLimitInAmps)
        |> mailboxProcessor.Post

    member this.SetRampTarget(rampRateInAmpsPerSec) =
        SetRampTarget(rampRateInAmpsPerSec)
        |> mailboxProcessor.Post

    member this.SetPause(pause) =
        SetPause(pause)
        |> mailboxProcessor.Post

    member this.WaitToReachTarget() =
        this.waitToReachTargetAsync()
        |> Async.StartAsTask

    member this.PrepareToChangeCurrentDirection() =
        this.prepareToChangeCurrentDirectionAsync()
        |> Async.StartAsTask

    member this.RampToZero() =
        this.rampToZeroAsync()
        |> Async.StartAsTask

    member this.RampToZeroAndSetCurrentDirection(currentDirection) =
        this.rampToZeroAndSetCurrentDirectionAsync(currentDirection)
        |> Async.StartAsTask

    member this.GetState() =
        this.getStateAsync()
        |> Async.StartAsTask

    member this.AvailableRampRatesInAmpsPerSec =
        seq { 
            for index in 0 .. 64 do
                yield (10.0 ** ((float index) / 16.0)) * (this.MaximumCurrentInAmps / 1e5) }
        |> Seq.takeWhile (fun rampRate -> rampRate <= this.RampRateLimitInAmpsPerSec)

    member this.RampRateForIndexInAmpsPerSec (index) =
        if index >= (Seq.length this.AvailableRampRatesInAmpsPerSec) || index < 0
            then failwith "Ramp rate index out of range."
            
        this.AvailableRampRatesInAmpsPerSec
        |> Seq.nth (index)

    member this.AvailableRampRatesInMilliteslaPerSec =
        this.AvailableRampRatesInAmpsPerSec
        |> Seq.map (fun rampRate -> rampRate * this.FieldCalibrationInMilliteslaPerAmp)
    
    member this.RampRateForIndexInMilliteslaPerSec (index) =
        this.RampRateForIndexInAmpsPerSec(index) * this.FieldCalibrationInMilliteslaPerAmp

    member this.NumberOfCurrentSteps =
        int (2.0 ** (float this.OutputResolutionInBits))

    member this.CurrentStepInAmps =
        this.MaximumCurrentInAmps / (float (this.NumberOfCurrentSteps - 1))

    member this.CurrentForIndexInAmps (index) =
        if abs(index) >= this.NumberOfCurrentSteps
            then failwith "Current index out of range."
        
        this.CurrentStepInAmps * (float index)

    member this.FieldStepInMillitesla =
        this.CurrentStepInAmps * this.FieldCalibrationInMilliteslaPerAmp

    member this.FieldForIndexInMillitesla (index) =
        this.CurrentForIndexInAmps(index) * this.FieldCalibrationInMilliteslaPerAmp

    member this.MaximumFieldInMillitesla =
        this.ZeroFieldCurrentInMillitesla + abs(this.FieldCalibrationInMilliteslaPerAmp) * this.CurrentLimitInAmps

    member this.MinimumFieldInMillitesla =
        this.ZeroFieldCurrentInMillitesla - abs(this.FieldCalibrationInMilliteslaPerAmp) * this.CurrentLimitInAmps

    member this.NearestDigitisedCurrentInAmps(currentInAmps) =
        round(currentInAmps / this.CurrentStepInAmps) * this.CurrentStepInAmps
        |> max this.CurrentLimitInAmps
        |> min -this.CurrentLimitInAmps
    
    member this.NearestDigitisedFieldInMillitesla(fieldInMillitesla) =
        (fieldInMillitesla - this.ZeroFieldCurrentInMillitesla) / this.FieldCalibrationInMilliteslaPerAmp
        |> this.NearestDigitisedCurrentInAmps
        |> (fun nearestCurrent -> nearestCurrent * this.FieldCalibrationInMilliteslaPerAmp)
    
    member this.NearestDigitisedRampRateInAmpsPerSec(rampRateInAmpsPerSec) =
        this.AvailableRampRatesInAmpsPerSec
        |> Seq.minBy (fun digitisedRampRate -> abs(digitisedRampRate - rampRateInAmpsPerSec))

    member this.NearestDigitisedRampRateInMilliteslaPerSec(rampRateInMilliteslaPerSec) =
        rampRateInMilliteslaPerSec / this.FieldCalibrationInMilliteslaPerAmp
        |> this.NearestDigitisedRampRateInAmpsPerSec
        |> (fun nearestRampRate -> nearestRampRate * this.FieldCalibrationInMilliteslaPerAmp)

    member this.MaximumShuntVoltageInVolts =
        this.MaximumCurrentInAmps * this.ShuntCalibrationInVoltsPerAmp

    member this.ShuntStepInvVolts =
        this.CurrentStepInAmps * this.ShuntCalibrationInVoltsPerAmp

    member this.NearestDigitisedOutputIndexForShuntVoltage(voltageInVolts) =
        int (round(voltageInVolts / this.ShuntStepInvVolts) * this.ShuntStepInvVolts)
    
    member this.NearestDigitisedCurrentInAmpsForShuntVoltage(voltageInVolts) =
        this.NearestDigitisedOutputIndexForShuntVoltage(voltageInVolts)
        |> this.CurrentForIndexInAmps

    member this.NearestDigitisedFieldInMilliteslaForShuntVoltage(voltageInVolts) =
        this.NearestDigitisedOutputIndexForShuntVoltage(voltageInVolts)
        |> this.FieldForIndexInMillitesla