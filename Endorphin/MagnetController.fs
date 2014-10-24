module MagnetController

open Units
open Utils
open Microsoft.FSharp.Control
open NationalInstruments.VisaNS

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
      outputCurrent : float<A>
      /// <summary>Magnet controller output voltage in V.</summary>
      outputVoltage : float<V>
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
      rampRate : float<A/s>
      /// <summary>Magnet controller current direction.</summary>
      currentDirection : CurrentDirection }

/// <summary>
/// Record containing the data returned by the magnet controller when set point parameters are requested.
/// </summary>
type SetPointParameters = 
    { /// <summary>Magnet controller lower current limit in A.</summary>
      lowerLimit : float<A>
      /// <summary>Magnet controller upper current limit in A.</summary>
      upperLimit : float<A>
      /// <summary>Magnet controller trip voltage in V.</summary>
      tripVoltage : float<V> }

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
    /// <summary>Sets the magnet controller ramp rate.</summary>
    | SetRampRate of rampRate : float<A/s> /// in A/s
    /// <summary>Sets the magnet controller trip voltage.</summary>
    | SetTripVoltage of tripVoltage : float<V> // in V
    /// <summary>Sets the magnet controller current direction.</summary>
    | SetCurrentDirection of currentDirection : CurrentDirection
    /// <summary>Sets the lower current set-point of the magnet controller.</summary>
    | SetLowerSetPoint of lowerCurrentLimit : float<A> // in A
    /// <summary>Sets the uppper current set-point of the magnet controller.</summary>
    | SetUpperSetPoint of upperCurrentLimit : float<A> // in A
    /// <summary>Sets the magnet controller ramp target to either zero or the upper or lower current limit.</summary>
    | SetRampTarget of rampTarget : RampTarget
    /// <summary>Enables or disables the ramp pause on the magnet controller.</summary>
    | SetPause of pause : bool
    /// <summary>Wait for a response on this channel before disposing the NI VISA session to ensure clean closure.</summary>
    | PrepareToCloseSession of replyChannel : AsyncReplyChannel<unit> 

/// <summary>
/// Sets the maximum ramp rate on the magnet controller.
/// </summary>
/// <param name="magnetController">The magnet controller.</param>
let setMaximumRampRate (magnetController : MailboxProcessor<Command>) = magnetController.Post <| SetRampRate(ampsPerSecond 0.1)

/// <summary>
/// Returns an asynchronous computation which recursively checks whether the magnet controller has
/// reached its target until it does so.
/// </summary>
/// <param name="magnetController">The magnet controller.</param>
let rec waitToReachTarget (magnetController : MailboxProcessor<Command>) = async { 
    let! currentParams = magnetController.PostAndAsyncReply(GetCurrentParameters)
    if not currentParams.reachedTarget then do! waitToReachTarget magnetController }

/// <summary>
/// Returns an asynchronous computation which recursively checks whether the magnet controller's
/// output current is zero (after the low-pass filtering applied to it when requesting output
/// parameters). This check is necessary when the magnet controller current direction needs to be
/// changed.
/// </summary>
/// <param name="magnetController">The magnet controller.</param>
let rec prepareToFlipCurrentDirection (magnetController : MailboxProcessor<Command>) = async {
    let! outputParams = magnetController.PostAndAsyncReply(GetOutputParameters)
    if not (outputParams.outputCurrent = 0.0<A>) then do! prepareToFlipCurrentDirection magnetController }

/// <summary>
/// Returns an asynchronous computation which ramps the magnet controller to zero current but does not
/// wait for it to complete.
/// </summary>
/// <param name="magnetController">The magnet controller.</param>
let rampToZero (magnetController : MailboxProcessor<Command>) = async { 
    magnetController.Post <| SetRampTarget(Zero) 
    setMaximumRampRate magnetController 
    magnetController.Post <| SetPause(false) }

/// <summary>
/// Returns an asynchronous computation which ramps the magnet controller to zero current and sets
/// the current direction as specified.
/// </summary>
/// <param name="currentDirection">The magnet controller.</param>
/// <param name="magnetController">The desired current direction.</param>
let rampToZeroAndSetCurrentDirection currentDirection (magnetController : MailboxProcessor<Command>) = async {
    do! rampToZero magnetController
    do! prepareToFlipCurrentDirection magnetController
    magnetController.Post <| SetCurrentDirection(currentDirection) }

/// <summary>
/// Returns an asynchronous computation which performs the necessary requests to the magnet controller
/// to build a complete <see cref="MagnetController.State" /> object and returns it.
/// </summary>
/// <param name="magnetController">The magnet controller.</param>
let getState (magnetController : MailboxProcessor<Command>) = async {
    let! setPointParams = magnetController.PostAndAsyncReply(GetSetPointParameters)
    let! outputParams = magnetController.PostAndAsyncReply(GetOutputParameters)
    let! operatingParams = magnetController.PostAndAsyncReply(GetOperatingParameters)
    let! currentParams = magnetController.PostAndAsyncReply(GetCurrentParameters)

    return { setPointParameters = setPointParams
             outputParameters = outputParams
             operatingParameters = operatingParams 
             currentParameters = currentParams } } 

/// <summary>
/// Actor mailbox which controls a Twickenham superconducting magnet controller, according to the
/// <see cref="MagnetController.Command" /> messages sent to it and responds via an AsyncReplyChannel
/// if needed.
/// </summary>
/// <param name="session">A <see cref="NationalInstruments.VisaNS.MessageBasedSession" /> object with
/// an established connection to the magnet controller hardware.</param>
let magnetControllerMailbox (session : MessageBasedSession) = 
    fun (mailbox : MailboxProcessor<Command>) -> 
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
                { outputCurrent = amps i
                  outputVoltage = volts v
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
                { rampRate = ampsPerSecond a
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
                { lowerLimit = amps l 
                  upperLimit = amps u
                  tripVoltage = volts y }
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
                replyChannel.Reply(())
            | _ ->
                do! buildCommand command |> writeToSesiion session
                do! Async.Sleep(1000)

            return! loop() }

        // initialse the actor state
        loop()