namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core.Utils
open Microsoft.FSharp.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
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

type MagnetControllerParameters = 
    { staticField : float<T>
      fieldCalibration : float<T/A>
      rampRateLimit : float<A/s>
      tripVoltageLimit : float<V>
      maximumCurrent : float<A>
      currentLimit : float<A>
      shuntCalibration : float<V/A>
      outputResolutionInBits : int
      setPointDecimalPlaces : int
      calibratedRampRates : float<A/s> seq }

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
type internal Command = 
    /// <summary>Gets output current, voltage and ramp target in a <see cref="MagnetController.OutputParameters" /> record.</summary>
    | GetOutputParameters of replyChannel : AsyncReplyChannel<OutputParameters>
    /// <summary>Gets ramp target, target reached indicator, pause status in a <see cref="MagnetController.CurrentParameters" /> record.</summary>
    | GetCurrentParameters of replyChannel : AsyncReplyChannel<CurrentParameters>
    /// <summary>Gets ramp rate and current direction in a <see cref="MagnetController.OperatingParameters" /> record.</summary>
    | GetOperatingParameters of replyChannel : AsyncReplyChannel<OperatingParameters>
    /// <summary>Gets output lower and upper voltage limits and trip voltage in a <see cref="MagnetController.SetPointParameters" /> record.</summary>
    | GetSetPointParameters of replyChannel : AsyncReplyChannel<SetPointParameters>
    /// <summary>Sets the magnet controller ramp rate in A/s.</summary>
    | SetRampRate of rampRate: float<A/s>
    /// <summary>Sets the magnet controller trip voltage in V.</summary>
    | SetTripVoltage of tripVoltage : float<V>
    /// <summary>Sets the magnet controller current direction.</summary>
    | SetCurrentDirection of currentDirection : CurrentDirection
    /// <summary>Sets the lower current set-point of the magnet controller in A.</summary>
    | SetLowerSetPoint of lowerSetPoint : float<A>
    /// <summary>Sets the uppper current set-point of the magnet controller in A.</summary>
    | SetUpperSetPoint of upperSetPoint : float<A>
    /// <summary>Sets the magnet controller ramp target to either zero or the upper or lower current limit.</summary>
    | SetRampTarget of rampTarget : RampTarget
    /// <summary>Enables or disables the ramp pause on the magnet controller.</summary>
    | SetPause of pause : bool
    /// <summary>Wait for a response on this channel before disposing the NI VISA session to ensure clean closure.</summary>
    | PrepareToCloseSession of replyChannel : AsyncReplyChannel<unit> 


type MagnetController(visaAddress, parameters) =
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
    let agent = 
        Agent.Start(fun mailbox -> 
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
                    { outputCurrent = i * 1.0<A>
                      outputVoltage = v * 1.0<V>
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
                    { rampRate = a * 1.0<A/s>
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
                    { lowerLimit = l * 1.0<A>
                      upperLimit = u * 1.0<A>
                      tripVoltage = y * 1.0<V> }
                | _ -> failwith "Invalid magnet controller set point parameter string"
        
            /// <summary>
            /// Returns an async computation expression defining the magnet controller's message handling
            /// behaviour.
            /// </summary>
            let rec loop() = async {
                let! command = mailbox.Receive()
                match command with
                
                | PrepareToCloseSession(replyChannel) ->
                    replyChannel.Reply()

                // Requests

                | GetOutputParameters replyChannel ->
                    let! response = session.QuerryAsync "G\r\n"
                    response 
                    |> parseOutputParameters
                    |> replyChannel.Reply

                | GetCurrentParameters replyChannel ->
                    let! response = session.QuerryAsync "K\r\n"
                    response 
                    |> parseCurrentParameters
                    |> replyChannel.Reply

                | GetOperatingParameters replyChannel ->
                    let! response = session.QuerryAsync "O\r\n"
                    response 
                    |> parseOperatingParameters
                    |> replyChannel.Reply

                | GetSetPointParameters replyChannel ->
                    let! response = session.QuerryAsync "S\r\n"
                    response 
                    |> parseSetPointParameters
                    |> replyChannel.Reply
                
                // Instructions

                | SetRampRate(rate) -> 
                    do!  
                        sprintf "A%08.5f" (float rate)
                        |> session.WriteAsync
                    do! Async.Sleep(1000)

                | SetTripVoltage(voltage) -> 
                    do!
                        sprintf "Y%04.1f" (float voltage)
                        |> session.WriteAsync
                    do! Async.Sleep(1000)

                 | SetCurrentDirection(direction) -> 
                    do! 
                        match direction with
                        | Forward -> session.WriteAsync "D0"
                        | Reverse -> session.WriteAsync "D1"
                    do! Async.Sleep(1000)

                | SetLowerSetPoint(lower) -> 
                    do! 
                        sprintf "L%07.3f" (float lower) 
                        |> session.WriteAsync
                    do! Async.Sleep(1000)

                | SetUpperSetPoint(upper) -> 
                    do! 
                        sprintf "U%07.3f" (float upper)
                        |> session.WriteAsync
                    do! Async.Sleep(1000)

                | SetRampTarget(target) -> 
                    do!
                        match target with
                        | Zero -> session.WriteAsync "R0"
                        | Lower -> session.WriteAsync "R1"
                        | Upper -> session.WriteAsync "R2"
                    do! Async.Sleep(1000)

                | SetPause(pause) -> 
                    do! 
                        if pause then session.WriteAsync "P1"
                        else session.WriteAsync "P0"
                    do! Async.Sleep(1000)

                return! loop() }

            and start() = async {
                // sleep for 1s before sending any commands because communications can otherwise crash
                // (seems to be an issue with the magnet controller hardware)
                do! Async.Sleep(1000)
                return! loop() }

            // initialse the actor state
            start())
    
    member this.GetAllParametersAsync() = 
        async {
            let! setPointParams = GetSetPointParameters |> agent.PostAndAsyncReply
            let! outputParams = GetOutputParameters |> agent.PostAndAsyncReply
            let! operatingParams = GetOperatingParameters |> agent.PostAndAsyncReply
            let! currentParams = GetCurrentParameters |> agent.PostAndAsyncReply

            return { setPointParameters = setPointParams
                     outputParameters = outputParams
                     operatingParameters = operatingParams 
                     currentParameters = currentParams } } 

    member this.InitiateRampToZero() =
        SetRampTarget(Zero) |> agent.Post
        SetRampRate(this.RampRateLimit) |> agent.Post
        SetPause(false) |> agent.Post

    member this.RampToZeroAsync() = 
        async { 
            this.InitiateRampToZero()
            do! this.WaitToReachTargetAsync() }

    member this.WaitToReachTargetAsync() =
        let rec loop() = async {
            let! currentParams = agent.PostAndAsyncReply(GetCurrentParameters)
            if not currentParams.reachedTarget 
            then do! loop() }
        loop()

    member this.WaitToReachZeroAndSetCurrentDirectionAsync currentDirection =
        let rec loop() = async {
            let! outputParams = agent.PostAndAsyncReply(GetOutputParameters)
            if not (outputParams.outputCurrent = 0.0<A>)
            then do! loop() }
        
        async {
            do! loop()
            SetCurrentDirection(currentDirection) |> agent.Post }

    member this.RampToZeroAndSetCurrentDirectionAsync currentDirection = 
        async {
            do! this.RampToZeroAsync()
            do! this.WaitToReachZeroAndSetCurrentDirectionAsync currentDirection }

    interface IDisposable with
        member this.Dispose() =
            PrepareToCloseSession |> agent.PostAndReply
            session.Dispose()

    member this.RampRateLimit = parameters.rampRateLimit
    member this.TripVoltageLimit = parameters.tripVoltageLimit
    member this.MaximumCurrent = parameters.maximumCurrent
    member this.CurrentLimit = parameters.currentLimit
    member this.StaticField = parameters.staticField
    member this.FieldCalibration = parameters.fieldCalibration
    member this.OutputResolutionInBits = parameters.outputResolutionInBits
    member this.SetPointDecimalPlaces = parameters.setPointDecimalPlaces
    member this.ShuntCalibration = parameters.shuntCalibration

    member this.GetOutputParametersAsync() =
        GetOutputParameters
        |> agent.PostAndAsyncReply

    member this.GetCurrentParametersAsync() =
        GetCurrentParameters
        |> agent.PostAndAsyncReply

    member this.GetOperatingParametersAsync() =
        GetOperatingParameters
        |> agent.PostAndAsyncReply

    member this.GetSetPointParametersAsync() =
        GetSetPointParameters
        |> agent.PostAndAsyncReply

    member this.SetRampRate(rampRate) =
        if rampRate > this.RampRateLimit then 
            failwith (String.Format("Cannot set magnet controller ramp rate greater than {0} A/s.", this.RampRateLimit))
        if rampRate < 0.0<A/s> then
            failwith "Cannot set magnet controller ramp rate to a negative value."

        SetRampRate(rampRate)
        |> agent.Post

    member this.SetRampRateByIndex(index) =
        this.CurrentRampRateForIndex(index)
        |> this.SetRampRate

    member this.SetTripVoltage(tripVoltage) =
        if tripVoltage > this.TripVoltageLimit then 
            failwith (String.Format("Cannot set magnet controller trip voltage greater than {0} V.", this.TripVoltageLimit))
        if tripVoltage < 0.0<V> then
            failwith "Cannot set magnet controller trip voltage to a negative value."

        SetTripVoltage(tripVoltage)
        |> agent.Post

    member this.SetCurrentDirection(currentDirection) =
        SetCurrentDirection(currentDirection)
        |> agent.Post

    member this.SetLowerSetPoint(lowerCurrentLimit) =
        if lowerCurrentLimit > this.CurrentLimit then
            failwith (String.Format("Cannot set magnet controller current limit to a value greater than {0} A.", this.CurrentLimit))
        if lowerCurrentLimit < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value"

        SetLowerSetPoint(lowerCurrentLimit)
        |> agent.Post
        
    member this.SetUpperSetPoint(upperCurrentLimit) =
        if upperCurrentLimit > this.CurrentLimit then
            failwith (String.Format("Cannot set magnet controller current limit to a value greater than {0} A.", this.CurrentLimit))
        if upperCurrentLimit < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value"
        
        SetUpperSetPoint(upperCurrentLimit)
        |> agent.Post

    member this.SetRampTarget(rampRateInAmpsPerSec) =
        SetRampTarget(rampRateInAmpsPerSec)
        |> agent.Post

    member this.SetPause(pause) =
        SetPause(pause)
        |> agent.Post

    member this.AvailableCurrentRampRates =
        parameters.calibratedRampRates
        |> Seq.filter (fun rampRate -> rampRate <= this.RampRateLimit)
        |> Seq.sort

    member this.CurrentRampRateForIndex (index) =
        if index >= (Seq.length this.AvailableCurrentRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        this.AvailableCurrentRampRates
        |> Seq.nth (index)

    member this.AvailableFieldRampRates =
        this.AvailableCurrentRampRates
        |> Seq.map (fun rampRate -> rampRate * abs(this.FieldCalibration))
    
    member this.FieldRampRateForIndex (index) =
        if index >= (Seq.length this.AvailableFieldRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        this.AvailableFieldRampRates
        |> Seq.nth (index)

    member this.NumberOfCurrentSteps =
        int (2.0 ** (float this.OutputResolutionInBits))

    member this.CurrentStep =
        this.MaximumCurrent / (float (this.NumberOfCurrentSteps - 1))

    member this.CurrentForIndex (index) =
        if abs(index) >= this.NumberOfCurrentSteps
            then failwith "Current index out of range."
        
        this.CurrentStep * (float index)

    member this.FieldStep =
        this.CurrentStep * abs(this.FieldCalibration)

    member this.FieldForIndex (index) =
        this.CurrentForIndex(index) * this.FieldCalibration

    member this.MaximumField =
        this.StaticField + abs(this.FieldCalibration * this.CurrentLimit)

    member this.MinimumField =
        this.StaticField - abs(this.FieldCalibration *  this.CurrentLimit)

    member this.NearestDigitisedCurrent(current) =
        current
        |> max this.CurrentLimit
        |> min -this.CurrentLimit
        |> fun current -> round(current / this.CurrentStep) * this.CurrentStep

    member this.NearestDigitisedField(field) =
        (field - this.StaticField) / this.FieldCalibration
        |> this.NearestDigitisedCurrent
        |> (fun nearestCurrent -> nearestCurrent * this.FieldCalibration)
    
    member this.NearestDigitisedCurrentRampRate(rampRate) =
        this.AvailableCurrentRampRates
        |> Seq.minBy (fun digitisedRampRate -> abs(digitisedRampRate - rampRate))

    member this.NearestDigitisedFieldRampRate(rampRate) =
        rampRate / abs(this.FieldCalibration)
        |> this.NearestDigitisedCurrentRampRate
        |> (fun nearestRampRate -> nearestRampRate * abs(this.FieldCalibration))

    member this.NearestDigitisedCurrentRampRateIndex(rampRate) =
        let digitisedRampRate = this.NearestDigitisedCurrentRampRate(rampRate)
        Seq.findIndex (fun value -> value = digitisedRampRate) this.AvailableCurrentRampRates
   
    member this.NearestDigitisedFieldRampRateIndex(rampRate) =
        let digitisedRampRate = this.NearestDigitisedCurrentRampRate(rampRate / abs(this.FieldCalibration))
        Seq.findIndex (fun value -> value = digitisedRampRate) this.AvailableCurrentRampRates
    
    member this.MaximumShuntVoltage =
        this.MaximumCurrent * this.ShuntCalibration

    member this.ShuntStep =
        this.CurrentStep * this.ShuntCalibration

    member this.NearestDigitisedOutputIndexForShuntVoltage(voltage) =
        int (round(voltage / this.ShuntStep))
    
    member this.NearestDigitisedCurrentForShuntVoltage(voltage) =
        this.NearestDigitisedOutputIndexForShuntVoltage(voltage)
        |> this.CurrentForIndex

    member this.NearestDigitisedFieldForShuntVoltage(voltage) =
        this.NearestDigitisedOutputIndexForShuntVoltage(voltage)
        |> this.FieldForIndex
