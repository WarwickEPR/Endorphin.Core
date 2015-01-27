﻿namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core
open Endorphin.Core.StringUtils
open Endorphin.Core.Units
open Microsoft.FSharp.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NationalInstruments.VisaNS
open System
open log4net

type CurrentDirection = 
    | Forward
    | Reverse

type RampTarget = 
    | Zero
    | Lower
    | Upper

type OutputParameters = 
    { OutputCurrent : float<A>
      OutputVoltage : float<V>
      RampTarget : RampTarget }

type CurrentParameters = 
    { RampTarget : RampTarget
      ReachedTarget : bool
      IsPaused : bool }

type OperatingParameters = 
    { RampRate : float<A/s>
      CurrentDirection : CurrentDirection }

type SetPointParameters = 
    { LowerSetPoint : float<A>
      UpperSetPoint : float<A>
      TripVoltage : float<V> }

type MagnetControllerParameters = 
    { StaticField : float<T>
      FieldCalibration : float<T/A>
      RampRateLimit : float<A/s>
      TripVoltageLimit : float<V>
      MaximumCurrent : float<A>
      CurrentLimit : float<A>
      ShuntOffset : float<V>
      ShuntCalibration : float<V/A>
      ShuntNoise : float<V>
      OutputResolution : int<bits>
      SetPointPrecision : int<dp>
      CalibratedRampRates : float<A/s> seq }
      
    member this.AvailableCurrentRampRates =
        this.CalibratedRampRates
        |> Seq.filter (fun rampRate -> rampRate <= this.RampRateLimit)
        |> Seq.sort

    member this.CurrentRampRateForIndex index =
        if index >= (Seq.length this.AvailableCurrentRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        Seq.nth index (this.AvailableCurrentRampRates) 

    member this.AvailableFieldRampRates =
        this.AvailableCurrentRampRates
        |> Seq.map (fun rampRate -> rampRate * abs(this.FieldCalibration))
    
    member this.FieldRampRateForIndex index =
        if index >= (Seq.length this.AvailableFieldRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        Seq.nth index (this.AvailableFieldRampRates)

    member this.NumberOfCurrentSteps =
        int (2.0 ** (float this.OutputResolution))

    member this.CurrentStep =
        this.MaximumCurrent / (float (this.NumberOfCurrentSteps - 1))

    member this.CurrentForIndex index =
        if abs index >= this.NumberOfCurrentSteps then
            failwith "Current index out of range."
        
        this.CurrentStep * (float index)

    member this.FieldStep =
        this.CurrentStep * (abs this.FieldCalibration)

    member this.FieldForIndex index =
        this.StaticField + (this.CurrentForIndex index) * this.FieldCalibration

    member this.ShuntVoltageForIndex index =
        this.ShuntOffset + (this.CurrentForIndex index) * this.ShuntCalibration

    member this.FieldForShuntVoltage voltage =
        ((this.CurrentForShuntVoltage voltage) * this.FieldCalibration) + this.StaticField

    member this.IndexForCurrent current =
        if abs current > this.CurrentLimit then
            failwith "Current outside current limit."

        int (round (current / this.CurrentStep))

    member this.IndexForField field =
        if (field > this.MaximumField) || (field < this.MinimumField) then
            failwith "Field outside of field range."

        int (round ((field - this.StaticField) / (this.CurrentStep * this.FieldCalibration)))

    member this.MaximumField =
        this.StaticField + abs (this.FieldCalibration * this.CurrentLimit)

    member this.MinimumField =
        this.StaticField - abs (this.FieldCalibration * this.CurrentLimit)

    member this.NearestDigitisedCurrent current =
        current
        |> max -this.CurrentLimit
        |> min this.CurrentLimit
        |> fun current -> round(current / this.CurrentStep) * this.CurrentStep

    member this.NearestDigitisedField field =
        (field - this.StaticField) / this.FieldCalibration
        |> this.NearestDigitisedCurrent
        |> fun nearestCurrent -> this.StaticField + nearestCurrent * this.FieldCalibration
    
    member this.NearestDigitisedCurrentRampRate rampRate =
        this.AvailableCurrentRampRates
        |> Seq.minBy (fun digitisedRampRate -> abs(digitisedRampRate - rampRate))

    member this.NearestDigitisedFieldRampRate rampRate =
        rampRate / (abs this.FieldCalibration)
        |> this.NearestDigitisedCurrentRampRate
        |> fun nearestRampRate -> nearestRampRate * (abs this.FieldCalibration)

    member this.NearestDigitisedCurrentRampRateIndex rampRate =
        let digitisedRampRate = (this.NearestDigitisedCurrentRampRate rampRate)
        Seq.findIndex ((=) digitisedRampRate) this.AvailableCurrentRampRates
   
    member this.NearestDigitisedFieldRampRateIndex rampRate =
        let digitisedRampRate = this.NearestDigitisedCurrentRampRate (rampRate / (abs this.FieldCalibration))
        Seq.findIndex ((=) digitisedRampRate) this.AvailableCurrentRampRates
    
    member this.MaximumShuntVoltage =
        this.ShuntOffset + this.MaximumCurrent * this.ShuntCalibration

    member this.ShuntStep =
        this.CurrentStep * this.ShuntCalibration

    member this.CurrentForShuntVoltage shuntVoltage =
        (shuntVoltage - this.ShuntOffset) / this.ShuntCalibration

type AllParameters = 
    { SetPointParameters : SetPointParameters
      OutputParameters : OutputParameters
      OperatingParameters : OperatingParameters
      CurrentParameters : CurrentParameters }

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

type MagnetController(session : MessageBasedSession, deviceParameters : MagnetControllerParameters) =
    static let log = LogManager.GetLogger typeof<MagnetController>
    
    let sessionReleased = new Event<unit>()
    
    let agent = 
        Agent.Start(fun mailbox -> 
            let (|ParseCurrentDirection|_|) = 
                function 
                | "0" -> Some Forward
                | "1" -> Some Reverse
                | _ -> None
        
            let (|ParseRampTarget|_|) = 
                function 
                | "0" -> Some Zero
                | "1" -> Some Lower
                | "2" -> Some Upper
                | _ -> None
        
            let parseOutputParameters response = 
                // regex for string format "Isnnn.nnnVsnn.nRd[A/V]" where s is +/-, n can be any digit
                // and d is 0, 1 or 2   
                sprintf "Twichenham SMC %s parsing output parameter string %s." (session.ResourceName) response |> log.Info
                let regex = @"\GI([\+\-]\d{3}\.\d{3})V([\+\-]\d{2}.\d)R([012])[AV]\s$"
                match response with
                | ParseRegex regex [ ParseFloat i; ParseFloat v; ParseRampTarget r ] ->
                    { OutputCurrent = i * 1.0<A>
                      OutputVoltage = v * 1.0<V>
                      RampTarget = r }
                | _ -> failwith "Invalid magnet controller output parameter string"
        
            let parseCurrentParameters response = 
                // regex for string format "RdMdPdXdHdZ0.00EddQsnnn.nnn" where d can take specific digit
                // values in each instance, s is +/- and n can be any digit
                sprintf "Twichenham SMC %s parsing current parameter string %s." (session.ResourceName) response |> log.Info
                let regex = @"\GR([012])M([01])P([01])X[0-5]H[012]Z0\.00E[0-3][0-7]Q[\+\-\s]\d{3}\.\d{3}\s$"
                match response with
                | ParseRegex regex [ ParseRampTarget r; ParseIntegerBool m; ParseIntegerBool p ] ->
                    { RampTarget = r 
                      ReachedTarget = m
                      IsPaused = p }
                | _ -> failwith "Invalid magnet controller current parameter string"

            let parseOperatingParameters response = 
                // regex for string format "Ann.nnnnnDdTdBdWnnn.C0.nnnnnn" where n can be any digit and
                // d can be 0 or 1 in each case 
                sprintf "Twichenham SMC %s parsing operating parameter string %s." (session.ResourceName) response |> log.Info
                let regex = @"\GA(\d{2}\.\d{5})D([01])T[01]B[01]W\d{3}\.C0\.\d{6}\s$"
                match response with
                | ParseRegex regex [ ParseFloat a; ParseCurrentDirection d ] ->
                    { RampRate = a * 1.0<A/s>
                      CurrentDirection = d }
                | _ -> failwith "Invalid magnet controller operating parameter string"
        
            let parseSetPointParameters response = 
                // regex for string format "TdUnnn.nnnnLnnn.nnnYnn.n" where d can be 0 or 1 and n can be
                // any digit in each case
                sprintf "Twichenham SMC %s parsing set point parameter string %s." (session.ResourceName) response |> log.Info
                let regex = @"\GT[01]U(\d{3}\.\d{3})L(\d{3}\.\d{3})Y(\d{2}\.\d)\s$"
                match response with
                | ParseRegex regex [ ParseFloat u; ParseFloat l; ParseFloat y ] ->
                    { LowerSetPoint = l * 1.0<A>
                      UpperSetPoint = u * 1.0<A>
                      TripVoltage = y * 1.0<V> }
                | _ -> failwith "Invalid magnet controller set point parameter string"
        
            let rec loop () = async {
                sprintf "(Re)entering Twickenham SMC %s agent loop." (session.ResourceName) |> log.Info

                let! message = mailbox.Receive()
                sprintf "Twickenham SMC %s received message %A." (session.ResourceName) message |> log.Info 
                
                match message with
                
                | ReleaseSession ->
                    sprintf "Twickenham SMC %s releasing session." (session.ResourceName) |> log.Info
                    if mailbox.CurrentQueueLength <> 0 then
                        failwithf "Twickenham SMC %s received ReleaseSession message when message queue is non-empty." (session.ResourceName)
                    sessionReleased.Trigger()
                    
                // Requests

                | GetOutputParameters replyChannel ->
                    let! response = session.QuerryAsync "G\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." (session.ResourceName) response |> log.Info
                    let outputParameters = response |> parseOutputParameters
                    sprintf "Twickenham SMC %s replying to output parameter request with\n%A." (session.ResourceName) outputParameters |> log.Info
                    outputParameters |> replyChannel.Reply
                    return! loop ()

                | GetCurrentParameters replyChannel ->
                    let! response = session.QuerryAsync "K\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." (session.ResourceName) response |> log.Info
                    let currentParameters = response |> parseCurrentParameters
                    sprintf "Twickenham SMC %s replying to currenet parameter request with\n%A."
                        (session.ResourceName) currentParameters |> log.Info
                    currentParameters |> replyChannel.Reply
                    return! loop ()

                | GetOperatingParameters replyChannel ->
                    let! response = session.QuerryAsync "O\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." (session.ResourceName) response |> log.Info
                    let operatingParameters = response |> parseOperatingParameters
                    sprintf "Twickenham SMC %s replying to operating parameter request with\n%A."
                        (session.ResourceName) operatingParameters |> log.Info
                    operatingParameters |> replyChannel.Reply
                    return! loop ()

                | GetSetPointParameters replyChannel ->
                    let! response = session.QuerryAsync "S\r\n"
                    sprintf "Twickenham SMC %s received response from hardware: %s." (session.ResourceName) response |> log.Info
                    let setPointParameters = response |> parseSetPointParameters
                    sprintf "Twickenham SMC %s replying to set-point parameter request with\n%A."
                        (session.ResourceName) setPointParameters |> log.Info
                    setPointParameters |> replyChannel.Reply
                    return! loop ()
                
                // Commands

                | SetRampRate rampRate -> 
                    sprintf "Twickenham SMC %s setting ramp rate to %08.5f A/s." (session.ResourceName) (float rampRate) |> log.Info
                    do! 
                        sprintf "A%08.5f" (float rampRate)
                        |> session.WriteAsync
                    do! Async.Sleep 1000
                    return! loop ()

                | SetTripVoltage tripVoltage -> 
                    sprintf "Twickenham SMC %s setting trip voltage to %04.1f V." (session.ResourceName) (float tripVoltage) |> log.Info
                    do!
                        sprintf "Y%04.1f" (float tripVoltage)
                        |> session.WriteAsync
                    do! Async.Sleep 1000
                    return! loop ()

                 | SetCurrentDirection currentDirection -> 
                    sprintf "Twickenham SMC %s setting current direction %A." (session.ResourceName) currentDirection |> log.Info
                    do! 
                        match currentDirection with
                        | Forward -> session.WriteAsync "D0"
                        | Reverse -> session.WriteAsync "D1"
                    do! Async.Sleep 1000
                    return! loop ()
                    
                | SetLowerSetPoint lowerSetPoint ->
                    sprintf "Twickenham SMC %s setting lower set point to %08.4f." (session.ResourceName) (float lowerSetPoint) |> log.Info
                    do! 
                        if lowerSetPoint < 100.0<A> then
                            sprintf "L%07.4f" (float lowerSetPoint)
                        else
                            sprintf "L%07.3f" (float lowerSetPoint) 
                        |> session.WriteAsync
                    do! Async.Sleep 1000
                    return! loop ()

                | SetUpperSetPoint upperSetPoint -> 
                    sprintf "Twickenham SMC %s setting upper set point to %08.4f." (session.ResourceName) (float upperSetPoint) |> log.Info
                    do! 
                        if upperSetPoint < 100.0<A> then
                            sprintf "U%07.4f" (float upperSetPoint)
                        else
                            sprintf "U%07.3f" (float upperSetPoint) 
                        |> session.WriteAsync
                    do! Async.Sleep 1000
                    return! loop ()

                | SetRampTarget rampTarget -> 
                    sprintf "Twickenham SMC %s setting ramp target to %A current limit." (session.ResourceName) rampTarget |> log.Info
                    do!
                        match rampTarget with
                        | Zero -> session.WriteAsync "R0"
                        | Lower -> session.WriteAsync "R1"
                        | Upper -> session.WriteAsync "R2"
                    do! Async.Sleep 1000
                    return! loop ()

                | SetPause pause ->
                    sprintf "Twickenham SMC %s setting pause %s." (session.ResourceName) (if pause then "on" else "off") |> log.Info 
                    do! 
                        if pause then session.WriteAsync "P1"
                        else session.WriteAsync "P0"
                    do! Async.Sleep 1000
                    return! loop () }

            // initialse the agent state
            loop())

    // Events

    member __.Error = agent.Error
    member __.SessionReleased = sessionReleased.Publish
    member __.DeviceParameters = deviceParameters

    // Public members for sending agent commands and requests

    interface IDisposable with
        member __.Dispose () =
            ReleaseSession |> agent.Post

    member __.GetOutputParametersAsync () =
        GetOutputParameters
        |> agent.PostAndAsyncReply

    member __.GetCurrentParametersAsync () =
        GetCurrentParameters
        |> agent.PostAndAsyncReply

    member __.GetOperatingParametersAsync () =
        GetOperatingParameters
        |> agent.PostAndAsyncReply

    member __.GetSetPointParametersAsync () =
        GetSetPointParameters
        |> agent.PostAndAsyncReply

    member __.SetRampRate rampRate =
        if rampRate > deviceParameters.RampRateLimit then 
            failwithf "Cannot set magnet controller ramp rate greater than %A A/s." deviceParameters.RampRateLimit
        if rampRate < 0.0<A/s> then
            failwith "Cannot set magnet controller ramp rate to a negative value."

        SetRampRate rampRate
        |> agent.Post

    member this.SetRampRateByIndex index =
        deviceParameters.CurrentRampRateForIndex index
        |> this.SetRampRate

    member __.SetTripVoltage tripVoltage =
        if tripVoltage > deviceParameters.TripVoltageLimit then 
            failwithf "Cannot set magnet controller trip voltage greater than %A V." deviceParameters.TripVoltageLimit
        if tripVoltage < 0.0<V> then
            failwith "Cannot set magnet controller trip voltage to a negative value."

        SetTripVoltage tripVoltage
        |> agent.Post

    member __.SetCurrentDirection currentDirection =
        SetCurrentDirection currentDirection
        |> agent.Post

    member __.SetLowerSetPoint lowerSetPoint =
        if lowerSetPoint > deviceParameters.CurrentLimit then
            failwithf "Cannot set magnet controller current limit to a value greater than %A A." deviceParameters.CurrentLimit
        if lowerSetPoint < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value"

        SetLowerSetPoint lowerSetPoint
        |> agent.Post

    member this.SetLowerSetPointByIndex currentIndex =
        currentIndex
        |> this.DeviceParameters.CurrentForIndex
        |> this.SetLowerSetPoint

    member __.SetUpperSetPoint upperSetPoint =
        if upperSetPoint > deviceParameters.CurrentLimit then
            failwithf "Cannot set magnet controller current limit to a value greater than %A A." deviceParameters.CurrentLimit
        if upperSetPoint < 0.0<A> then
            failwith "Cannot set magnet controller current limit to a negative value"
        
        SetUpperSetPoint upperSetPoint
        |> agent.Post

    member this.SetUpperSetPointByIndex currentIndex =
        currentIndex
        |> this.DeviceParameters.CurrentForIndex
        |> this.SetUpperSetPoint

    member __.SetRampTarget rampRateInAmpsPerSec =
        SetRampTarget rampRateInAmpsPerSec
        |> agent.Post

    member __.SetPause pause =
        SetPause pause
        |> agent.Post
        
    // Useful workflows, requests and commands which send multiple messages
    
    member this.GetAllParametersAsync () = 
        async {
            let! setPointParams = this.GetSetPointParametersAsync()
            let! outputParams = this.GetOutputParametersAsync()
            let! operatingParams = this.GetOperatingParametersAsync()
            let! currentParams = this.GetCurrentParametersAsync()

            return { SetPointParameters = setPointParams
                     OutputParameters = outputParams
                     OperatingParameters = operatingParams 
                     CurrentParameters = currentParams } } 
                     
    member this.WaitToReachTargetAsync () =
        let rec loop() = async {
            let! currentParams = this.GetCurrentParametersAsync()
            if not currentParams.ReachedTarget then
                do! loop() }
        loop()
        
    member this.WaitToReachZeroAndSetCurrentDirectionAsync currentDirection =
        let rec loop() = async {
            let! outputParams = this.GetOutputParametersAsync()
            if not (outputParams.OutputCurrent = 0.0<A>) then 
                do! loop() }
        
        async {
            do! loop()
            this.SetCurrentDirection currentDirection }

    member this.BeginRampToZero () =
        this.SetRampTarget Zero
        this.SetRampRate (deviceParameters.RampRateLimit)
        this.SetPause false

    member this.RampToZeroAsync () = 
        async { 
            this.BeginRampToZero()
            do! this.WaitToReachTargetAsync() }

    member this.RampToZeroAndSetCurrentDirectionAsync currentDirection = 
        async {
            do! this.RampToZeroAsync()
            do! this.WaitToReachZeroAndSetCurrentDirectionAsync currentDirection }