namespace Endorphin.Instrument.TwickenhamSmc

open Endorphin.Core.StringUtils
open Endorphin.Core.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Represents the Twickenham magnet controller current direction.
type CurrentDirection =
    /// Forward current (typically opposing the main magnet).
    | Forward
    /// Reverse current (typically alligned with the main magnet).
    | Reverse

    with

    /// Gives the string representation of a CurrentDirection as used in hardware commands and responses.
    member currentDirection.CommandString() =
        match currentDirection with
        | Forward -> "0"
        | Reverse -> "1"

/// Represents the target set point of a Twicknham magnet controller.
type RampTarget = 
    /// Zero current.
    | Zero
    /// Lower current set point.
    | Lower
    /// Upper current set point.
    | Upper

    with 
    
    /// Gives the string representation of a RampTarget as used in hardware commands and responses.
    member rampTarget.CommandString() =
        match rampTarget with
        | Zero -> "0"
        | Lower -> "1"
        | Upper -> "2"

[<AutoOpen>]
module Utils =
    /// Active pattern which parses a magnet controller response substring into a RampTarget.
    let (|ParseRampTarget|_|) = 
        function 
        | "0" -> Some Zero
        | "1" -> Some Lower
        | "2" -> Some Upper
        | _ -> None // no match unless the string is "0", "1" or "2"

    // Active pattern which parses a magnet controller response substring into a CurrentDirection.
    let (|ParseCurrentDirection|_|) = 
        function 
        | "0" -> Some Forward
        | "1" -> Some Reverse
        | _ -> None // no match unless the string is "0" or "1"

/// Groups the values returned when the Twickham magnet controller's output parameters are requested.
type OutputParameters = 
    { /// Output current in amps.
      OutputCurrent : float<A>
      /// Output voltage in volts.
      OutputVoltage : float<V>
      /// Current manget controller ramp target.
      RampTarget : RampTarget }

    with
    
    /// Parses a magnet controller output parameter response string.
    static member Parse responseString =
        // regex for string format "I(snnn.nnn)V(snn.n)R(d)[A/V]" where s is +/-, n can be any digit and d is 0, 1 or 2
        // the () brackets indicate the string capture groups
        let regex = @"\GI([\+\-]\d{3}\.\d{3})V([\+\-]\d{2}.\d)R([012])[AV]\s$"
            
        // extract the match groups from the response into a list and then parse the elements accordingly
        match responseString with
        | ParseRegex regex [ ParseFloat i; ParseFloat v; ParseRampTarget r ] ->
            { OutputCurrent = i * 1.0<A>
              OutputVoltage = v * 1.0<V>
              RampTarget = r }
        | _ -> 
            // raise an exception if the string does not match the required format
            failwithf "Invalid magnet controller output parameter string %s." responseString

/// Groups the values returned when the Twickenham magnet controller's current parameters are requested.
type CurrentParameters = 
    { /// Magnet controller ramp target.
      RampTarget : RampTarget
      /// Indicates whether the magnet controller has reached its ramp target.
      ReachedTarget : bool
      /// Indicates whether the magnet controller is paused.
      IsPaused : bool }

    with
    
    // Parses a magnet controller current parameter response string.
    static member Parse responseString = 
        // regex for string format "R(d)M(d)P(d)XdHdZ0.00EddQsnnn.nnn" where d can take specific digit values in each instance, s is +/-
        // and n can be any digit
        // the () brackets indicate the string capture groups
        let regex = @"\GR([012])M([01])P([01])X[0-5]H[012]Z0\.00E[0-3][0-7]Q[\+\-\s]\d{3}\.\d{3}\s$"
            
        // extract the match groups from the response into a list and then parse the elements accordingly
        match responseString with
        | ParseRegex regex [ ParseRampTarget r; ParseIntegerBool m; ParseIntegerBool p ] ->
            { RampTarget = r 
              ReachedTarget = m
              IsPaused = p }
        | _ -> 
            // raise an exception if the string does not match the required format
            failwithf "Invalid magnet controller current parameter string %s." responseString

type OperatingParameters = 
    { /// Ramp rate in amps per second.
      RampRate : float<A/s>
      /// Magnet controller current direction.
      CurrentDirection : CurrentDirection }

    with
    
    // Parses a magnet controller operating parameter response string.
    static member Parse responseString =
        // regex for string format "A(nn.nnnnn)D(d)TdBdWnnn.C0.nnnnnn" where n can be any digit and d can be 0 or 1 in each case
        // the () brackets indicate the string capture groups
        let regex = @"\GA(\d{2}\.\d{5})D([01])T[01]B[01]W\d{3}\.C0\.\d{6}\s$"
            
        // extract the match groups from the response into a list and then parse the elements accordingly
        match responseString with
        | ParseRegex regex [ ParseFloat a; ParseCurrentDirection d ] ->
            { RampRate = a * 1.0<A/s>
              CurrentDirection = d }
        | _ -> 
            // raise an exception if the string does not match the required format
            failwithf "Invalid magnet controller operating parameter string %s." responseString

/// Groups the values returned when the Twickenham magnet controller's set point parameters are requested.
type SetPointParameters = 
    { /// Lower set point current in amps. Note that this will only be read to three decimal places even if the magnet
      /// controller has sufficiently high resolution to set the fourth decimal place. This is the case on low output
      /// current models.
      LowerSetPoint : float<A>
      /// Upper set point current in amps. Note that this will only be read to three decimal places even if the magnet
      /// controller has sufficiently high resolution to set the fourth decimal place. This is the case on low output
      /// current models.
      UpperSetPoint : float<A>
      /// The back EMF trip voltage at which a ramp will be terminated.
      TripVoltage : float<V> }

    with

    static member Parse responseString = 
        // regex for string format "TdU(nnn.nnnn)L(nnn.nnn)Y(nn.n)" where d can be 0 or 1 and n can be any digit in each case
        // the () brackets indicate the string capture groups
        let regex = @"\GT[01]U(\d{3}\.\d{3})L(\d{3}\.\d{3})Y(\d{2}\.\d)\s$"

        // extract the match groups from the response into a list and then parse the elements accordingly
        match responseString with
        | ParseRegex regex [ ParseFloat u; ParseFloat l; ParseFloat y ] ->
            { LowerSetPoint = l * 1.0<A>
              UpperSetPoint = u * 1.0<A>
              TripVoltage = y * 1.0<V> }
        | _ -> 
            // raise an exception if the string does not match the required format
            failwithf "Invalid magnet controller set point parameter string %s" responseString

/// Groups together all parameters which can be obtained about the Twickenham magnet controller state.      
type AllParameters = 
    { /// The magnet controller SetPointParameters.
      SetPointParameters : SetPointParameters
      /// The magnet controller OutputParameters.
      OutputParameters : OutputParameters
      /// The magnet controller OperatingParameters.
      OperatingParameters : OperatingParameters
      /// The magnet controller CurrentParameters.
      CurrentParameters : CurrentParameters }

/// Represents the device parameters for a Twickenham magnet controller and the magnet to which it is connected.
type MagnetControllerParameters = 
    { /// The static (zero-current) field of the magnet in tesla.
      StaticField : float<T>
      /// The calibration constant for the magnet to which the magnet controller is connected in tesla per amp.
      FieldCalibration : float<T/A>
      /// The maximum ramp rate allowed on the device in amps per second . Although the device may be capable of
      /// ramping the current more quickly, this is implemented as a software safety check to prevent excessively
      /// large ramp rates from being used.
      RampRateLimit : float<A/s>
      /// The maximum trip voltage allowed to be set to the device in volts. Although the device may be capable of
      /// setting larger trip voltages, this is implemented as a software safety check to prevent the trip voltage
      /// from being set to an excessively large value.
      TripVoltageLimit : float<V>
      /// The maximum current of the Twickenham magnet controller model in amps. This is used to determine the digital
      /// output steps of the magnet controller current.
      MaximumCurrent : float<A>
      /// The maximum current allowed to be set to the device in amps. Although the device may be capable of producing
      /// larger currents, this is implemented as a software safety check to prevent set point limits, and therefore
      /// output current, from being set to an excessively large value.
      CurrentLimit : float<A>
      /// The voltage offset of the magnet controller readout shunt (including any preamp used to measure it) in volts.
      ShuntOffset : float<V>
      /// The calibration constant of the magnet controller readout shunt (including any preamp used to measure it) in
      /// volts per amp. Note that the readout for a given (absolute) output current does not change with the magnet 
      /// controller current direction.
      ShuntCalibration : float<V/A>
      /// The amplitude of the shunt noise and/or mains hum in the readout shunt (including any preamp used to measure
      /// it) in volts. This can be used to automatically set the appropriate voltage range on an oscilloscope when
      /// sampling the shunt voltage, for exmaple.
      ShuntNoise : float<V>
      /// The output resolution of the magnet controller in bits. Typically 16 bit.
      OutputResolution : int<bits>
      /// The collection of calibrated ramp rates which can be set to the magnet controller in amps per second.
      CalibratedRampRates : float<A/s> seq }
    
    /// Formats a set point current with the appropriate precision for a magnet controller command string
    member internal magnetControllerParams.FormatSetPoint (setPointCurrent : float<A>) =
        // the command string has 6 significant figures so use the 4 decimal place format for models with maximum output
        // current under 100.0 A and the 3 decimal place format for models with output current over this
        if magnetControllerParams.MaximumCurrent < 100.0<A> then 
            sprintf "%07.4f" (float setPointCurrent)
        else
            sprintf "%07.3f" (float setPointCurrent)
    
    /// The collection of current ramp rates avalable on the magnet controller in amps per second, below the ramp rate
    /// limit in ascending order.
    member magnetControllerParams.AvailableCurrentRampRates =
        magnetControllerParams.CalibratedRampRates
        |> Seq.filter (fun rampRate -> rampRate <= magnetControllerParams.RampRateLimit)
        |> Seq.sort

    /// The calibrated magnet controller current ramp rate for a given ramp rate index in amps per second.
    member magnetControllerParams.CurrentRampRateForIndex index =
        // raise an exception if the index is out of bounds
        if index >= (Seq.length magnetControllerParams.AvailableCurrentRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        Seq.nth index (magnetControllerParams.AvailableCurrentRampRates) 

    /// The collection of field ramp rates available on the magnet controller in tesla per second (converted from current
    /// ramp rates to field ramp rates using the calibration constant), below the ramp rate limit in ascending order.
    member magnetControllerParams.AvailableFieldRampRates =
        magnetControllerParams.AvailableCurrentRampRates
        |> Seq.map (fun rampRate -> rampRate * abs(magnetControllerParams.FieldCalibration))
    
    /// The calibrated magnet controller field ramp rate for a given ramp rate index in tesla per second.
    member magnetControllerParams.FieldRampRateForIndex index =
        if index >= (Seq.length magnetControllerParams.AvailableFieldRampRates) || index < 0
            then failwith "Ramp rate index out of range."
            
        Seq.nth index (magnetControllerParams.AvailableFieldRampRates)

    /// The number of digitised output steps available on the magnet controller as determined by the output resolution.
    member magnetControllerParams.NumberOfCurrentSteps =
        int (2.0 ** (float magnetControllerParams.OutputResolution))

    /// The digital current step between individual magnet controller output steps in amps, as determined by the output
    /// resolutionand maximum current.
    member magnetControllerParams.CurrentStep =
        magnetControllerParams.MaximumCurrent / (float (magnetControllerParams.NumberOfCurrentSteps - 1))

    /// The outpuc current in amps for a (signed) magnet controller output index.
    member magnetControllerParams.CurrentForIndex index =
        // if the current is out of bounds, raise an exception
        if abs index >= magnetControllerParams.NumberOfCurrentSteps then
            failwith "Current index out of range."
        
        magnetControllerParams.CurrentStep * (float index)

    /// The digital field step between individual magnet controller steps in tesla, as determined by the output resolution,
    /// maximum current and field calibration constant. 
    member magnetControllerParams.FieldStep =
        magnetControllerParams.CurrentStep * (abs magnetControllerParams.FieldCalibration)

    /// The field in tesla for a (signed) magnet controller output index.
    member magnetControllerParams.FieldForIndex index =
        magnetControllerParams.StaticField + (magnetControllerParams.CurrentForIndex index) * magnetControllerParams.FieldCalibration

    /// The shunt voltage step between individual magnet controller output steps in volts.
    member magnetControllerParams.ShuntStep =
        magnetControllerParams.CurrentStep * magnetControllerParams.ShuntCalibration

    /// The shunt voltage at the maximum output current of the manget controller in volts.
    member magnetControllerParams.MaximumShuntVoltage =
        magnetControllerParams.ShuntOffset + magnetControllerParams.MaximumCurrent * magnetControllerParams.ShuntCalibration

    /// The shunt voltage for a magnet controller output index. Note that negative and positive output indecies give the same
    /// shunt voltage, as is the case for the magnet controller. 
    member magnetControllerParams.ShuntVoltageForIndex index =
        magnetControllerParams.ShuntOffset + magnetControllerParams.CurrentForIndex (abs index) * magnetControllerParams.ShuntCalibration

    /// The output current in amps (not digitised) for a given shunt voltage in volts. Note that this value does not change 
    /// with the magnet controller current direction and only indicates the absolute value of the output current.
    member magnetControllerParams.CurrentForShuntVoltage shuntVoltage =
        (shuntVoltage - magnetControllerParams.ShuntOffset) / magnetControllerParams.ShuntCalibration

    /// The magnet field in tesla for a given magnet controller current direction and shunt voltage in volts.
    member magnetControllerParams.FieldForShuntVoltage currentDirection voltage =
        match currentDirection with
        | Forward -> magnetControllerParams.StaticField + (magnetControllerParams.CurrentForShuntVoltage voltage) * magnetControllerParams.FieldCalibration
        | Reverse -> magnetControllerParams.StaticField - (magnetControllerParams.CurrentForShuntVoltage voltage) * magnetControllerParams.FieldCalibration

    /// The digitised output index for a given magnet controller current in amps.
    member magnetControllerParams.OutputIndexForCurrent current =
        // if the current exceeds the maximum current then raise an exception
        if abs current > magnetControllerParams.MaximumCurrent then
            failwith "Current outside current limit."

        int (round (current / magnetControllerParams.CurrentStep))

    /// The digitised output index for a given magnet controller field in tesla.
    member magnetControllerParams.OutputIndexForField field =
        // if the field is out of the available magnet controller current range then raise an exception
        if (field > magnetControllerParams.MaximumField) || (field < magnetControllerParams.MinimumField) then
            failwith "Field outside of field range."

        int (round ((field - magnetControllerParams.StaticField) / (magnetControllerParams.CurrentStep * magnetControllerParams.FieldCalibration)))
    
    /// The maximum field which can be generated by the magnet, at the alligned magnet controller current limit.
    member magnetControllerParams.MaximumField =
        magnetControllerParams.StaticField + abs (magnetControllerParams.FieldCalibration * magnetControllerParams.CurrentLimit)

    /// The minimum field which can be generated by the magnet, at the opposing magnet controller current limit.
    member magnetControllerParams.MinimumField =
        magnetControllerParams.StaticField - abs (magnetControllerParams.FieldCalibration * magnetControllerParams.CurrentLimit)

    /// The nearest digital current step for a given current in amps.
    member magnetControllerParams.NearestDigitisedCurrent current =
        current
        |> max -magnetControllerParams.CurrentLimit
        |> min magnetControllerParams.CurrentLimit
        |> fun current -> round(current / magnetControllerParams.CurrentStep) * magnetControllerParams.CurrentStep

    /// The nearest digital field step for a given field in tesla.
    member magnetControllerParams.NearestDigitisedField field =
        (field - magnetControllerParams.StaticField) / magnetControllerParams.FieldCalibration
        |> magnetControllerParams.NearestDigitisedCurrent
        |> fun nearestCurrent -> magnetControllerParams.StaticField + nearestCurrent * magnetControllerParams.FieldCalibration
    
    /// The nearest available calibrated magnet controller ramp rate in amps per second.
    member magnetControllerParams.NearestCalibratedCurrentRampRate rampRate =
        magnetControllerParams.AvailableCurrentRampRates
        |> Seq.minBy (fun digitisedRampRate -> abs(digitisedRampRate - rampRate))

    /// The nearest avialble calibrated magnet controller ramp rate in tesla per second.
    member magnetControllerParams.NearestCalibratedFieldRampRate rampRate =
        rampRate / (abs magnetControllerParams.FieldCalibration)
        |> magnetControllerParams.NearestCalibratedCurrentRampRate
        |> fun nearestRampRate -> nearestRampRate * (abs magnetControllerParams.FieldCalibration)

    /// The nearest available calibrated magnet controller ramp rate index for a given ramp rate in amps per second.
    member magnetControllerParams.NearestCalibratedCurrentRampRateIndex rampRate =
        let digitisedRampRate = (magnetControllerParams.NearestCalibratedCurrentRampRate rampRate)
        Seq.findIndex ((=) digitisedRampRate) magnetControllerParams.AvailableCurrentRampRates
   
    /// The nearest available calibrated magnet controller ramp rate index for a given ramp rate in tesla per second.
    member magnetControllerParams.NearestCalibratedFieldRampRateIndex rampRate =
        let digitisedRampRate = magnetControllerParams.NearestCalibratedCurrentRampRate (rampRate / (abs magnetControllerParams.FieldCalibration))
        Seq.findIndex ((=) digitisedRampRate) magnetControllerParams.AvailableCurrentRampRates
