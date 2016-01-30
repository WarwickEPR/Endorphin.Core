// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.TwickenhamSmc

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open TwickenhamSmc

[<AutoOpen>]
/// Core instrument API functions.
module Instrument = 

    [<RequireQualifiedAccess>]
    /// Functions posting comands and performing queries to a Twickenham Scientific superconducting
    /// magnet controller.
    module MagnetController =

        /// Gets the settings for the magnet controller.
        let settings (MagnetController (_, settings)) = settings

        /// Opens a connection to a Twickenham Scientific superconducting magnet controller with the
        /// specified VISA address and settings.
        let openInstrument visaAddress timeout settings = async {
            // a 1s delay is required after write commands or the connection may fail
            let instrument = Visa.openGpibInstrument visaAddress timeout (Some 1000<ms>)
            let magnetController = MagnetController(instrument, settings)
            
            try // check whether the magnet controller responds to querying the output parameters
                // with the correct format...
                do! IO.queryOutputParameters Keys.outputParameters magnetController |> Async.Ignore
            with exn ->
                failwith "Failed to connect to magnet controller: unexpected response to query during initialisation."
            
            return magnetController }

        /// Closes a connection to a Twickenham Scientific superconducting magnet controller.
        let closeInstrument (MagnetController (magnetController, _)) =
            Visa.closeInstrument magnetController

        /// Queries the output parameters of the magnet controller, containing the output current,
        /// voltage and ramp target.
        let queryOutputParameters = IO.queryOutputParameters Keys.outputParameters

        /// Queries the current parameters of the magnet controller, containing the ramp target and
        /// flags indicating whether the target has been reached and whether ramping is paused.
        let queryCurrentParameters = IO.queryCurrentParrameters Keys.currentParameters
    
        /// Queries the operating parameters of the magnet controller, containing the ramp rate and
        /// current direction.
        let queryOperatingParameters = IO.queryOperatingParameters Keys.operatingParameters

        /// Queries the set-point parameters of the magnet controller, containing the lower and upper
        /// set points and the trip voltage.
        let querySetPointParameters = IO.querySetPointParameters Keys.setPointParameters

        /// Functions related to the magnet controller settings.
        module Settings =
        
            /// Returns the output resolution of the magnet controller in bits.
            let outputResolution (settings : Settings) = 16<bit>

            /// Returns the number of digital steps for the magnet controller output.
            let digitalOutputStepCount (settings : Settings) = System.UInt16.MaxValue

            /// Returns the maximum output current of the magnet controller.
            let maximumCurrent settings =
                settings.HardwareParameters.MaximumCurrent

            /// Returns the digital current step of the magnet controller.
            let currentStep settings = 
                (maximumCurrent settings)
                / (1.0M * decimal (digitalOutputStepCount settings))

            /// Returns the static field of the magnet controlled by the magnet controller.
            let staticField settings =
                settings.FieldCalibration.StaticField

            /// Returns the linear proportionality coefficient between current and magnetic field for
            /// the magnet controller.
            let linearFieldCoefficient settings =
                settings.FieldCalibration.LinearCoefficient

            /// Returns the digital magnetic field step of the magnet controller.
            let fieldStep settings =
                abs <| (currentStep settings) * (linearFieldCoefficient settings) 

            /// Returns the output monitoring shunt voltage offset of the magnet controller in volts.
            let shuntVoltageOffset settings =
                settings.ShuntCalibration.VoltageOffset

            /// Returns the linear proportionality coefficient between current and output monitoring 
            /// shunt voltage for the magnet controller.
            let linearShuntVoltageCoefficient settings =
                settings.ShuntCalibration.LinearCoefficient

            /// Returns the RMS voltage noise for the output monitoring shunt of the magnet controller.
            let shuntVoltageRmsNoise settings =
                settings.ShuntCalibration.RmsVoltageNoise

            /// Returns the output monitoring shunt voltage for the magnet controller at maximum output
            /// current.
            let maximumShuntVoltage settings =
                shuntVoltageOffset settings + (maximumCurrent settings) * (linearShuntVoltageCoefficient settings)

            /// Returns the output monitoring shunt voltage step size between digital output steps of 
            /// the magnet controller.
            let shuntVoltageStep settings =
                (currentStep settings) * (linearShuntVoltageCoefficient settings)

            /// Returns the list of calibrated ramp rates available on the magnet controller.
            let calibratedRampRates settings =
                settings.HardwareParameters.CalibratedRampRates

            /// Functions related to unit conversions.
            module Convert =

                /// Gives the current corresponding to the specified digital step indeex for the magnet
                /// controller.
                let stepIndexToCurrent magnetController (index : uint16) =
                    (1.0M * decimal index) * (currentStep magnetController)

                /// Gives the output current corresponding to the given monitoring shunt voltage readout for
                /// the magnet controller.
                let shuntVoltageToCurrent magnetController shuntVoltage =
                    (shuntVoltage - shuntVoltageOffset magnetController) / (linearShuntVoltageCoefficient magnetController)
                    |> min (maximumCurrent magnetController)
                    |> max 0.0M<A>

                /// Gives the current direction and current required to achieve the specified magnetic field
                /// for the magnet controller.
                let magneticFieldToCurrent magnetController magneticField =
                    let signedCurrent = (magneticField - staticField magnetController) / (linearFieldCoefficient magnetController)
                    if signedCurrent >= 0.0M<A>
                    then (Forward, abs signedCurrent)
                    else (Reverse, abs signedCurrent)

                /// Gives the magnetic field corresponding to the given current direction and current for the
                /// magnet controller.
                let currentToMagneticField magnetController (currentDirection, current) =
                    match currentDirection with
                    | Forward -> staticField magnetController + current * (linearFieldCoefficient magnetController)
                    | Reverse -> staticField magnetController - current * (linearFieldCoefficient magnetController)

                /// Gives the magnetic field corresponding to the given current direction and output step
                /// index for the magnet controller.
                let stepIndexToMagneticField magnetController (currentDirection, index) =
                    let current = stepIndexToCurrent magnetController index
                    currentToMagneticField magnetController (currentDirection, current)

                /// Gives the mangetic field corresponding to the given current direction and monitoring shunt
                /// voltage readout for the magnet controller.
                let shuntVoltageToMagneticField magnetController (currentDirection, shuntVoltage) =
                    let current = shuntVoltageToCurrent magnetController shuntVoltage
                    currentToMagneticField magnetController (currentDirection, current)

                /// Gives the monitoring shunt voltage readout corresponding to the given current for the 
                /// magnet controller.
                let currentToShuntVoltage magnetController current =
                    shuntVoltageOffset magnetController + current * (linearShuntVoltageCoefficient magnetController)

                /// Gives the monitoring shunt voltage readout corresponding to the given output step index
                /// for the magnet controller.
                let stepIndexToShuntVoltage magnetController index =
                    stepIndexToCurrent magnetController index
                    |> currentToShuntVoltage magnetController

                /// Gives the current direction and monitoring shunt voltage readout corresponding to the
                /// magnetic field for the magnet controller.
                let magneticFieldToShuntVoltage magnetController magneticField =
                    let (currentDirection, current) = magneticFieldToCurrent magnetController magneticField
                    (currentDirection, currentToShuntVoltage magnetController current)
            
                /// Gives the output step index corresponding to the specified current for the magnet
                /// controller.
                let currentToStepIndex magnetController (current : decimal<A>) =
                    if current > maximumCurrent magnetController then
                        invalidArg "current" (sprintf "Current exceeds maximum magnet controller output current: %f." current)

                    uint16 (round (decimal (current / (currentStep magnetController))))

                /// Gives the current direction and output step index corresponding to the specified magnetic
                /// field for the magnet controller.
                let magneticFieldToStepIndex magnetController magneticField =
                    let (currentDirection, current) = magneticFieldToCurrent magnetController magneticField
                    (currentDirection, currentToStepIndex magnetController current)

                /// Gives the output step index correspondign to the specified monitoring shunt voltage
                /// readout for the magnet controller.
                let shuntVoltageToStepIndex magnetController shuntVoltage =
                    let current = shuntVoltageToCurrent magnetController shuntVoltage
                    currentToStepIndex magnetController current

            /// Functions to digitise values to the discrete magnet controller values which are available.
            module Digitise =
                /// Digitses the given current to the nearest available digital output value.
                let outputCurrent settings current =
                    Convert.currentToStepIndex settings current
                    |> Convert.stepIndexToCurrent settings

                /// Digitises the given magnetic field to the nearest available digital output value.
                let magneticField settings field =
                    Convert.magneticFieldToStepIndex settings field
                    |> Convert.stepIndexToMagneticField settings

                /// Digitises the shunt voltage to that correspond to the nearest available digital output
                /// value.
                let shuntVoltage settings voltage =
                    Convert.shuntVoltageToStepIndex settings voltage
                    |> Convert.stepIndexToShuntVoltage settings

            /// Functions related to magnet controller output and ramp rate limits.
            module Limit =
                /// Returns the software-defined output current limit for the magnet controller.
                let current settings  =
                    settings.Limits.CurrentLimit
            
                /// Returns the minimum magnetic field value which can be reached by the magnet controller,
                /// when the generated magnetic field is opposing the static field and the current is at the
                /// software-defined current limit.
                let lowerField settings =
                    min <| Convert.currentToMagneticField settings (Forward, current settings)
                        <| Convert.currentToMagneticField settings (Reverse, current settings)
                
                /// Returns the maximum magnetic field value which can be reached by the magnet controller,
                /// when the generated magnetic field is alligned with the static field and the current is at
                /// the software-defined current limit.
                let upperField settings =
                    max <| Convert.currentToMagneticField settings (Forward, current settings)
                        <| Convert.currentToMagneticField settings (Reverse, current settings)
                
                /// Returns the magnetic field range which can be reached by the magnet controller within the
                /// software-defined current limit.
                let fieldRange settings = (lowerField settings, upperField settings)

                /// Returns the software-defined ramp rate limit for the magnet controller.
                let rampRate settings =
                    settings.Limits.RampRateLimit

                /// Returns the software-defined trip voltage limit for the magnet controller.
                let tripVoltage settings =
                    settings.Limits.TripVoltageLimit
                    
            module RampRate = 
                /// Lists the available calibrated ramp rate values for the magnet controller which are within
                /// the software-defined ramp rate limit, sorted in ascending order.
                let availableValues settings =
                    settings 
                    |> calibratedRampRates 
                    |> List.filter (fun rampRate -> rampRate <= Limit.rampRate settings)
                    |> List.sort

                /// Returns the largest calibrated ramp rate value for the magnet controller which is within the
                /// software-defined ramp rate limit.
                let maximum = availableValues >> List.max

                /// Returns the index of the largest calibrated ramp rate value for the magnet controller which
                /// is within the software-defined ramp rate limit.
                let maximumIndex settings = (availableValues settings |> List.length) - 1

                /// Returns the calibrated ramp rate value corresponding to the specified ramp rate index for the
                /// magnet controller.
                let fromIndex settings i = 
                    availableValues settings
                    |> List.item i

                /// Returns the nearest available ramp rate value for the magnet controller. 
                let nearest settings rampRate =
                    availableValues settings
                    |> Seq.minBy (fun rampRate' -> abs(rampRate' - rampRate))

                /// Returns the index of the nearest available ramp rate value for the magnet controller.
                let nearestIndex settings rampRate =
                    availableValues settings
                    |> Seq.findIndex ((=) (nearest settings rampRate))
                    
            /// Functions to verify values before sending them to the magnet controller hardware.
            module internal Verify =

                /// Checks whether the given output current is within the magnet controller output range and
                /// software-defined current limit.
                let outputCurrent settings current =
                    if current < 0.0M<A> then invalidArg "current" (sprintf "Magnet controller output current must be non-negative: %f." current)
                    if current > maximumCurrent settings then invalidArg "current" (sprintf "Magnet controller output current cannot exceed maximum output current: %f." current)
                    if current > Limit.current settings   then invalidArg "current" (sprintf "Magnet controller output current cannot exceed current limit: %f." current)
                    else current

                /// Checks whether the given trip voltage is within the software-defined trip voltage limit.
                let tripVoltage settings voltage =
                    if voltage < 0.0M<V>                   then invalidArg "voltage" (sprintf "Magnet controller trip voltage must be non-negative: %f." voltage)
                    if voltage > Limit.tripVoltage settings then invalidArg "voltage" (sprintf "Magnet controller trip voltage cannot exceed trip voltage limit: %f." voltage)
                    else voltage

                /// Checks whether the given ramp rate is within the software-defined ramp rate limit.
                let rampRate settings rampRate =
                    if rampRate < 0.0M<A/s>              then invalidArg "rampRate" (sprintf "Magnet controller ramp rate must be non-negative: %f." rampRate)
                    if rampRate > Limit.rampRate settings then invalidArg "rampRate" (sprintf "Magnet controller ramp rate cannot exceed ramp rate limit: %f." rampRate)
                    else rampRate

        /// Functions related to setting the magnet controller output.
        module Output =

            /// Returns the digital output resolution of the magnet controller.
            let resolution = settings >> Settings.outputResolution

            /// Returns the number of discrete digital steps in the magnet controller output.
            let digitisedStepCount = settings >> Settings.digitalOutputStepCount

            /// Asynchronously sets the output current direction of the magnet controller.
            let setDirection = IO.setCurrentDirection Keys.currentDirection

            /// Functions related to the output current of the magnet controller.
            module Current =

                /// Retruns the maximum output current of the magnet controller hardware.
                let maximum = settings >> Settings.maximumCurrent

                /// Returns the software-defined current limit for the magnet controller.
                let limit = settings >> Settings.Limit.current

                /// Returns the current difference between the digital output current steps of the magnet
                /// controller.
                let step = settings >> Settings.currentStep 

                /// Returns the nearest digitised output current value of the magnet controller.
                let digitise = settings >> Settings.Digitise.outputCurrent

                /// Returns the output current corresponding to the specified digital step index for the
                /// magnet controller.
                let fromStepIndex = settings >> Settings.Convert.stepIndexToCurrent

                /// Returns the nearest digital step index corresponding to the specified current for the
                /// magnet controller.
                let toStepIndex = settings >> Settings.Convert.currentToStepIndex

                /// Returns the current direction and output current corresponding to the specified
                /// magnetic field for the magnet controller.
                let fromMagneticField = settings >> Settings.Convert.magneticFieldToCurrent

                /// Returns the magnetic field corresponding to the specified current direction and output
                /// current for the magnet controller.
                let toMagneticField = settings >> Settings.Convert.currentToMagneticField

                /// Returns the output current corresponding to the specified monitoring shunt voltage 
                /// readout for the magnet controller.
                let fromShuntVoltage = settings >> Settings.Convert.shuntVoltageToCurrent

                /// Returns the monitoring shunt voltage readout corresponding to the specified output
                /// current for the magnet controller.
                let toShuntVoltage = settings >> Settings.Convert.currentToShuntVoltage

                /// Sets the lower current set-point for the magnet controller.
                let setLowerSetPoint magnetController = 
                    Settings.Verify.outputCurrent (settings magnetController)
                    >> IO.setCurrent Keys.lowerSetPoint magnetController 

                /// Sets the lower current set-point for the magnet controller in terms of its digital step
                /// index.
                let setLowerSetPointIndex magnetController =
                    Settings.Convert.stepIndexToCurrent (settings magnetController)
                    >> setLowerSetPoint magnetController

                /// Sets the upper current set-point for the magnet controller.
                let setUpperSetPoint magnetController =
                    Settings.Verify.outputCurrent (settings magnetController)
                    >> IO.setCurrent Keys.upperSetPoint magnetController

                /// Sets the upper current set-point for the magnet controller in terms of its digital step
                /// index.
                let setUpperSetPointIndex magnetController =
                    Settings.Convert.stepIndexToCurrent (settings magnetController)
                    >> setUpperSetPoint magnetController

            /// Functions related to the magnetic field of the magnet attached to the the magnet controller.
            module MagneticField = 

                /// Returns the magnetic field corresponding to zero output current.
                let staticField = settings >> Settings.staticField

                /// Returns the calibration constant for magnetic field per unit output current for the magnet
                /// controller.
                let linearCoefficient = settings >> Settings.linearFieldCoefficient

                /// Returns the magnetic field difference between digital output steps of the magnet controller.
                let step = settings >> Settings.fieldStep

                /// Returns the nearest digitised magnetic field value for the magnet controller.
                let digitise = settings >> Settings.Digitise.magneticField

                /// Returns the magnetic field corresponding to the specified current direction and output
                /// current for the magnet controller.
                let fromCurrent = settings >> Settings.Convert.currentToMagneticField

                /// Returns the current direction and output current corresponding to the specified
                /// magnetic field for the magnet controller.
                let toCurrent = settings >> Settings.Convert.magneticFieldToCurrent

                /// Returns the magnetic field corresponding to the specified current direction and digital
                /// output step index for the magnet controller.
                let fromStepIndex = settings >> Settings.Convert.stepIndexToMagneticField

                /// Returns the current direction and digital output step index corresponding to the specified
                /// mangetic field for the magnet controller.
                let toStepIndex = settings >> Settings.Convert.magneticFieldToStepIndex

                /// Returns the magnetic field corresponding to the specified current direction and monitoring
                /// shunt voltage readout for the magnet controller.
                let fromShuntVoltage = settings >> Settings.Convert.shuntVoltageToMagneticField

                /// Returns the current direction and monitoring shunt voltage readout corresponding to the
                /// specified magnetic field for the magnet controller.
                let toShuntVoltage = settings >> Settings.Convert.magneticFieldToShuntVoltage

                /// Returns the minimum magnetic field value which can be reached by the magnet controller,
                /// when the generated magnetic field is opposing the static field and the current is at the
                /// software-defined current limit.
                let lowerLimit magnetController =
                    min <| fromCurrent magnetController (Forward, Current.limit magnetController)
                        <| fromCurrent magnetController (Reverse, Current.limit magnetController)
                
                /// Returns the maximum magnetic field value which can be reached by the magnet controller,
                /// when the generated magnetic field is alligned with the static field and the current is at
                /// the software-defined current limit.
                let upperLimit magnetController =
                    max <| fromCurrent magnetController (Forward, Current.limit magnetController)
                        <| fromCurrent magnetController (Reverse, Current.limit magnetController)
                
                /// Returns the magnetic field range which can be reached by the magnet controller within the
                /// software-defined current limit.
                let range magnetController = (lowerLimit magnetController, upperLimit magnetController)

            /// Functions related to the monitoring shunt voltage readout of the magnet controller.
            module ShuntVoltage =

                /// Returns the voltage offset in the monitoring shunt readout at zero current.
                let offset = settings >> Settings.shuntVoltageOffset

                /// Returns the calibration constant for monitoring shunt voltage per unit output current.
                let linearCoefficient = settings >> Settings.linearShuntVoltageCoefficient

                /// Returns the RMS voltage noise level in the monitoring shunt readout.
                let rmsNoise = settings >> Settings.shuntVoltageRmsNoise

                /// Returns the shunt voltage readout at maximum output current for the magnet controller.
                let maximum = settings >> Settings.maximumShuntVoltage

                /// Returns the monitoring shunt voltage difference between digital output steps of the
                /// magnet controller.
                let step = settings >> Settings.shuntVoltageStep

                /// Returns the nearest digitised monitoring shunt voltage readout.
                let digitise = settings >> Settings.Digitise.shuntVoltage

                /// Returns the monitoring shunt voltage readout corresponding to the specified output
                /// current for the magnet controller.
                let fromCurrent = settings >> Settings.Convert.currentToShuntVoltage

                /// Returns the output current corresponding to the specified monitoring shunt voltage 
                /// readout for the magnet controller.
                let toCurrent = settings >> Settings.Convert.shuntVoltageToCurrent

                /// Returns the monitoring shunt voltage readout corresponding to the specified digital
                /// output step index for the magnet controller.
                let fromStepIndex = settings >> Settings.Convert.stepIndexToShuntVoltage

                /// Returns the digitial output step index corresponding to the specified monitoring
                /// shunt voltage readout for the magnet controller.
                let toStepIndex = settings >> Settings.Convert.shuntVoltageToStepIndex

                /// Returns the current direction and monitoring shunt voltage readout corresponding to the
                /// specified magnetic field for the magnet controller.
                let fromMagneticField = settings >> Settings.Convert.magneticFieldToShuntVoltage

                /// Returns the magnetic field corresponding to the specified current direction and monitoring
                /// shunt voltage readout for the magnet controller.
                let toMagneticField = settings >> Settings.Convert.shuntVoltageToMagneticField

            /// Functions relate to the back-EMF trip voltage for the magnet controller.
            module TripVoltage =

                /// Returns the software-defined back-EMF trip voltage limit for the magnet controller.
                let limit = settings >> Settings.Limit.tripVoltage

                /// Asynchronously sets the back-EMF trip voltage for the magnet controller.
                let set = IO.setVoltage Keys.tripVoltage

        /// Functions related to controlling output ramping for the magnet controller.
        module Ramp =
            
            /// Asynchronously enables or disables the ramp pause for the magnet controller.
            let setPause = IO.setBooleanState Keys.pause

            /// Asynchronously sets the magnet controller ramp target.
            let setTarget = IO.setRampTarget Keys.rampTarget

            /// Asynchronously waits to reach the magnet controller ramp target.
            let rec waitToReachTarget magnetController = async {
                let! currentParams = queryCurrentParameters magnetController
                if not currentParams.ReachedTarget then
                    do! waitToReachTarget magnetController }

            /// Asynchronously waits for the magnet controller output current readout to reach zero, which is
            /// required before current polarity can be changed.
            let rec waitToReachZero magnetController = async {
                let! outputParams = queryOutputParameters magnetController
                if outputParams.OutputCurrent <> 0.0M<A> then 
                    do! waitToReachZero magnetController }

            /// Functions related to magnet controller ramp rate.
            module Rate = 
                
                /// Lists the available calibrated ramp rate values for the magnet controller which are within
                /// the software-defined ramp rate limit, sorted in ascending order.
                let availableValues = settings >> Settings.RampRate.availableValues

                /// Returns the largest calibrated ramp rate value for the magnet controller which is within the
                /// software-defined ramp rate limit.
                let maximum = settings >> Settings.RampRate.maximum

                /// Returns the index of the largest calibrated ramp rate value for the magnet controller which
                /// is within the software-defined ramp rate limit.
                let maximumIndex = settings >> Settings.RampRate.maximumIndex

                /// Returns the calibrated ramp rate value corresponding to the specified ramp rate index for the
                /// magnet controller.
                let fromIndex = settings >> Settings.RampRate.fromIndex 

                /// Returns the nearest available ramp rate value for the magnet controller. 
                let nearest = settings >> Settings.RampRate.nearest

                /// Returns the index of the nearest available ramp rate value for the magnet controller.
                let nearestIndex = settings >> Settings.RampRate.nearestIndex

                /// Asynchronously sets the magnet controller ramp rate.
                let set = IO.setRampRate Keys.rampRate

                /// Asynchronously sets the magnet controller ramp rate by index.
                let setByIndex magnetController i =
                    fromIndex magnetController i
                    |> set magnetController

                /// Asynchronously sets the magnet controller ramp rate to the largest available value.
                let setToMaximum magnetController =
                    maximum magnetController
                    |> set magnetController