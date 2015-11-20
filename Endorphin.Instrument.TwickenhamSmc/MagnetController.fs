namespace Endorphin.Instrument.TwickenhamSmc

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core

[<AutoOpen>]
/// Core instrument API functions.
module Instrument = 

    [<RequireQualifiedAccess>]
    /// Functions posting comands and performing queries to a Twickenham Scientific superconducting
    /// magnet controller.
    module MagnetController =

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
        module private Settings =
        
            /// Returns the output resolution of the magnet controller in bits.
            let outputResolution (MagnetController _) = 16<bit>

            /// Returns the number of digital steps for the magnet controller output.
            let digitalOutputStepCount (MagnetController _) = System.UInt16.MaxValue

            /// Returns the maximum output current of the magnet controller.
            let maximumCurrent (MagnetController (_, settings)) =
                settings.HardwareParameters.MaximumCurrent

            /// Returns the digital current step of the magnet controller.
            let currentStep magnetController = 
                (maximumCurrent magnetController)
                / (1.0 * float (digitalOutputStepCount magnetController))

            /// Returns the static field of the magnet controlled by the magnet controller.
            let staticField (MagnetController (_, settings)) =
                settings.FieldCalibration.StaticField

            /// Returns the linear proportionality coefficient between current and magnetic field for
            /// the magnet controller.
            let linearFieldCoefficient (MagnetController (_, settings)) =
                settings.FieldCalibration.LinearCoefficient

            /// Returns the digital magnetic field step of the magnet controller.
            let fieldStep magnetController =
                (currentStep magnetController) * (linearFieldCoefficient magnetController) 

            /// Returns the output monitoring shunt voltage offset of the magnet controller in volts.
            let shuntVoltageOffset (MagnetController (_, settings)) =
                settings.ShuntCalibration.VoltageOffset

            /// Returns the linear proportionality coefficient between current and output monitoring 
            /// shunt voltage for the magnet controller.
            let linearShuntVoltageCoefficient (MagnetController (_, settings)) =
                settings.ShuntCalibration.LinearCoefficient

            /// Returns the RMS voltage noise for the output monitoring shunt of the magnet controller.
            let shuntVoltageRmsNoise (MagnetController (_, settings)) =
                settings.ShuntCalibration.RmsVoltageNoise

            /// Returns the output monitoring shunt voltage for the magnet controller at maximum output
            /// current.
            let maximumShuntVoltage magnetController =
                shuntVoltageOffset magnetController + (maximumCurrent magnetController) * (linearShuntVoltageCoefficient magnetController)

            /// Returns the output monitoring shunt voltage step size between digital output steps of 
            /// the magnet controller.
            let shuntVoltageStep magnetController =
                (currentStep magnetController) * (linearShuntVoltageCoefficient magnetController)

            /// Returns the list of calibrated ramp rates available on the magnet controller.
            let calibratedRampRates (MagnetController (_, settings)) =
                settings.HardwareParameters.CalibratedRampRates

            /// Returns the software-defined output current limit for the magnet controller.
            let currentLimit (MagnetController (_, settings)) =
                settings.Limits.CurrentLimit

            /// Returns the software-defined ramp rate limit for the magnet controller.
            let rampRateLimit (MagnetController (_, settings)) =
                settings.Limits.RampRateLimit

            /// Returns the software-defined trip voltage limit for the magnet controller.
            let tripVoltageLimit (MagnetController (_, settings)) =
                settings.Limits.TripVoltageLimit

        /// Functions related to unit conversions.
        module private Convert =

            /// Gives the current corresponding to the specified digital step indeex for the magnet
            /// controller.
            let stepIndexToCurrent magnetController (index : uint16) =
                (1.0 * float index) * (Settings.currentStep magnetController)

            /// Gives the output current corresponding to the given monitoring shunt voltage readout for
            /// the magnet controller.
            let shuntVoltageToCurrent magnetController shuntVoltage =
                (shuntVoltage - Settings.shuntVoltageOffset magnetController) / (Settings.linearShuntVoltageCoefficient magnetController)
                |> min (Settings.maximumCurrent magnetController)
                |> max 0.0<A>

            /// Gives the current direction and current required to achieve the specified magnetic field
            /// for the magnet controller.
            let magneticFieldToCurrent magnetController magneticField =
                let signedCurrent = (magneticField - Settings.staticField magnetController) / (Settings.linearFieldCoefficient magnetController)
                if signedCurrent >= 0.0<A>
                then (Forward, abs signedCurrent)
                else (Reverse, abs signedCurrent)

            /// Gives the magnetic field corresponding to the given current direction and current for the
            /// magnet controller.
            let currentToMagneticField magnetController (currentDirection, current) =
                match currentDirection with
                | Forward -> Settings.staticField magnetController + current * (Settings.linearFieldCoefficient magnetController)
                | Reverse -> Settings.staticField magnetController - current * (Settings.linearFieldCoefficient magnetController)

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
                Settings.shuntVoltageOffset magnetController + current * (Settings.linearShuntVoltageCoefficient magnetController)

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
            let currentToStepIndex magnetController (current : float<A>) =
                if current > Settings.maximumCurrent magnetController then
                    invalidArg "current" "Current exceeds maximum magnet controller output current." current

                uint16 (round(float (current / (Settings.currentStep magnetController))))

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

            /// Digitses the given current to the nearest available digital output value.
            let nearestDigitisedCurrent magnetController current =
                currentToStepIndex magnetController current
                |> stepIndexToCurrent magnetController

            /// Digitises the given magnetic field to the nearest available digital output value.
            let nearestDigitisedMagneticField magnetController magneticField =
                magneticFieldToStepIndex magnetController magneticField
                |> stepIndexToMagneticField magnetController

            /// Digitises the shunt voltage to that correspond to the nearest available digital output
            /// value.
            let nearestDigitisedShuntVoltage magnetController shuntVoltage =
                shuntVoltageToStepIndex magnetController shuntVoltage
                |> stepIndexToShuntVoltage magnetController

        /// Functions to verify values before sending them to the magnet controller hardware.
        module private Verify =

            /// Checks whether the given output current is within the magnet controller output range and
            /// software-defined current limit.
            let outputCurrent magnetController current =
                if current < 0.0<A> then invalidArg "current" "Magnet controller output current must be non-negative." current
                if current > Settings.maximumCurrent magnetController then invalidArg "current" "Magnet controller output current cannot exceed maximum output current." current
                if current > Settings.currentLimit magnetController   then invalidArg "current" "Magnet controller output current cannot exceed current limit." current
                else current

            /// Checks whether the given trip voltage is within the software-defined trip voltage limit.
            let tripVoltage magnetController voltage =
                if voltage < 0.0<V> then invalidArg "voltage" "Magnet controller trip voltage must be non-negative." voltage
                if voltage > Settings.tripVoltageLimit magnetController then invalidArg "voltage" "Magnet controller trip voltage cannot exceed trip voltage limit." voltage
                else voltage

            /// Checks whether the given ramp rate is within the software-defined ramp rate limit.
            let rampRate magnetController rampRate =
                if rampRate < 0.0<A/s> then invalidArg "rampRate" "Magnet controller ramp rate must be non-negative." rampRate
                if rampRate > Settings.rampRateLimit magnetController then invalidArg "rampRate" "Magnet controller ramp rate cannot exceed ramp rate limit." rampRate
                else rampRate

        /// Functions related to setting the magnet controller output.
        module Output =

            /// Returns the digital output resolution of the magnet controller.
            let resolution = Settings.outputResolution

            /// Returns the number of discrete digital steps in the magnet controller output.
            let digitisedStepCount = Settings.digitalOutputStepCount

            /// Asynchronously sets the output current direction of the magnet controller.
            let setDirection = IO.setCurrentDirection Keys.currentDirection

            /// Functions related to the output current of the magnet controller.
            module Current =

                /// Retruns the maximum output current of the magnet controller hardware.
                let maximum = Settings.maximumCurrent

                /// Returns the software-defined current limit for the magnet controller.
                let limit = Settings.currentLimit

                /// Returns the current difference between the digital output current steps of the magnet
                /// controller.
                let step = Settings.currentStep 

                /// Returns the nearest digitised output current value of the magnet controller.
                let digitise = Convert.nearestDigitisedCurrent

                /// Returns the output current corresponding to the specified digital step index for the
                /// magnet controller.
                let fromStepIndex = Convert.stepIndexToCurrent

                /// Returns the nearest digital step index corresponding to the specified current for the
                /// magnet controller.
                let toStepIndex = Convert.currentToStepIndex

                /// Returns the current direction and output current corresponding to the specified
                /// magnetic field for the magnet controller.
                let fromMagneticField = Convert.magneticFieldToCurrent

                /// Returns the magnetic field corresponding to the specified current direction and output
                /// current for the magnet controller.
                let toMagneticField = Convert.currentToMagneticField

                /// Returns the output current corresponding to the specified monitoring shunt voltage 
                /// readout for the magnet controller.
                let fromShuntVoltage = Convert.shuntVoltageToCurrent

                /// Returns the monitoring shunt voltage readout corresponding to the specified output
                /// current for the magnet controller.
                let toShuntVoltage = Convert.currentToShuntVoltage

                /// Sets the lower current set-point for the magnet controller.
                let setLowerSetPoint magnetController = 
                    Verify.outputCurrent magnetController
                    >> IO.setCurrent Keys.lowerSetPoint magnetController 

                /// Sets the lower current set-point for the magnet controller in terms of its digital step
                /// index.
                let setLowerSetPointIndex magnetController index =
                    fromStepIndex magnetController index
                    |> setLowerSetPoint magnetController

                /// Sets the upper current set-point for the magnet controller.
                let setUpperSetPoint magnetController =
                    Verify.outputCurrent magnetController
                    >> IO.setCurrent Keys.upperSetPoint magnetController

                /// Sets the upper current set-point for the magnet controller in terms of its digital step
                /// index.
                let setUpperSetPointIndex magnetController index =
                    fromStepIndex magnetController index
                    |> setUpperSetPoint magnetController

            /// Functions related to the magnetic field of the magnet attached to the the magnet controller.
            module MagneticField = 

                /// Returns the magnetic field corresponding to zero output current.
                let staticField = Settings.staticField

                /// Returns the calibration constant for magnetic field per unit output current for the magnet
                /// controller.
                let linearCoefficient = Settings.linearFieldCoefficient

                /// Returns the magnetic field difference between digital output steps of the magnet controller.
                let step = Settings.fieldStep

                /// Returns the nearest digitised magnetic field value for the magnet controller.
                let digitise = Convert.nearestDigitisedMagneticField

                /// Returns the magnetic field corresponding to the specified current direction and output
                /// current for the magnet controller.
                let fromCurrent = Convert.currentToMagneticField

                /// Returns the current direction and output current corresponding to the specified
                /// magnetic field for the magnet controller.
                let toCurrent = Convert.magneticFieldToCurrent

                /// Returns the magnetic field corresponding to the specified current direction and digital
                /// output step index for the magnet controller.
                let fromStepIndex = Convert.stepIndexToMagneticField

                /// Returns the current direction and digital output step index corresponding to the specified
                /// mangetic field for the magnet controller.
                let toStepIndex = Convert.magneticFieldToStepIndex

                /// Returns the magnetic field corresponding to the specified current direction and monitoring
                /// shunt voltage readout for the magnet controller.
                let fromShuntVoltage = Convert.shuntVoltageToMagneticField

                /// Returns the current direction and monitoring shunt voltage readout corresponding to the
                /// specified magnetic field for the magnet controller.
                let toShuntVoltage = Convert.magneticFieldToShuntVoltage

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
                let offset = Settings.shuntVoltageOffset

                /// Returns the calibration constant for monitoring shunt voltage per unit output current.
                let linearCoefficient = Settings.linearShuntVoltageCoefficient

                /// Returns the RMS voltage noise level in the monitoring shunt readout.
                let rmsNoise = Settings.shuntVoltageRmsNoise

                /// Returns the shunt voltage readout at maximum output current for the magnet controller.
                let maximum = Settings.maximumShuntVoltage

                /// Returns the monitoring shunt voltage difference between digital output steps of the
                /// magnet controller.
                let step = Settings.shuntVoltageStep

                /// Returns the nearest digitised monitoring shunt voltage readout.
                let digitise = Convert.nearestDigitisedShuntVoltage

                /// Returns the monitoring shunt voltage readout corresponding to the specified output
                /// current for the magnet controller.
                let fromCurrent = Convert.currentToShuntVoltage

                /// Returns the output current corresponding to the specified monitoring shunt voltage 
                /// readout for the magnet controller.
                let toCurrent = Convert.shuntVoltageToCurrent

                /// Returns the monitoring shunt voltage readout corresponding to the specified digital
                /// output step index for the magnet controller.
                let fromStepIndex = Convert.stepIndexToShuntVoltage

                /// Returns the digitial output step index corresponding to the specified monitoring
                /// shunt voltage readout for the magnet controller.
                let toStepIndex = Convert.shuntVoltageToStepIndex

                /// Returns the current direction and monitoring shunt voltage readout corresponding to the
                /// specified magnetic field for the magnet controller.
                let fromMagneticField = Convert.magneticFieldToShuntVoltage

                /// Returns the magnetic field corresponding to the specified current direction and monitoring
                /// shunt voltage readout for the magnet controller.
                let toMagneticField = Convert.shuntVoltageToMagneticField

            /// Functions relate to the back-EMF trip voltage for the magnet controller.
            module TripVoltage =

                /// Returns the software-defined back-EMF trip voltage limit for the magnet controller.
                let limit = Settings.tripVoltageLimit

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
                if outputParams.OutputCurrent <> 0.0<A> then 
                    do! waitToReachZero magnetController }

            /// Functions related to magnet controller ramp rate.
            module Rate = 
                
                /// Lists the available calibrated ramp rate values for the magnet controller which are within
                /// the software-defined ramp rate limit, sorted in ascending order.
                let availableValues magnetController =
                    Settings.calibratedRampRates magnetController
                    |> List.filter (fun rampRate -> rampRate <= Settings.rampRateLimit magnetController)
                    |> List.sort

                /// Returns the largest calibrated ramp rate value for the magnet controller which is within the
                /// software-defined ramp rate limit.
                let maximum = availableValues >> List.max

                /// Returns the index of the largest calibrated ramp rate value for the magnet controller which
                /// is within the software-defined ramp rate limit.
                let maximumIndex magnetController = (availableValues magnetController |> List.length) - 1

                /// Returns the calibrated ramp rate value corresponding to the specified ramp rate index for the
                /// magnet controller.
                let fromIndex magnetController i = 
                    availableValues magnetController 
                    |> List.item i

                /// Returns the nearest available ramp rate value for the magnet controller. 
                let nearest magnetController rampRate =
                    availableValues magnetController
                    |> Seq.minBy (fun rampRate' -> abs(rampRate' - rampRate))

                /// Returns the index of the nearest available ramp rate value for the magnet controller.
                let nearestIndex magnetController rampRate =
                    availableValues magnetController
                    |> Seq.findIndex ((=) (nearest magnetController rampRate))

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