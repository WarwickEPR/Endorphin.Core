namespace Endorphin.Instrument.TwickenhamSmc

module TwickenhamSmcFSharp = 
    let maximumRampRateInAmpsPerSec (magnetController : MagnetController) =
        magnetController.RampRateLimitInAmpsPerSec

    let maximumTripVoltageInVolts (magnetController : MagnetController) =
        magnetController.TripVoltageLimitInVolts
    
    let outputParameters (magnetController : MagnetController) =
        GetOutputParameters
        |> magnetController.PostAndReply
        
    let outputParametersAsync (magnetController : MagnetController) =
        GetOutputParameters
        |> magnetController.PostAndAsyncReply

    let currentParameters (magnetController : MagnetController) =
        GetCurrentParameters
        |> magnetController.PostAndReply

    let currentParametersAsync (magnetController : MagnetController) =
        GetCurrentParameters
        |> magnetController.PostAndAsyncReply

    let operatingParameters (magnetController : MagnetController) =
        GetOperatingParameters
        |> magnetController.PostAndReply

    let operatingParametersAsync (magnetController : MagnetController) =
        GetOperatingParameters
        |> magnetController.PostAndAsyncReply

    let setPointParameters (magnetController : MagnetController) =
        GetSetPointParameters
        |> magnetController.PostAndReply

    let setPointParametersAsync (magnetController : MagnetController) =
        GetSetPointParameters
        |> magnetController.PostAndAsyncReply
        
    let setRampRateInAmpsPerSec rampRateInAmpsPerSec (magnetController : MagnetController) =
        magnetController.SetRampRate(rampRateInAmpsPerSec)

    let setTripVoltage tripVoltageInVolts (magnetController : MagnetController) =
        magnetController.SetTripVoltage(tripVoltageInVolts)

    let setCurrentDirection currentDirection (magnetController : MagnetController) =
        magnetController.SetCurrentDirection(currentDirection)

    let setLowerSetPoint lowerCurrentLimitInAmps (magnetController : MagnetController) =
        magnetController.SetLowerSetPoint(lowerCurrentLimitInAmps)

    let setUpperSetPoint upperCurrentLimitInAmps (magnetController : MagnetController) =
        magnetController.SetUpperSetPoint(upperCurrentLimitInAmps)

    let setRampTarget rampRateInAmpsPerSec (magnetController : MagnetController) =
        magnetController.SetRampTarget(rampRateInAmpsPerSec)

    let setPause pause (magnetController : MagnetController) =
        magnetController.SetPause(pause)

    let waitToReachTarget (magnetController : MagnetController) =
        magnetController.waitToReachTargetAsync()

    let prepareToChangeCurrentDirection (magnetController : MagnetController) =
        magnetController.prepareToChangeCurrentDirectionAsync()
        
    let rampToZero (magnetController : MagnetController) =
        magnetController.rampToZeroAsync()
        
    let rampToZeroAndSetCurrentDirection(currentDirection) (magnetController : MagnetController) =
        magnetController.rampToZeroAndSetCurrentDirectionAsync(currentDirection)

    let magnetControllerState (magnetController : MagnetController) =
        magnetController.getStateAsync()

    let availableRampRatesInAmpsPerSec (magnetController : MagnetController) =
        magnetController.AvailableRampRatesInAmpsPerSec

    let rampRateForIndexInAmpsPerSec (index) (magnetController : MagnetController) =
        magnetController.RampRateForIndexInAmpsPerSec(index)

    let availableRampRatesInMilliteslaPerSec (magnetController : MagnetController) =
        magnetController.AvailableRampRatesInMilliteslaPerSec

    let rampRateForIndexInMilliteslaPerSec (index) (magnetController : MagnetController) =
        magnetController.RampRateForIndexInMilliteslaPerSec(index)

    let numberOfCurrentSteps (magnetController : MagnetController) =
        magnetController.NumberOfCurrentSteps

    let currentStepInAmps (magnetController : MagnetController) =
        magnetController.CurrentStepInAmps

    let currentForIndexInAmps (index) (magnetController : MagnetController) =
        magnetController.CurrentForIndexInAmps(index)

    let fieldStepInMillitesla (magnetController : MagnetController) =
        magnetController.FieldStepInMillitesla

    let fieldForIndexInMillitesla (index) (magnetController : MagnetController) =
        magnetController.FieldForIndexInMillitesla(index)

    let maximumFieldInMillitesla (magnetController : MagnetController) =
        magnetController.MaximumFieldInMillitesla

    let minimumFieldInMillitesla (magnetController : MagnetController) =
        magnetController.MinimumFieldInMillitesla

    let nearestDigitisedCurrentInAmps (currentInAmps) (magnetController : MagnetController) =
        magnetController.NearestDigitisedCurrentInAmps(currentInAmps)
    
    let nearestDigitisedFieldInMillitesla (fieldInMillitesla) (magnetController : MagnetController) =
        magnetController.NearestDigitisedFieldInMillitesla(fieldInMillitesla)

    let nearestDigitisedRampRateInAmpsPerSec (rampRateInAmpsPerSec) (magnetController : MagnetController) =
        magnetController.NearestDigitisedRampRateInAmpsPerSec(rampRateInAmpsPerSec)

    let nearestDigitisedRampRateInMilliteslaPerSec (rampRateInMilliteslaPerSec) (magnetController : MagnetController) =
        magnetController.NearestDigitisedRampRateInMilliteslaPerSec(rampRateInMilliteslaPerSec)

    let shuntStepInvVolts (magnetController : MagnetController) =
        magnetController.ShuntStepInvVolts

    let nearestDigitisedOutputIndexForShuntVoltage voltageInVolts (magnetController : MagnetController) =
        magnetController.NearestDigitisedOutputIndexForShuntVoltage(voltageInVolts)
    
    let nearestDigitisedCurrentInAmpsForShuntVoltage voltageInVolts (magnetController : MagnetController) =
        magnetController.NearestDigitisedCurrentInAmpsForShuntVoltage(voltageInVolts)

    let nearestDigitisedFieldInMilliteslaForShuntVoltage(voltageInVolts) (magnetController : MagnetController) =
        magnetController.NearestDigitisedFieldInMilliteslaForShuntVoltage(voltageInVolts)