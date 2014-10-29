﻿namespace Endorphin.Instrument.TwickenhamSmc

module TwickenhamSmcFSharp = 
    let maximumRampRateInAmpsPerSec (magnetController : MagnetController) =
        magnetController.MaximumRampRateInAmpsPerSec

    let maximumTripVoltageInVolts (magnetController : MagnetController) =
        magnetController.MaximumTripVoltageInVolts
    
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
