// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Model.SignalGenerator

[<RequireQualifiedAccess>]
/// Functions for specifying trigger settings.
module SignalGenerator =

    /// Specifies a fixed frequency
    let fixedFrequency = FixedFrequency

    /// Specifies a frequency sweep with the given start, stop, increment and dwell time at each step.
    /// The frequency is swept in the specified direciton.
    let frequencySweep start stop increment dwell direction =
        { StartFrequency     = start
          StopFrequency      = stop
          FrequencyIncrement = increment
          DwellTime          = dwell
          SweepDirection     = direction }
        |> FrequencySweep

    /// Functions for specifying waveforms.
    module Waveform =
        
        /// Creates a sine wave.
        let sine peakToPeak offset frequency = Sine(peakToPeak, offset, frequency)
        
        /// Creates a square wave.
        let square peakToPeak offset frequency = Square(peakToPeak, offset, frequency)
        
        /// Creates a triangle wave.
        let triangle peakToPeak offset frequency = Triangle(peakToPeak, offset, frequency)
        
        /// Creates a constant DC voltage.
        let dcVoltage = DCVoltage

        /// Creates an upward ramp. The centre of the ramp is at the specified offset.
        let rampUp peakToPeak offset frequency = RampUp(peakToPeak, offset, frequency)

        /// Creates a downward ramp. The centre of the ramp is at the specified offset.
        let rampDown peakToPeak offset frequency = RampDown(peakToPeak, offset, frequency)

        /// Creates a sinc (= sin(x) / x) function
        let sinc peakToPeak offset frequency = Sinc(peakToPeak, offset, frequency)

        /// Creates a Gaussian function.
        let gaussian peakToPeak offset frequency = Gaussian(peakToPeak, offset, frequency)

        /// Creates a rectified sine wave.
        let halfSine peakToPeak offset frequency = HalfSine(peakToPeak, offset, frequency)

        /// Creates white noise.
        let whiteNoise peakToPeak offset = WhiteNoise(peakToPeak, offset)

        /// Creates a pseudo-random bit stream.
        let pseudoRandomBitStream peakToPeak offset bitRate = PseudoRandomBitStream(peakToPeak, offset, bitRate)

    /// Functions for specifying the function generator trigger source.
    module Trigger =

        /// Sets the signal generator to trigger immediately.
        let auto = AutoTrigger

        /// Sets the signal generator to share the oscilloscope trigger.
        let scope triggerType = SignalGeneratorTrigger(ScopeTrigger, triggerType)

        /// Sets the signal generator to be triggered in software.
        let software triggerType = SignalGeneratorTrigger(SoftwareTrigger, triggerType)

        /// Sets the signal generator to be triggered by the External input.
        let external triggerType threshold = SignalGeneratorTrigger(ExternalTrigger threshold, triggerType)

        /// Specifies that the signal generator should trigger on a rising edge.
        let rising = Rising

        /// Specifies that the signal generator should trigger on a falling edge.
        let falling = Falling

        /// Specifies that the signal generator should run while the gate signal is high.
        let gateHigh = GateHigh

        /// Specifies that the signal generator should run while the gate signal is low.
        let gateLow = GateLow

    /// Functions for specifying the number of waveform repetitions.
    module Playback =
        
        /// Sets the device to play the waveform indefinitely.
        let continuous = ContinuousPlayback

        /// Sets the device to perform the specified number of waveform cycles.
        let numberOfCycles = NumberOfCycles

        /// Sets the device to perform the specified number of frequency sweeps.
        let numberOfSweeps = NumberOfSweeps

    /// Creates a built-in waveform with the specified parameters and trigger settings
    let createBuiltInWaveform waveform playback triggerSettings =
        { Waveform        = waveform
          PlaybackMode    = playback
          TriggerSettings = triggerSettings }