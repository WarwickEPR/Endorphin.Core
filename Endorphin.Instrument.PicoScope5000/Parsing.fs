// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open NativeModel
open StatusCodes

/// Contains functions related to parsing between native model and high level model domain
/// types.
module internal Parsing =

    /// Parses a 16-bit integer into a toggle state indicating whether a setting is enabled
    /// or disabled.
    let parseToggleState value = if value <> 0s then Enabled else Disabled
    
    [<AutoOpen>]
    /// Parsing functions related to status codes.
    module StatusCodes =

        /// Matches StatusCode.Ok as Ok and all other status codes as errors. Note that this
        /// is not always the desired behaviour as some status codes indicate different device
        /// states rather than failure.
        let (|Ok|HasError|) = function
            | StatusCode.Ok -> Ok
            | error         -> HasError (statusMessage error)

        /// Parses the provided status as an availability status, matching StatusCode.Ok as
        /// Available and StatusCode.Busy as Busy. Doesn't match any other status codes.
        let (|AvailabilityStatus|_|) = function
            | StatusCode.Ok   -> Some (AvailabilityStatus Available)
            | StatusCode.Busy -> Some (AvailabilityStatus Busy)
            | _               -> None

        /// Parses the provided status as a found/not found status, match StatusCode.Ok as
        /// (Found true) and StatusCode.NotFound as (Found false). 
        let (|Found|_|) = function
            | StatusCode.Ok       -> Some (Found true)
            | StatusCode.NotFound -> Some (Found false)
            | _                   -> None
        
        /// Parses the provided status as a power source, matching
        /// StatusCode.PowerSupplyConnected and StatusCode.PowerSupplyNotConnected accordingly.
        /// Doesn't match any other status codes.
        let (|PowerSourceStatus|_|) = function
            | StatusCode.PowerSupplyConnected    -> Some MainsPower
            | StatusCode.PowerSupplyNotConnected -> Some UsbPower
            | _                                  -> None

        /// Returns the status code corresponding to a provided power source, which can be used
        /// to switch the power source of the PicoScope hardware.
        let powerSourceStatusCode = function
            | MainsPower -> StatusCode.PowerSupplyConnected
            | UsbPower   -> StatusCode.PowerSupplyNotConnected

    [<AutoOpen>]
    /// Parsing functions related to the device indicators and information.
    module Device =
        // TODO: model device info types and add device parsing functions

        /// Converts an LedFlash setting to a 16-bit integer which can be used to set the LED
        /// flash state on the device.
        let ledFlashCounts = function
            | LedOff              -> 0s
            | LedRepeat n         -> n
            | LedIndefiniteRepeat -> -1s 

    /// Parses the provided integer interval with and enumeration indicating the time unit into
    /// an interval with unit of measure.
    let parseIntervalWithInterval (interval, unit) =
        match unit with
        | TimeUnitEnum.Femtoseconds -> Interval_fs (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Picoseconds  -> Interval_ps  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Nanoseconds  -> Interval_ns  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Microseconds -> Interval_us (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Milliseconds -> Interval_ms (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Seconds      -> Interval_s      (LanguagePrimitives.Int32WithMeasure interval)
        | enum                      -> failwithf "Unexpected time unit enum value: %A" enum

    /// Converts the provided interval with unit of measure to an integer interval with an
    /// enumeration indicating the time unit.
    let intervalAndTimeUnitEnum = function
        | Interval_fs interval -> (int interval, TimeUnitEnum.Femtoseconds)
        | Interval_ps interval -> (int interval, TimeUnitEnum.Picoseconds )
        | Interval_ns interval -> (int interval, TimeUnitEnum.Nanoseconds )
        | Interval_us interval -> (int interval, TimeUnitEnum.Microseconds)
        | Interval_ms interval -> (int interval, TimeUnitEnum.Milliseconds)
        | Interval_s  interval -> (int interval, TimeUnitEnum.Seconds     )
    
    /// Converts an AutoTriggerDelay option to a corresponding 16-bit integer indicating either that
    /// there is no auto-trigger or the auto-trigger delay value in milliseconds.
    let autoTriggerDelayIntInMilliseconds = function
        | Some (AutoTriggerDelay_ms delay) -> int16 delay
        | None                             -> 0s

    [<AutoOpen>]
    /// Parsing functions related to channel settings.
    module ChannelSettings =
        
        /// Parses an enumeration as an input channel. Note that this does not parse all possible
        /// values of ChannelEnum as not all of these indicate an input channel.
        let parseInputChannel = function
            | ChannelEnum.A -> ChannelA
            | ChannelEnum.B -> ChannelB
            | ChannelEnum.C -> ChannelC
            | ChannelEnum.D -> ChannelD
            | enum          -> failwithf "Unexpected input channel enum value: %A." enum

        /// Converts the provided input channel into an enumeration.
        let inputChannelEnum = function
            | ChannelA -> ChannelEnum.A
            | ChannelB -> ChannelEnum.B
            | ChannelC -> ChannelEnum.C
            | ChannelD -> ChannelEnum.D
        
        /// Parses a enumeration into a coupling.
        let parseCoupling = function
            | CouplingEnum.AC -> AC
            | CouplingEnum.DC -> DC
            | enum            -> failwithf "Unexpected coupling enum value: %A." enum

        /// Converts the provided coupling into an enumeration.
        let couplingEnum = function
            | AC -> CouplingEnum.AC
            | DC -> CouplingEnum.DC

        /// Parses the provided enumeration into a voltage range.
        let parseRange = function
            | RangeEnum._10mV  -> Range_10mV
            | RangeEnum._20mV  -> Range_20mV
            | RangeEnum._50mV  -> Range_50mV
            | RangeEnum._100mV -> Range_100mV
            | RangeEnum._200mV -> Range_200mV
            | RangeEnum._500mV -> Range_500mV
            | RangeEnum._1V    -> Range_1V
            | RangeEnum._2V    -> Range_2V
            | RangeEnum._5V    -> Range_5V
            | RangeEnum._10V   -> Range_10V
            | RangeEnum._20V   -> Range_20V
            | RangeEnum._50V   -> Range_50V
            | enum             -> failwithf "Unexpected range enum value: %A." enum

        /// Converts the provided voltage range into an enumeration.
        let rangeEnum = function
            | Range_10mV  -> RangeEnum._10mV 
            | Range_20mV  -> RangeEnum._20mV 
            | Range_50mV  -> RangeEnum._50mV 
            | Range_100mV -> RangeEnum._100mV
            | Range_200mV -> RangeEnum._200mV
            | Range_500mV -> RangeEnum._500mV
            | Range_1V    -> RangeEnum._1V   
            | Range_2V    -> RangeEnum._2V   
            | Range_5V    -> RangeEnum._5V   
            | Range_10V   -> RangeEnum._10V  
            | Range_20V   -> RangeEnum._20V  
            | Range_50V   -> RangeEnum._50V

        /// Parses the provided enumeration into a bandwidth limit.
        let parseBandwidthLimit = function
            | BandwidthLimitEnum.Full   -> FullBandwidth
            | BandwidthLimitEnum._20MHz -> Bandwidth_20MHz
            | enum                      -> failwithf "Unexpected bandwidth limit enum value: %A." enum

        /// Converts the provided bandwidth limit into an enumeration.
        let bandwidthLimitEnum = function
            | FullBandwidth   -> BandwidthLimitEnum.Full
            | Bandwidth_20MHz -> BandwidthLimitEnum._20MHz 

    [<AutoOpen>]
    /// Parsing functions related to triggering.
    module Triggering =

        /// Converts the provided level threshold direction into an enumeration.
        let internal levelThresholdEnum = function
            | Above           -> ThresholdDirectionEnum.Above          
            | Below           -> ThresholdDirectionEnum.Below          
            | Rising          -> ThresholdDirectionEnum.Rising         
            | Falling         -> ThresholdDirectionEnum.Falling        
            | RisingOrFalling -> ThresholdDirectionEnum.RisingOrFalling
        
        /// Converts the provided window threshold direction into an enumeration.
        let internal windowThresholdEnum = function
            | Inside      -> ThresholdDirectionEnum.Inside     
            | Outside     -> ThresholdDirectionEnum.Outside    
            | Enter       -> ThresholdDirectionEnum.Enter      
            | Exit        -> ThresholdDirectionEnum.Exit       
            | EnterOrExit -> ThresholdDirectionEnum.EnterOrExit

        /// Parses the provided enumeration into a trigger channel (which can be an input
        /// channel, or the external or auxiliary channel).
        let parseTriggerChannel = function
            | ChannelEnum.A   -> InputChannelTrigger ChannelA
            | ChannelEnum.B   -> InputChannelTrigger ChannelB
            | ChannelEnum.C   -> InputChannelTrigger ChannelC
            | ChannelEnum.D   -> InputChannelTrigger ChannelD
            | ChannelEnum.Ext -> ExternalTrigger
            | ChannelEnum.Aux -> AuxiliaryTrigger
            | enum            -> failwithf "Unexpected trigger channel enum value: %A." enum

        /// Converts the provided trigger channel into a enumeration.
        let triggerChannelEnum = function
            | InputChannelTrigger channel -> inputChannelEnum channel
            | ExternalTrigger             -> ChannelEnum.Ext
            | AuxiliaryTrigger            -> ChannelEnum.Aux

    [<AutoOpen>]
    module Acquisition =
        /// Parses the provided enumeration into a resolution.
        let parseResolution = function
            | ResolutionEnum._8bit  -> Acquisition.Resolution_8bit
            | ResolutionEnum._12bit -> Acquisition.Resolution_12bit
            | ResolutionEnum._14bit -> Acquisition.Resolution_14bit
            | ResolutionEnum._15bit -> Acquisition.Resolution_15bit
            | ResolutionEnum._16bit -> Acquisition.Resolution_16bit
            | enum                  -> failwithf "Unexpected resolution enum value: %A." enum

        /// Converts the provided resolution into an enumeration.
        let resolutionEnum = function
            | Acquisition.Resolution_8bit  -> ResolutionEnum._8bit
            | Acquisition.Resolution_12bit -> ResolutionEnum._12bit
            | Acquisition.Resolution_14bit -> ResolutionEnum._14bit
            | Acquisition.Resolution_15bit -> ResolutionEnum._15bit
            | Acquisition.Resolution_16bit -> ResolutionEnum._16bit

        /// Parses the provided enumeration into an ETS mode.
        let parseEtsMode = function
            | EtsModeEnum.Off  -> Acquisition.NoEts
            | EtsModeEnum.Fast -> Acquisition.FastEts
            | EtsModeEnum.Slow -> Acquisition.SlowEts
            | enum             -> failwithf "Unexpected ETS mode enum value: %A." enum

        /// Converts the provided ETS mode to an enumeration.
        let etsModeEnum = function
            | Acquisition.NoEts   -> EtsModeEnum.Off
            | Acquisition.FastEts -> EtsModeEnum.Fast
            | Acquisition.SlowEts -> EtsModeEnum.Slow
        
        /// Parses the provided enumeration as a downsampling mode.
        let parseDownsamplingMode = function
            | DownsamplingModeEnum.None      -> NoDownsampling
            | DownsamplingModeEnum.Averaged  -> Averaged
            | DownsamplingModeEnum.Decimated -> Decimated
            | DownsamplingModeEnum.Aggregate -> Aggregate
            | enum                           -> failwithf "Unexpected downsampling mode enum value: %A." enum

        /// Converts the provided enumeration to a downsampling mode.
        let downsamplingModeEnum = function
            | NoDownsampling -> DownsamplingModeEnum.None
            | Averaged       -> DownsamplingModeEnum.Averaged
            | Decimated      -> DownsamplingModeEnum.Decimated
            | Aggregate      -> DownsamplingModeEnum.Aggregate
           
        /// Converts a set of downsampling modes into an enumeration.
        let downsamplingModeEnumForSet =
            Set.map downsamplingModeEnum
            >> Set.reduce (|||)
        
        /// Converts the provided StreamStop settings into the parameters required by the hardware,
        /// containing the flag indicating whether auto-stop is enabled and the maximum number of
        /// pre- and post-trigger samples.
        let streamStopParameters streamStop : (int16 * SampleIndex * SampleIndex) =
            match streamStop with
            | ManualStop                                       -> (0s, 0u, 1u)
            | AutoStop (preTriggerSamples, postTriggerSamples) -> (1s, preTriggerSamples, postTriggerSamples)
            
        /// Parses the provided trigger flag and position into a TriggerPosition.
        let parseTriggerPosition triggered position =
            if triggered then Triggered position
            else NotTriggered
    
    [<AutoOpen>]
    /// Parsing functions for signal generator settings.
    module SignalGenerator =
        open Model.SignalGenerator    

        /// Creates signal generator output voltage settings from peak-to-peak voltage and offset.
        let private outputVoltageSettings (peakToPeak : Voltage) (offset : Voltage) =
            { PeakToPeakVoltage = uint32 (int (peakToPeak * 1e6f<uV/V>))
              OffsetVoltage     = int (offset * 1e6f<uV/V>) } 
        
        /// Conversts the given sweep direction into an enumeration.
        let private sweepDirection = function
            | Up     -> SignalGeneratorSweepTypeEnum.Up
            | Down   -> SignalGeneratorSweepTypeEnum.Down
            | UpDown -> SignalGeneratorSweepTypeEnum.UpDown
            | DownUp -> SignalGeneratorSweepTypeEnum.DownUp

        /// Converts a waveform frequency (or sweep) into signal generator frequency settings.
        let private frequencySettings freq : SignalGeneratorFrequency =
            match freq with
            | FixedFrequency frequency ->
                { StartFrequency     = float32 frequency
                  StopFrequency      = float32 frequency
                  FrequencyIncrement = 0.0f
                  DwellTime          = 0.0f
                  SweepDirection     = SignalGeneratorSweepTypeEnum.Up }
            | FrequencySweep parameters ->
                { StartFrequency     = float32 parameters.StartFrequency
                  StopFrequency      = float32 parameters.StopFrequency
                  FrequencyIncrement = float32 parameters.FrequencyIncrement
                  DwellTime          = float32 parameters.DwellTime
                  SweepDirection     = sweepDirection parameters.SweepDirection }
        
        /// Converts a playback mode into signal generator playback settings.
        let private playbackMode = function 
            | ContinuousPlayback -> { Shots = System.UInt32.MaxValue ; Sweeps = System.UInt32.MaxValue }
            | NumberOfCycles n   -> { Shots = n  ; Sweeps = 0u }
            | NumberOfSweeps n   -> { Shots = 0u ; Sweeps = n  }
        
        /// Converts a given trigger type into an enumeration.
        let private triggerType = function
            | Rising   -> SignalGeneratorTriggerTypeEnum.Rising
            | Falling  -> SignalGeneratorTriggerTypeEnum.Falling
            | GateHigh -> SignalGeneratorTriggerTypeEnum.GateHigh
            | GateLow  -> SignalGeneratorTriggerTypeEnum.GateLow
        
        /// Converts an External channel input voltage to ADC counts.
        let private externalVoltageToAdcCounts (voltage : Voltage) =
            int16 <| (voltage / 5.0f<V>) * (float32 <| System.Int32.MaxValue)

        /// Converts a signal generator trigger into signal generator trigger settings.
        let private signalGeneratorTrigger = function
            | AutoTrigger -> 
                { TriggerSource     = SignalGeneratorTriggerSourceEnum.None
                  TriggerType       = SignalGeneratorTriggerTypeEnum.Rising
                  ExternalThreshold = 0s }
            | SignalGeneratorTrigger (ScopeTrigger, type') ->
                { TriggerSource     = SignalGeneratorTriggerSourceEnum.Scope
                  TriggerType       = triggerType type'
                  ExternalThreshold = 0s }
            | SignalGeneratorTrigger ((ExternalTrigger threshold), type') ->
                { TriggerSource     = SignalGeneratorTriggerSourceEnum.External
                  TriggerType       = triggerType type'
                  ExternalThreshold = externalVoltageToAdcCounts threshold }
            | SignalGeneratorTrigger (SoftwareTrigger, type') ->
                { TriggerSource     = SignalGeneratorTriggerSourceEnum.Software
                  TriggerType       = triggerType type'
                  ExternalThreshold = 0s }
        
        /// Returns the signal generator built-in function settings for a given function.
        let private builtInFunction = function
            | Sine _       -> { WaveformType = SignalGeneratorWaveTypeEnum.Sine ;       ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | Square _     -> { WaveformType = SignalGeneratorWaveTypeEnum.Square ;     ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | Triangle _   -> { WaveformType = SignalGeneratorWaveTypeEnum.Triangle ;   ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | DCVoltage _  -> { WaveformType = SignalGeneratorWaveTypeEnum.DcVoltage ;  ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | RampUp _     -> { WaveformType = SignalGeneratorWaveTypeEnum.RampUp ;     ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | RampDown _   -> { WaveformType = SignalGeneratorWaveTypeEnum.RampDown ;   ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | Sinc _       -> { WaveformType = SignalGeneratorWaveTypeEnum.Sinc ;       ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | Gaussian _   -> { WaveformType = SignalGeneratorWaveTypeEnum.Gaussian ;   ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | HalfSine _   -> { WaveformType = SignalGeneratorWaveTypeEnum.HalfSine ;   ExtraFunctions = SignalGeneratorExtrasEnum.None }
            | WhiteNoise _ -> { WaveformType = SignalGeneratorWaveTypeEnum.Sine ;       ExtraFunctions = SignalGeneratorExtrasEnum.WhiteNoise }
            | PseudoRandomBitStream _ -> 
                { WaveformType   = SignalGeneratorWaveTypeEnum.Sine
                  ExtraFunctions = SignalGeneratorExtrasEnum.PseudoRandomBitStream }

        /// Returns the peak-to-peak voltage, offset and frequency settings for a given function.
        let private outputVoltageAndFrequency = function
            | Sine       (peakToPeak, offset, frequency)
            | Square     (peakToPeak, offset, frequency)
            | Triangle   (peakToPeak, offset, frequency)
            | RampUp     (peakToPeak, offset, frequency)
            | RampDown   (peakToPeak, offset, frequency)
            | Sinc       (peakToPeak, offset, frequency)
            | Gaussian   (peakToPeak, offset, frequency)
            | HalfSine   (peakToPeak, offset, frequency)          -> (peakToPeak, offset, frequency)
            | WhiteNoise (peakToPeak, offset)                     -> (peakToPeak, offset, FixedFrequency 0.0f<Hz>)
            | DCVoltage offset                                    -> (0.0f<V>, offset, FixedFrequency 0.0f<Hz>) 
            | PseudoRandomBitStream (peakToPeak, offset, bitRate) -> (0.0f<V>, 0.0f<V>, FixedFrequency bitRate)

        /// Returns the signal generator built-in waveform settings for a given waveform.
        let builtInWaveformSettings waveform =
            let playbackSettings                = playbackMode waveform.PlaybackMode
            let triggerSettings                 = signalGeneratorTrigger waveform.TriggerSettings
            let function'                       = builtInFunction waveform.Waveform
            let (peakToPeak, offset, frequency) = outputVoltageAndFrequency waveform.Waveform
            { OutputVoltageSettings = outputVoltageSettings peakToPeak offset
              FrequencySettings     = frequencySettings frequency
              Function              = function'
              PlaybackSettings      = playbackSettings
              TriggerSettings       = triggerSettings }