namespace Endorphin.Instrument.PicoScope5000

open NativeModel
open StatusCodes

module internal Parsing =
    let parseToggleState value = if value <> 0s then Enabled else Disabled

    [<AutoOpen>]
    module StatusCodes = 
        let (|Ok|Error|) =
            function
            | StatusCode.Ok -> Ok
            | error         -> Error (statusMessage error)

        let (|AvailabilityStatus|_|) =
            function
            | StatusCode.Ok   -> Some (AvailabilityStatus Available)
            | StatusCode.Busy -> Some (AvailabilityStatus Busy)
            | _               -> None

        let (|Found|_|) =
            function
            | StatusCode.Ok       -> Some (Found true)
            | StatusCode.NotFound -> Some (Found false)
            | _                   -> None

        let (|PowerSourceStatus|_|) =
            function
            | StatusCode.PowerSupplyConnected    -> Some MainsPower
            | StatusCode.PowerSupplyNotConnected -> Some UsbPower
            | _                                  -> None

        let powerSourceStatusCode =
            function
            | MainsPower -> StatusCode.PowerSupplyConnected
            | UsbPower   -> StatusCode.PowerSupplyNotConnected

    [<AutoOpen>]
    module Device =
        let ledFlashCounts =
            function
            | LedOff              -> 0s
            | LedRepeat n         -> n
            | LedIndefiniteRepeat -> -1s 

    let parseIntervalWithInterval (interval, unit) =
        match unit with
        | TimeUnitEnum.Femtoseconds -> IntervalInFemtoseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Picoseconds  -> IntervalInPicoseconds  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Nanoseconds  -> IntervalInNanoseconds  (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Microseconds -> IntervalInMicroseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Milliseconds -> IntervalInMilliseconds (LanguagePrimitives.Int32WithMeasure interval)
        | TimeUnitEnum.Seconds      -> IntervalInSeconds      (LanguagePrimitives.Int32WithMeasure interval)
        | enum                      -> failwithf "Unexpected time unit enum value: %A" enum

    let intervalAndTimeUnitEnum =
        function
        | IntervalInFemtoseconds interval -> (int interval, TimeUnitEnum.Femtoseconds)
        | IntervalInPicoseconds  interval -> (int interval, TimeUnitEnum.Picoseconds )
        | IntervalInNanoseconds  interval -> (int interval, TimeUnitEnum.Nanoseconds )
        | IntervalInMicroseconds interval -> (int interval, TimeUnitEnum.Microseconds)
        | IntervalInMilliseconds interval -> (int interval, TimeUnitEnum.Milliseconds)
        | IntervalInSeconds      interval -> (int interval, TimeUnitEnum.Seconds     )
    
    let voltageFloatInVolts (VoltageInVolts v) = float32 v
    let autoTriggerDelayIntInMilliseconds = 
        function
        | Some (AutoTriggerDelayInMilliseconds delay) -> int16 delay
        | None                                        -> 0s

    [<AutoOpen>]
    module ChannelSettings =
        let parseInputChannel =
            function
            | ChannelEnum.A -> ChannelA
            | ChannelEnum.B -> ChannelB
            | ChannelEnum.C -> ChannelC
            | ChannelEnum.D -> ChannelD
            | enum          -> failwithf "Unexpected input channel enum value: %A." enum

        let inputChannelEnum =
            function
            | ChannelA -> ChannelEnum.A
            | ChannelB -> ChannelEnum.B
            | ChannelC -> ChannelEnum.C
            | ChannelD -> ChannelEnum.D
        
        let parseCoupling =
            function
            | CouplingEnum.AC -> AC
            | CouplingEnum.DC -> DC
            | enum            -> failwithf "Unexpected coupling enum value: %A." enum

        let couplingEnum =
            function
            | AC -> CouplingEnum.AC
            | DC -> CouplingEnum.DC

        let parseRange =
            function
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

        let rangeEnum =
            function
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

        let parseBandwidthLimit =
            function
            | BandwidthLimitEnum.Full   -> FullBandwidth
            | BandwidthLimitEnum._20MHz -> Bandwidth_20MHz
            | enum                      -> failwithf "Unexpected bandwidth limit enum value: %A." enum

        let bandwidthLimitEnum =
            function
            | FullBandwidth   -> BandwidthLimitEnum.Full
            | Bandwidth_20MHz -> BandwidthLimitEnum._20MHz 

    [<AutoOpen>]
    module Triggering =
        let internal levelThresholdEnum =
            function
            | Above           -> ThresholdDirectionEnum.Above          
            | Below           -> ThresholdDirectionEnum.Below          
            | Rising          -> ThresholdDirectionEnum.Rising         
            | Falling         -> ThresholdDirectionEnum.Falling        
            | RisingOrFalling -> ThresholdDirectionEnum.RisingOrFalling
                
        let internal windowThresholdEnum = 
            function
            | Inside      -> ThresholdDirectionEnum.Inside     
            | Outside     -> ThresholdDirectionEnum.Outside    
            | Enter       -> ThresholdDirectionEnum.Enter      
            | Exit        -> ThresholdDirectionEnum.Exit       
            | EnterOrExit -> ThresholdDirectionEnum.EnterOrExit

        let parseTriggerChannel =
            function
            | ChannelEnum.A   -> InputChannelTrigger ChannelA
            | ChannelEnum.B   -> InputChannelTrigger ChannelB
            | ChannelEnum.C   -> InputChannelTrigger ChannelC
            | ChannelEnum.D   -> InputChannelTrigger ChannelD
            | ChannelEnum.Ext -> ExternalTrigger
            | ChannelEnum.Aux -> AuxiliaryTrigger
            | enum            -> failwithf "Unexpected trigger channel enum value: %A." enum

        let triggerChannelEnum =
            function
            | InputChannelTrigger channel -> inputChannelEnum channel
            | ExternalTrigger             -> ChannelEnum.Ext
            | AuxiliaryTrigger            -> ChannelEnum.Aux

    [<AutoOpen>]
    module Acquisition =
        let parseResolution =
            function
            | ResolutionEnum._8bit  -> Acquisition.Resolution_8bit
            | ResolutionEnum._12bit -> Acquisition.Resolution_12bit
            | ResolutionEnum._14bit -> Acquisition.Resolution_14bit
            | ResolutionEnum._15bit -> Acquisition.Resolution_15bit
            | ResolutionEnum._16bit -> Acquisition.Resolution_16bit
            | enum                  -> failwithf "Unexpected resolution enum value: %A." enum

        let resolutionEnum = 
            function
            | Acquisition.Resolution_8bit  -> ResolutionEnum._8bit
            | Acquisition.Resolution_12bit -> ResolutionEnum._12bit
            | Acquisition.Resolution_14bit -> ResolutionEnum._14bit
            | Acquisition.Resolution_15bit -> ResolutionEnum._15bit
            | Acquisition.Resolution_16bit -> ResolutionEnum._16bit

        let parseEtsMode =
            function
            | EtsModeEnum.Off  -> Acquisition.NoEts
            | EtsModeEnum.Fast -> Acquisition.FastEts
            | EtsModeEnum.Slow -> Acquisition.SlowEts
            | enum             -> failwithf "Unexpected ETS mode enum value: %A." enum

        let (|EtsMode|) =
            function
            | Acquisition.NoEts   -> EtsModeEnum.Off
            | Acquisition.FastEts -> EtsModeEnum.Fast
            | Acquisition.SlowEts -> EtsModeEnum.Slow

        let parseDownsamplingMode =
            function
            | DownsamplingModeEnum.None      -> NoDownsampling
            | DownsamplingModeEnum.Averaged  -> Averaged
            | DownsamplingModeEnum.Decimated -> Decimated
            | DownsamplingModeEnum.Aggregate -> Aggregate
            | enum                           -> failwithf "Unexpected downsampling mode enum value: %A." enum

        let downsamplingModeEnum =
            function
            | NoDownsampling -> DownsamplingModeEnum.None
            | Averaged       -> DownsamplingModeEnum.Averaged
            | Decimated      -> DownsamplingModeEnum.Decimated
            | Aggregate      -> DownsamplingModeEnum.Aggregate
            
        let downsamplingModeEnumForSet =
            Set.map downsamplingModeEnum
            >> Set.reduce (|||)
        
        let streamStopParameters streamStop : (int16 * SampleIndex * SampleIndex) =
            match streamStop with
            | ManualStop                                       -> (0s, 0u, 1u)
            | AutoStop (preTriggerSamples, postTriggerSamples) -> (1s, preTriggerSamples, postTriggerSamples)

        let parseTriggerPosition triggered position =
            if triggered then Triggered position
            else NotTriggered
 