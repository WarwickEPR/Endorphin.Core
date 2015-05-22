namespace Endorphin.Instrument.PicoScope5000

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module ChannelSettings =

    /// Enumeration representing a PicoScope 5000 series device channel.
    type internal ChannelEnum =
        | A    = 0
        | B    = 1
        | C    = 2
        | D    = 3
        | Ext  = 4
        | Aux  = 5
        | None = 6

    type InputChannel =
        | ChannelA
        | ChannelB
        | ChannelC
        | ChannelD

    let internal parseInputChannel =
        function
        | ChannelEnum.A -> ChannelA
        | ChannelEnum.B -> ChannelB
        | ChannelEnum.C -> ChannelC
        | ChannelEnum.D -> ChannelD
        | enum          -> failwithf "Unexpected input channel enum value: %A." enum

    let internal (|InputChannel|) =
        function
        | ChannelA -> ChannelEnum.A
        | ChannelB -> ChannelEnum.B
        | ChannelC -> ChannelEnum.C
        | ChannelD -> ChannelEnum.D

    type TriggerChannel =
        | InputChannelTrigger of inputChannel : InputChannel
        | ExternalTrigger
        | AuxiliaryTrigger

    let internal parseTriggerChannelOption =
        function
        | ChannelEnum.A    -> Some (InputChannelTrigger ChannelA)
        | ChannelEnum.B    -> Some (InputChannelTrigger ChannelB)
        | ChannelEnum.C    -> Some (InputChannelTrigger ChannelC)
        | ChannelEnum.D    -> Some (InputChannelTrigger ChannelD)
        | ChannelEnum.Ext  -> Some ExternalTrigger
        | ChannelEnum.Aux  -> Some AuxiliaryTrigger
        | ChannelEnum.None -> None
        | enum             -> failwithf "Unexpected trigger channel enum value: %A." enum

    let internal (|TriggerChannel|) =
        function
        | Some (InputChannelTrigger (InputChannel channel)) -> channel
        | Some ExternalTrigger                              -> ChannelEnum.Ext
        | Some AuxiliaryTrigger                             -> ChannelEnum.Aux
        | None                                              -> ChannelEnum.None

    /// Enumeration representing the input coupling of a PicoScope 5000 series channel.
    type internal CouplingEnum = 
        | AC = 0
        | DC = 1

    type Coupling = AC | DC

    let internal parseCoupling =
        function
        | CouplingEnum.AC -> AC
        | CouplingEnum.DC -> DC
        | enum            -> failwithf "Unexpected coupling enum value: %A." enum

    let internal (|Coupling|) =
        function
        | AC -> CouplingEnum.AC
        | DC -> CouplingEnum.DC

    /// Enumeration representing the input voltage range of a PicoScope 5000 series channel.
    type internal RangeEnum =
        | _10mV  = 0
        | _20mV  = 1
        | _50mV  = 2
        | _100mV = 3
        | _200mV = 4
        | _500mV = 5
        | _1V    = 6
        | _2V    = 7
        | _5V    = 8
        | _10V   = 9
        | _20V   = 10
        | _50V   = 11

    type Range =
        | Range_10mV
        | Range_20mV
        | Range_50mV
        | Range_100mV
        | Range_200mV
        | Range_500mV
        | Range_1V
        | Range_2V
        | Range_5V
        | Range_10V
        | Range_20V
        | Range_50V

    let internal parseRange =
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

    let internal (|Range|) =
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

    let rangeToVoltage =
        function
        | Range_10mV  -> VoltageInVolts 0.010f<V>
        | Range_20mV  -> VoltageInVolts 0.020f<V>
        | Range_50mV  -> VoltageInVolts 0.050f<V>
        | Range_100mV -> VoltageInVolts 0.100f<V>
        | Range_200mV -> VoltageInVolts 0.200f<V>
        | Range_500mV -> VoltageInVolts 0.500f<V>
        | Range_1V    -> VoltageInVolts 1.0f<V>
        | Range_2V    -> VoltageInVolts 2.0f<V>
        | Range_5V    -> VoltageInVolts 5.0f<V>
        | Range_10V   -> VoltageInVolts 10.0f<V>
        | Range_20V   -> VoltageInVolts 20.0f<V>
        | Range_50V   -> VoltageInVolts 50.0f<V>

    /// Returns the smallest available input range which will handle a specified zero-to-peak input voltage.
    let minimumRangeForVoltage availableRanges voltage =
        availableRanges
        |> Seq.sortBy rangeToVoltage
        |> Seq.tryFind (fun rangeVoltage -> rangeVoltage > voltage)

    /// Enumeration indicating whether equivalent time sampling is enabled on a PicoScope 5000 series device.
    type internal EtsModeEnum =
        | Off = 0
        | Fast = 1
        | Slow = 2

    type EtsMode = 
        | NoEts
        | FastEts
        | SlowEts

    let internal parseEtsMode =
        function
        | EtsModeEnum.Off  -> NoEts
        | EtsModeEnum.Fast -> FastEts
        | EtsModeEnum.Slow -> SlowEts
        | enum             -> failwithf "Unexpected ETS mode enum value: %A." enum

    let internal (|EtsMode|) =
        function
        | NoEts   -> EtsModeEnum.Off
        | FastEts -> EtsModeEnum.Fast
        | SlowEts -> EtsModeEnum.Slow

    /// Enumeration representing possible types channel information which can be requested from a PicoScope 5000 series device.
    type ChannelInfoEnum =
        | VoltageOffsetRanges = 0

    /// Enumeration representing the input bandwidth of a PicoScope 5000 series device.
    type internal BandwidthLimitEnum =
        | Full = 0
        | _20MHz = 1

    type Bandwidth =
        | FullBandwidth
        | Bandwidth_20MHz

    let internal parseBandwidthLimit =
        function
        | BandwidthLimitEnum.Full   -> FullBandwidth
        | BandwidthLimitEnum._20MHz -> Bandwidth_20MHz
        | enum                      -> failwithf "Unexpected bandwidth limit enum value: %A." enum

    let internal (|Bandwidth|) =
        function
        | FullBandwidth   -> BandwidthLimitEnum.Full
        | Bandwidth_20MHz -> BandwidthLimitEnum._20MHz 

    /// Defines all input settings for a given PicoScope 5000 series channel.
    type InputSettings = 
        { Coupling       : Coupling
          Range          : Range
          AnalogueOffset : Voltage
          BandwidthLimit : Bandwidth }

    /// Defines the possible states of a PicoScope 5000 series channel. It can either be enabled or
    /// disabled. If it is enabled, it need to have specified input settings.
    type ChannelSettings =
        | EnabledChannel of inputSettings : InputSettings
        | DisabledChannel