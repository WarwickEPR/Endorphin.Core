// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope5000

open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
/// Functions related to vertical resolution for digital samples. The PicoScope 5000
/// series uses a variable resolution architecture which allows it to change the
/// resolution between 8 and 16 bit in exchange for having fewer channels and/or a
/// lower maximum sampling rate. 
module Resolution =

    /// Returns the value of the resolution in bits.
    let valueInBits =
        function
        | Resolution_8bit  ->  8<bit>
        | Resolution_12bit -> 12<bit>
        | Resolution_14bit -> 14<bit>
        | Resolution_15bit -> 15<bit>
        | Resolution_16bit -> 16<bit>

    /// Returns a set of available channels for the given resolution. Note that fewer
    /// channels may be available depending on the device model, power source and
    /// acquisition settings.
    let internal availableChannels = function
        | Resolution_8bit
        | Resolution_12bit 
        | Resolution_14bit -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | Resolution_15bit -> [ ChannelA ; ChannelB ] |> Set.ofList
        | Resolution_16bit -> [ ChannelA ] |> Set.ofList

    /// Returns the maximum ADC count value for a given resolution.
    let maximumAdcCounts = function
        | Resolution_8bit -> 0x7F00s : AdcCount
        | _               -> 0x7FFFs : AdcCount

[<RequireQualifiedAccess>]
/// Functions related to device timebase and sample interval.
module Timebase =
    
    /// The fastest available timebase for a given resolution.
    let fastestForResolution = function
        | Resolution_8bit  -> 0u
        | Resolution_12bit -> 1u
        | Resolution_14bit
        | Resolution_15bit -> 3u
        | Resolution_16bit -> 4u

    /// Returns the next timebase value, corresponding to a longer sample interval.
    let next index = (index + 1u)

    /// Returns the fastest streaming acquisition sample interval available for a
    /// given number of enabled channels and vertical resolution.
    let fastestStreamingInterval channelCount resolution =
        match (resolution, channelCount) with
        | (Resolution_12bit, 4)
        | (Resolution_12bit, 3)
        | (Resolution_14bit, 4)
        | (Resolution_14bit, 3) -> Interval_ns 256<ns>
        | (Resolution_8bit,  4) 
        | (Resolution_8bit,  3) 
        | (Resolution_12bit, 2) 
        | (Resolution_14bit, 2) 
        | (Resolution_15bit, 2) -> Interval_ns 128<ns>
        | (Resolution_8bit,  3)
        | (Resolution_12bit, 1)
        | (Resolution_14bit, 1)
        | (Resolution_15bit, 1)
        | (Resolution_16bit, 2) -> Interval_ns 64<ns>
        | (Resolution_8bit,  1) -> Interval_ns 32<ns>
        | _ -> failwithf "Invalid number of channels for resolution %A: %d." resolution channelCount

    // using programming guide, section 3.6 for USB 3.0 models of PicoScope 3000
    // May not have all modes available, unless some channels are disabled
    let timebase sampleInterval =
        let ns = Interval.asIntegerNanoseconds sampleInterval

        if ns < 2L      then 0u   // 1 ns
        else if ns < 4L then 1u   // 2 ns
        else if ns < 8L then 2u   // 4 ns
        else (uint32 ns) * 125u / 1000u + 1u

[<RequireQualifiedAccess>]
/// Functions related to memory segments.
module MemorySegment =
    
    /// Returns the default memory segment. When the device is started, its memory only
    /// contains one segment.
    let zero = 0u

    /// Returns the next memory segment.
    let next index = (index + 1u)

[<RequireQualifiedAccess>]
/// Functions related to the device power source.
module PowerSource =
    
    /// Returns a set of available channels for the given device power source. Note that
    /// fewer channels may be available depending on the device resolution, model and other 
    /// acquisition settings.
    let internal availableChannels = function
        | MainsPower -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | UsbPower   -> [ ChannelA ; ChannelB ] |> Set.ofList

[<RequireQualifiedAccess>]
/// Functions related to the device.
module Device =

    /// Returns a set of available channels for the specified model number. Note that fewer
    /// channels may be available depending on the device resolution, power source and other
    /// acquisition settings.
    let internal availableChannelsForModel (modelNumber : string) =
        match int <| modelNumber.[1].ToString() with
        | 2 -> [ ChannelA ; ChannelB ] |> Set.ofList 
        | 4 -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | _ -> failwithf "Unexpected model number: %s." modelNumber

[<RequireQualifiedAccess>]
/// Functions related to sample voltages.
module Voltage =
    
    /// Converts the given ADC counts value sampled at the specified voltage range and resolution
    /// to a voltage.
    let fromAdcCounts resolution range analogueOffset (adcCounts : AdcCount) =
        Range.voltage range * ((float32 adcCounts) / (float32 <| Resolution.maximumAdcCounts resolution)) - analogueOffset
