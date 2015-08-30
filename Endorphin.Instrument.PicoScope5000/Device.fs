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
        | Resolution_8bit  ->  8<bits>
        | Resolution_12bit -> 12<bits>
        | Resolution_14bit -> 14<bits>
        | Resolution_15bit -> 15<bits>
        | Resolution_16bit -> 16<bits>

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
        | Resolution_8bit -> 0x7F00s
        | _               -> 0x7FFFs

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
        | (Resolution_14bit, 3) -> Interval.fromNanoseconds 256<ns>
        | (Resolution_8bit,  4) 
        | (Resolution_8bit,  3) 
        | (Resolution_12bit, 2) 
        | (Resolution_14bit, 2) 
        | (Resolution_15bit, 2) -> Interval.fromNanoseconds 128<ns>
        | (Resolution_8bit,  3)
        | (Resolution_12bit, 1)
        | (Resolution_14bit, 1)
        | (Resolution_15bit, 1)
        | (Resolution_16bit, 2) -> Interval.fromNanoseconds 64<ns>
        | (Resolution_8bit,  1) -> Interval.fromNanoseconds 32<ns>
        | _ -> failwithf "Invalid number of channels for resolution %A: %d." resolution channelCount

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
