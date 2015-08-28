﻿namespace Endorphin.Instrument.PicoScope5000

open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
module Resolution =
    let valueInBits =
        function
        | Resolution_8bit  ->  8<bits>
        | Resolution_12bit -> 12<bits>
        | Resolution_14bit -> 14<bits>
        | Resolution_15bit -> 15<bits>
        | Resolution_16bit -> 16<bits>

    let internal availableChannels =
        function
        | Resolution_8bit
        | Resolution_12bit 
        | Resolution_14bit -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | Resolution_15bit -> [ ChannelA ; ChannelB ] |> Set.ofList
        | Resolution_16bit -> [ ChannelA ] |> Set.ofList

    let maximumAdcCounts = function
        | Resolution_8bit -> 0x7F00s
        | _               -> 0x7FFFs

[<RequireQualifiedAccess>]
module Timebase =
    let fastestForResolution =
        function
        | Resolution_8bit  -> 0u
        | Resolution_12bit -> 1u
        | Resolution_14bit
        | Resolution_15bit -> 3u
        | Resolution_16bit -> 4u

    let next index = (index + 1u)

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
module MemorySegment =
    let zero = 0u
    let next index = (index + 1u)


[<RequireQualifiedAccess>]
module PowerSource =
    let availableChannels = function
        | MainsPower -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | UsbPower   -> [ ChannelA ; ChannelB ] |> Set.ofList

[<RequireQualifiedAccess>]
module Device =
    let availableChannelsForModel (modelNumber : string) =
        match int <| modelNumber.[1].ToString() with
        | 2 -> [ ChannelA ; ChannelB ] |> Set.ofList 
        | 4 -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> Set.ofList
        | _ -> failwithf "Unexpected model number: %s." modelNumber
