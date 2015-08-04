namespace Endorphin.Instrument.PicoScope5000

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

[<RequireQualifiedAccess>]
module Timebase =
    let fastestForResolution =
        function
        | Resolution_8bit  -> Timebase 0u
        | Resolution_12bit -> Timebase 1u
        | Resolution_14bit
        | Resolution_15bit -> Timebase 3u
        | Resolution_16bit -> Timebase 4u

    let next (Timebase index) = Timebase (index + 1u)

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
    let zero = MemorySegment 0u
    let next (MemorySegment index) = MemorySegment (index + 1u)