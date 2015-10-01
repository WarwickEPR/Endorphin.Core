namespace Endorphin.Instrument.PicoScope3000

open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

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
        | _ -> unexpectedReply <| sprintf "Unexpected model number: %s." modelNumber
