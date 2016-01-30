// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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

    /// Returns a set of the available digital ports for the specified model number.  Only MSO
    /// models have any digital ports at all.
    let internal availablePortsForModel (modelNumber : string) =
        match modelNumber.[-3 .. -1] with
        | "MSO" -> [ DigitalPort0 ; DigitalPort1 ] |> Set.ofList
        | _     -> Set.empty

/// Functions related to the logic level of digital ports - what voltage must be reached for the port to
/// switch from 0 to 1.
module internal LogicLevel =
    /// The minimum logic level the machine can deal with.
    [<Literal>]
    let minimum = -5.0f<V>

    /// The maximum logic level the machine can deal with.
    [<Literal>]
    let maximum = 5.0f<V>

    /// Raise an exception if the specified logic level is not in the allowed range for the instrument.
    let check level =
        if not <| Voltage.between (Voltage.fromVolts minimum) (Voltage.fromVolts maximum) level then
            invalidArg "Logic level" <| sprintf "Logic level must be between %f V and %f V" minimum maximum

    /// Get the logic level as an int16 from a voltage.
    let fromVoltage voltage =
        check voltage
        let volts = Voltage.asVolts voltage
        int16 <| (float volts / float maximum) * (float NativeApi.Quantities.maximumLogicLevel)