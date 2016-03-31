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
        | MainsPower -> ([ ChannelA; ChannelB; ChannelC; ChannelD] |> List.map Analogue)
                        @ ([ Port0 ; Port1 ] |> List.map Digital)
                        |> Set.ofList
        | UsbPower   -> List.map Analogue [ ChannelA ; ChannelB ] |> Set.ofList

[<RequireQualifiedAccess>]
/// Functions related to the device.
module Device =
    /// Returns a set of available channels for the specified model number. Note that fewer
    /// channels may be available depending on the device resolution, power source and other
    /// acquisition settings.
    let internal availableChannelsForModel (modelNumber : string) =

        let analogueChannels =
            match int <| modelNumber.[1].ToString() with
            | 2 -> [ ChannelA ; ChannelB ] |> List.map Analogue |> Set.ofList
            | 4 -> [ ChannelA ; ChannelB ; ChannelC ; ChannelD ] |> List.map Analogue |> Set.ofList
            | _ -> unexpectedReply <| sprintf "Unexpected model number: %s." modelNumber

        let digitalChannels =
            if modelNumber.EndsWith("MSO") then
                [ Port0 ; Port1 ] |> List.map Digital |>  Set.ofList
            else
                Set.empty

        Set.union analogueChannels digitalChannels

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
        if level > maximum || level < minimum  then
            invalidArg "Logic level" <| sprintf "Logic level must be between %f V and %f V" minimum maximum

    /// Get the logic level as an int16 from a voltage.
    let fromVoltage voltage =
        check voltage
        int16 <| (float voltage / float maximum) * (float NativeApi.Quantities.maximumLogicLevelAdc)


module Timebase =

    // using programming guide, section 3.6 for USB 3.0 models of PicoScope 3000
    // May not have all modes available, unless some channels are disabled
    let timebase sampleInterval =
        let ns = Interval.asIntegerNanoseconds sampleInterval

        if ns < 2L      then 0u   // 1 ns
        else if ns < 4L then 1u   // 2 ns
        else if ns < 8L then 2u   // 4 ns
        else (uint32 ns) * 125u / 1000u + 1u