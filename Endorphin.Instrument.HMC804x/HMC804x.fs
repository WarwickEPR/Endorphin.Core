// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.HMC804x

/// Command set of the Keysight RF instrument.
/// Implements functions to modify & query configuration.
/// Organised by subsystem mirroring the Keysight configuration.
module HMC804x =

    /// Open a current/voltage source given a VISA address and timeout in milliseconds.
    let openInstrument  = IO.Connect.openInstrument

    /// Reset the instrument
    let reset =
        IO.writeKey "*RST"

    /// Mark the instrument as under remote operation
    let remote =
        IO.writeKey ":SYSTEM:REMOTE"

    /// Mark the instrument as under local operation
    let local =
        IO.writeKey ":SYSTEM:LOCAL"

    /// Selectable outputs of device
    let availableOutputs = function
    | HMC8041 -> [ OUT1 ] |> Set.ofList
    | HMC8042 -> [ OUT1; OUT2 ] |> Set.ofList
    | HMC8043 -> [ OUT1; OUT2; OUT3 ] |> Set.ofList

    /// Select an output channel to apply subsequent configuration to
    let selectOutput =
        IO.setOutput ":INST"

    /// Open a current/voltage source, reset it's state and set it as remote operated
    let takeInstrument resource time = async {
        let! inst = openInstrument resource time
        do! inst |> reset
        do! inst |> remote
        return inst }

    /// Close a source instrument.
    let closeInstrument = IO.Connect.closeInstrument

    /// Set the instrument to local and close
    let releaseInstrument inst = async {
        do! inst |> local
        do! inst |> closeInstrument }

    /// Query the identity of a source, and match the returned string against ones known
    /// to the program.
    let queryIdentity = IO.Identify.queryIdentity

    let setCurrent =
        IO.setCurrent ":CURR"

    let setVoltage =
        IO.setVoltage ":VOLT"

    /// Enable/Disable the currently selected channel
    let setChannelOutput =
        IO.setOnOffState ":OUTPUT:CHANNEL"

    /// Enable/Disable all enabled channels
    let setMasterOutput =
        IO.setOnOffState ":OUTPUT:MASTER"

    let setArbData =
        IO.setArb ":ARB:DATA"

    let setArbTriggering =
        IO.setOnOffState ":ARB:TRIG"

    let setArbRepetitions =
        IO.setArbRepetitions ":ARB:REP"

    let setArbTriggerMode =
        IO.setTriggerMode ":ARB:TRIG:MODE"

    let setArb =
        IO.setOnOffState ":ARB"

    let applyOutputSettings inst settings = async {
        match settings.Output with
        | Fixed (V,I)  -> do! setCurrent inst I
                          do! setVoltage inst V
        | Arb arb -> do! setArbData inst arb.Sequence
                     do! setArbTriggering inst arb.Triggering
                     do! setArbRepetitions inst arb.Repetitions
                     do! setArbTriggerMode inst arb.TriggerMode
                     do! setArb inst On
    }

    /// Apply a set of settings to the given current/voltage source
    let applySettings inst powerSupplySettings = async {
        for KeyValue(output,settings) in powerSupplySettings do
            do! selectOutput inst output
            do! applyOutputSettings inst settings
            do! setChannelOutput inst On
        }

    let start inst = setMasterOutput inst On

    let constantOutput voltage current =
        { Output     = Fixed (Voltage_V voltage, Current_A current)
          PowerLimit = None }

    let rampContinuous startVoltage startCurrent finishVoltage finishCurrent riseTime fallTime =
        let start  = (Voltage_V startVoltage, Current_A startCurrent, Time_s riseTime,Interpolate)
        let finish = (Voltage_V finishVoltage, Current_A finishCurrent, Time_s fallTime,Interpolate)
        { Output = Arb { Sequence    = [ start; finish ] |> Seq.ofList
                         Repetitions = Forever
                         Triggering  = Off
                         TriggerMode = Run }
          PowerLimit = None }
        
    let rampTriggered startVoltage startCurrent finishVoltage finishCurrent riseTime fallTime =
        let start  = (Voltage_V startVoltage, Current_A startCurrent, Time_s riseTime,Interpolate)
        let finish = (Voltage_V finishVoltage, Current_A finishCurrent, Time_s fallTime,Interpolate)
        { Output = Arb { Sequence    = [ start; finish ] |> Seq.ofList
                         Repetitions = Forever
                         Triggering  = On
                         TriggerMode = Single }
          PowerLimit = None }

    let withMaximumPower power settings =
        { settings with PowerLimit = Some <| MaximumPower (Power_W power) }
        