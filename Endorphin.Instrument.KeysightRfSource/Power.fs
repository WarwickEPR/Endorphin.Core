// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight

module Power =
    module Control =
        /// Key to control the ALC circuitry.
        /// Command reference p.77.
        let private alcKey = ":POWER:ALC"
        /// Set the state of the ALC circuitry.
        let setAlcState = IO.setOnOffState alcKey
        /// Query the state of the ALC circuitry.
        let queryAlcState = IO.queryOnOffState alcKey