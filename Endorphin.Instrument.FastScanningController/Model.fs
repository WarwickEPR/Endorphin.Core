// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.FastScanningController
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core


[<AutoOpen>]
module Model =
    [<AutoOpen>]
    module Motion = 
        /// Linear calibration for stage displacement as a function of voltage
        type PositionCalibration = 
              { X     : decimal<m/V>
                Y     : decimal<m/V>
                Z     : decimal<m/V> }

type ScanningController = internal ScanningController of scanningController : Visa.Instrument * settings : PositionCalibration