// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Units =

    [<AutoOpen>]
    module Length =

        /// Millimetre.
        [<Measure>] type mm

        /// Micrometre.
        [<Measure>] type um

        /// Nanometre.
        [<Measure>] type nm

        let metresPerMillimetre =  1.0e-3<m/mm>
        let metresPerMicrometre =  1.0e-6<m/um>
        let metresPerNanometre  =  1.0e-9<m/nm>
        
        let millimetresPerMetre       =  1.0e3<mm/m>
        let millimetresPerMicrometre  =  1.0e-3<mm/um>
        let millimetresPerNanometre   =  1.0e-6<mm/nm>

        let micrometresPerMetre      =  1.0e6<um/m>
        let micrometresPerMillimetre =  1.0e3<um/mm>
        let micrometresPerNanometre  =  1.0e-3<um/nm>

        let nanometresPerMetre       =  1.0e9<nm/m>
        let nanometresPerMillimetre  =  1.0e6<nm/mm>
        let nanometresPerMicrometre  =  1.0e3<nm/um>

        module Metres =
            let toMillimetres  (t : float<m>) = t * millimetresPerMetre
            let toMicrometres  (t : float<m>) = t * micrometresPerMetre
            let toNanometres   (t : float<m>) = t * nanometresPerMetre

        module Millimetres =
            let toMeters       (t : float<mm>) = t * metresPerMillimetre
            let toMicrometres  (t : float<mm>) = t * micrometresPerMillimetre
            let toNanometres   (t : float<mm>) = t * nanometresPerMillimetre

        module Micrometres =
            let toMetres       (t : float<um>) = t * metresPerMicrometre
            let toMillimetres  (t : float<um>) = t * millimetresPerMicrometre
            let toNanometres   (t : float<um>) = t * nanometresPerMicrometre

        module Nanometres =
            let toMetres       (t : float<nm>) = t * metresPerNanometre
            let toMillimetres  (t : float<nm>) = t * millimetresPerNanometre
            let toMicrometres  (t : float<nm>) = t * micrometresPerNanometre