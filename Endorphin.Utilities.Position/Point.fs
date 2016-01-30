// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Utilities.Position
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


module Point =
    type VoltagePoint = decimal<V> * decimal<V> * decimal<V>

    type Point = decimal<um> * decimal<um> * decimal<um>
    
    let tfst (a, _, _) =
        a
    
    let tsnd (_, a, _) = 
        a

    let ttrd (_, _, a) = 
        a