namespace Endorphin.Instrument.FastScanningController
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core

[<AutoOpen>]
module internal Parsing = 
    let dwellString (dwellTime : float<ms>) = 
        sprintf "DWELL %.1f" dwellTime

    let triggerDelayString (triggerDelay : float<ms>) = 
        sprintf "DELAY %.1f" triggerDelay

    let downloadString (numberOfPoints : int) = 
        sprintf "DL %d" numberOfPoints

    let internal tfst (a, _, _) =
        a
    
    let internal tsnd (_, a, _) = 
        a

    let internal ttrd (_, _, a) = 
        a