// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r @"bin\Debug\Endorphin.Instrument.HMC804x.dll"
#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.HMC804x
open HMC804x

log4net.Config.BasicConfigurator.Configure ()


let settings : PowerSupplySettings = [ (OUT1, constantOutput 5.0<V> 0.2<A> )
                                       (OUT2, rampContinuous 0.0<V> 0.1<A> 2.0<V> 0.1<A> 1.0<s> 0.2<s> |> withMaximumPower 0.5<W>)
                                       (OUT3, rampContinuous 1.0<V> 0.1<A> 4.0<V> 0.1<A> 1.0<s> 0.2<s> |> withMaximumPower 0.5<W>) ]
                                     |> Map.ofList

try
    async {
        // open the Rohde & Schwartz current source - set the VISA access string you need here and timeout
        let! source = HMC804x.takeInstrument "TCPIP0::10.0.0.2::5025::SOCKET" 5000<ms>
        do! applySettings source settings
        do! start source
        do! Async.Sleep 5000
        // tidy up and close
        do! HMC804x.releaseInstrument source }
    |> Async.RunSynchronously
with
    | :? InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0
