namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.NationalInstruments

module Dummy =

    let standardResponses =
        Map.ofList <| [ ("*IDN?", "Agilent Technologies, N5172B, MY53051252, B.01.55")
                        (":SYSTEM:ERROR?", "+0,\"No error\"")
                        (":UNIT:POW?","DBM")
                         ]

    type DummyInstrument (behaviour : Map<string,string>) =
        let response = Map.join (fun k v1 v2 -> v2) standardResponses behaviour
        new() = DummyInstrument (Map.empty)
        interface Visa.IVisa with
         member __.closeInstrument () = asyncChoice { return () }
         member __.queryInstrument visaCommand = asyncChoice {
            return! match response.TryFind visaCommand with
                    | Some str -> succeed str
                    | None -> fail ("Missing response for " + visaCommand) }
         member __.readString () = asyncChoice { return "" }
         member __.writeString visaCommand = printfn "write: %s" visaCommand

    let openInstrument (behaviour : Map<string,string>) =
        new DummyInstrument (behaviour) :> Visa.IVisa |> RfSource
    let openDumbInstrument =
        new DummyInstrument () :> Visa.IVisa |> RfSource
