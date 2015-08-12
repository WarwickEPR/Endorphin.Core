namespace Endorphin.Instrument.Keysight

open ExtCore.Control

module Route =
    [<AutoOpen>]
    module internal Parsing =
        /// If a type claiming to provide an interface we're using is passed, but we don't know about
        /// it, we have to fail execution.
        let private failIncorrectType signal =
            failwithf "Unexpected output signal in interface %A, %A" signal.GetType signal

        /// Convert an internal representation of a marker route into a machine representation.
        let private markerString = function
            | RouteMarker1 -> "M1"
            | RouteMarker2 -> "M2"
            | RouteMarker3 -> "M3"
            | RouteMarker4 -> "M4"

        /// Convert an internal representation of the AUX-29 pin into a machine representation.
        let private auxString = function
            | RouteAux29 -> "AUX29"

        /// Convert an internal representation of a user output route into a machine representation.
        let private userOutputSignalString (signal : IUserOutputSignal) =
            match signal with
            | :? UserOutputSignalMarker as signal -> markerString signal
            | :? UserOutputSignalAux as signal -> auxString signal
            | _ -> failIncorrectType signal

        /// Convert a machine representation of a user output route into an internal representation.
        let parseUserOutputSignal str =
            match String.toUpper str with
            | "M1"    -> RouteMarker1   :> IUserOutputSignal
            | "M2"    -> RouteMarker2   :> IUserOutputSignal
            | "M3"    -> RouteMarker3   :> IUserOutputSignal
            | "M4"    -> RouteMarker4   :> IUserOutputSignal
            | "AUX29" -> RouteAux29          :> IUserOutputSignal
            | "NONE"  -> NoOutputSignal :> IUserOutputSignal
            | _ -> failwithf "Unexpected user output signal string: %s" str

        /// Convert an internal representation of a common system routing into a machine representation.
        let private systemOutputSignalCommonString = function
            | RouteSweepOut -> "SWEEP"
            | RouteSourceSettled -> "SETTLED"
            | RoutePulseVideo -> "PVIDEO"
            | RoutePulseSync -> "PSYNC"
            | RouteSweptFunctionDone -> "SFDONE"

        /// Convert an internal representation of a sweep out-only routing into a machine representation.
        let private systemOutputSignalSweepOutString = function
            | RouteSweepRun -> "SRUN"

        /// Convert an internal representation of a sweep out routing into a machine representation.
        let private sweepOutSignalString (signal : ISweepOutSignal) =
            match signal with
            | :? SystemOutputSignalCommon as signal -> systemOutputSignalCommonString signal
            | :? SystemOutputSignalSweepOut as signal -> systemOutputSignalSweepOutString signal
            | _ -> failIncorrectType signal

        /// Convert an internal representation of a trigger-only routing into a machine representation.
        let private systemOutputSignalTriggerString = function
            | RouteLxi -> "LXI"
            | RoutePulseBnc -> "PULSE"
            | RouteOtherTrigger -> "TRIG"

        /// Convert an internal representation of a trigger routing into a machine representation.
        let private triggerSignalString (signal : ITriggerSignal) =
            match signal with
            | :? SystemOutputSignalCommon as signal -> systemOutputSignalCommonString signal
            | :? SystemOutputSignalTrigger as signal -> systemOutputSignalTriggerString signal
            | _ -> failIncorrectType signal

        /// Convert a machine representation of a sweep out routing into an internal representation.
        let parseSweepOutSignal str =
            match String.toUpper str with
            | "SWE" | "SWEEP"     -> RouteSweepOut          :> ISweepOutSignal
            | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ISweepOutSignal
            | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ISweepOutSignal
            | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ISweepOutSignal
            | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ISweepOutSignal
            | "SRUN"              -> RouteSweepRun          :> ISweepOutSignal
            | "NONE"              -> NoOutputSignal         :> ISweepOutSignal
            | _ -> failwithf "Unexpected sweep out signal string: %s" str

        /// Convert a machine representation of a trigger routing into an internal representation.
        let parseTriggerSignal str =
            match String.toUpper str with
            | "SWE" | "SWEEP"     -> RouteSweepOut          :> ITriggerSignal
            | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ITriggerSignal
            | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ITriggerSignal
            | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ITriggerSignal
            | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ITriggerSignal
            | "LXI"               -> RouteLxi               :> ITriggerSignal
            | "PULS" | "PULSE"    -> RoutePulseBnc          :> ITriggerSignal
            | "TRIG" | "TRIGGER1" | "TRIGGER2"
                                  -> RouteOtherTrigger      :> ITriggerSignal
            | "NONE"              -> NoOutputSignal         :> ITriggerSignal
            | _ -> failwithf "Unexpected trigger signal string: %s" str

        /// Convert an internal representation of an output routing into a machine representation.
        let outputSignalString (signal : IOutputSignal) =
            match signal with
            | :? NoOutputSignal              -> "NONE"
            | :? IUserOutputSignal as signal -> userOutputSignalString signal
            | :? ISweepOutSignal   as signal -> sweepOutSignalString signal
            | :? ITriggerSignal    as signal -> triggerSignalString signal
            | _ -> failIncorrectType signal

    [<AutoOpen>]
    module Control =
        /// Key to control the output routing of the BBTRIG 1 BNC.
        /// Command reference p.164.
        let private basebandTrigger1Key = ":ROUTE:CONNECTORS:BBTRIGGER1"
        /// Key to control the output routing of the BBTRIG 2 BNC.
        /// Command reference p.164.
        let private basebandTrigger2Key = ":ROUTE:CONNECTORS:BBTRIGGER2"

        /// Key to control the output routing of the EVENT 1 BNC.
        /// Command reference p.158.
        let private event1Key = ":ROUTE:CONNECTORS:EVENT"

        /// Key to control the output routing of the PAT TRIG BNC.
        /// Command reference p.164.
        let private patternTriggerKey = ":ROUTE:CONNECTORS:PTRIG"

        /// Key to control the output routing of the SWEEP OUT BNC.
        /// Command reference p.165.
        let private sweepOutKey = ":ROUTE:CONNECTORS:SOUT"

        /// Key to control the output routing of the TRIG 1 BNC.
        /// Command reference p.165.
        let private trigger1Key = ":ROUTE:CONNECTORS:TRIGGER1:OUTPUT"
        /// Key to control the output routing of the TRIG 2 BNC.
        /// Command reference p.165.
        let private trigger2Key = ":ROUTE:CONNECTORS:TRIGGER2:OUTPUT"

        /// Set a single output routing on the machine.
        let private setSingleOutputRoute key instrument route =
            IO.setValueString outputSignalString key instrument route

        /// Query a key for an IUserOutputSignal in internal representation.
        let private queryUserOutputSignal = IO.queryKeyString parseUserOutputSignal

        /// Query a key for an ISweepOutSignal in internal representation.
        let private querySweepOutSignal = IO.queryKeyString parseSweepOutSignal

        /// Query a key for an ITriggerSignal in internal representation.
        let private queryTriggerSignal = IO.queryKeyString parseTriggerSignal

        /// Set all the available output routes to the given values.
        let setOutputRouting instrument routing = asyncChoice {
            do! setSingleOutputRoute basebandTrigger1Key instrument routing.BbTrig1
            do! setSingleOutputRoute basebandTrigger2Key instrument routing.BbTrig2
            do! setSingleOutputRoute event1Key instrument routing.Event1
            do! setSingleOutputRoute patternTriggerKey instrument routing.PatTrig
            do! setSingleOutputRoute sweepOutKey instrument routing.SweepOut
            do! setSingleOutputRoute trigger1Key instrument routing.Trig1
            do! setSingleOutputRoute trigger2Key instrument routing.Trig2 }

        /// Query the machine for the currently setup output routing.
        let queryOutputRouting instrument = asyncChoice {
            let! bbTrig1  = queryUserOutputSignal basebandTrigger1Key instrument
            let! bbTrig2  = queryUserOutputSignal basebandTrigger2Key instrument
            let! event1   = queryUserOutputSignal event1Key instrument
            let! patTrig  = queryUserOutputSignal patternTriggerKey instrument
            let! sweepOut = querySweepOutSignal sweepOutKey instrument
            let! trig1    = queryTriggerSignal trigger1Key instrument
            let! trig2    = queryTriggerSignal trigger2Key instrument
            return {
                BbTrig1  = bbTrig1
                BbTrig2  = bbTrig2
                Event1   = event1
                PatTrig  = patTrig
                SweepOut = sweepOut
                Trig1    = trig1
                Trig2    = trig2 } }