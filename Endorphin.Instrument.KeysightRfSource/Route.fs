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

        /// Convert an internal representation of a marker signal into a machine representation.
        let private markerSignalString (signal : IMarkerSignal) =
            match signal with
            | :? UserSignalMarker as signal -> markerString signal
            | _ -> failIncorrectType signal

        /// Convert an internal representation of a user output route into a machine representation.
        let private userOutputSignalString (signal : IUserSignal) =
            match signal with
            | :? UserSignalMarker as signal -> markerString signal
            | :? UserSignalAux as signal -> auxString signal
            | _ -> failIncorrectType signal

        /// Convert a machine representation of a marker signal into an internal representation.
        let parseMarkerSignal str =
            match String.toUpper str with
            | "M1"   -> RouteMarker1 :> IMarkerSignal
            | "M2"   -> RouteMarker2 :> IMarkerSignal
            | "M3"   -> RouteMarker3 :> IMarkerSignal
            | "M4"   -> RouteMarker4 :> IMarkerSignal
            | "NONE" -> NoSignal     :> IMarkerSignal
            | _ -> failwithf "Unexpted marker signal string: %s" str

        /// Convert a machine representation of a user output route into an internal representation.
        let parseUserSignal str =
            match String.toUpper str with
            | "M1"    -> RouteMarker1 :> IUserSignal
            | "M2"    -> RouteMarker2 :> IUserSignal
            | "M3"    -> RouteMarker3 :> IUserSignal
            | "M4"    -> RouteMarker4 :> IUserSignal
            | "AUX29" -> RouteAux29   :> IUserSignal
            | "NONE"  -> NoSignal     :> IUserSignal
            | _ -> failwithf "Unexpected user output signal string: %s" str

        /// Convert an internal representation of a common system routing into a machine representation.
        let private systemSignalCommonString = function
            | RouteSourceSettled -> "SETTLED"
            | RoutePulseVideo -> "PVIDEO"
            | RoutePulseSync -> "PSYNC"
            | RouteSweptFunctionDone -> "SFDONE"

        /// Convert an internal representation of a sweep out-only routing into a machine representation.
        let private systemSignalSweepOutString = function
            | RouteSweepOut -> "SWEEP"
            | RouteSweepRun -> "SRUN"

        /// Convert an internal representation of a sweep out routing into a machine representation.
        let private sweepOutSignalString (signal : ISweepOutSignal) =
            match signal with
            | :? SystemSignalCommon as signal -> systemSignalCommonString signal
            | :? SystemSignalSweepOut as signal -> systemSignalSweepOutString signal
            | _ -> failIncorrectType signal

        /// Convert an internal representation of a trigger-only routing into a machine representation.
        let private systemSignalTriggerString = function
            | RouteSweepTriggerOut -> "SWEEP"
            | RouteLxi -> "LXI"
            | RoutePulseBnc -> "PULSE"
            | RouteOtherTrigger -> "TRIG"

        /// Convert an internal representation of a trigger routing into a machine representation.
        let private triggerSignalString (signal : ITriggerSignal) =
            match signal with
            | :? SystemSignalCommon as signal -> systemSignalCommonString signal
            | :? SystemSignalTrigger as signal -> systemSignalTriggerString signal
            | _ -> failIncorrectType signal

        /// Convert a machine representation of a sweep out routing into an internal representation.
        let parseSweepOutSignal str =
            match String.toUpper str with
            | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ISweepOutSignal
            | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ISweepOutSignal
            | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ISweepOutSignal
            | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ISweepOutSignal
            | "SWE" | "SWEEP"     -> RouteSweepOut          :> ISweepOutSignal
            | "SRUN"              -> RouteSweepRun          :> ISweepOutSignal
            | "NONE"              -> NoSignal               :> ISweepOutSignal
            | _ -> failwithf "Unexpected sweep out signal string: %s" str

        /// Convert a machine representation of a trigger routing into an internal representation.
        let parseTriggerSignal str =
            match String.toUpper str with
            | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ITriggerSignal
            | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ITriggerSignal
            | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ITriggerSignal
            | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ITriggerSignal
            | "SWE" | "SWEEP"     -> RouteSweepTriggerOut   :> ITriggerSignal
            | "LXI"               -> RouteLxi               :> ITriggerSignal
            | "PULS" | "PULSE"    -> RoutePulseBnc          :> ITriggerSignal
            | "TRIG" | "TRIGGER1" | "TRIGGER2"
                                  -> RouteOtherTrigger      :> ITriggerSignal
            | "NONE"              -> NoSignal               :> ITriggerSignal
            | _ -> failwithf "Unexpected trigger signal string: %s" str

        /// Convert an internal representation of an output routing into a machine representation.
        let signalString (signal : ISignal) =
            match signal with
            | :? NoSignal                    -> "NONE"
            | :? IUserSignal as signal -> userOutputSignalString signal
            | :? ISweepOutSignal   as signal -> sweepOutSignalString signal
            | :? ITriggerSignal    as signal -> triggerSignalString signal
            | :? IMarkerSignal     as signal -> markerSignalString signal
            | _ -> failIncorrectType signal

    [<AutoOpen>]
    module Configure =
        /// The default output routing that the machine would use after a *RST? command.
        let defaultOutputRouting = {
            BbTrig1  = RouteMarker2
            BbTrig2  = NoSignal
            Event1   = RouteMarker1
            PatTrig  = NoSignal
            SweepOut = RouteSweepOut
            Trig1    = NoSignal
            Trig2    = RouteSweepTriggerOut }

        /// The default internal routing that the machine would use after a *RST? command.
        let defaultInternalRouting = {
            AltAmplitude = NoSignal
            AlcHold = NoSignal
            RfBlank = NoSignal }

        /// Set the routing of the BBTRIG1 connector.
        let withBasebandTrigger1 value routing = { routing with BbTrig1 = value }
        /// Set the routing of the BBTRIG2 connector.
        let withBasebandTrigger2 value routing = { routing with BbTrig2 = value }

        /// Set the routing of the EVENT1 connector.
        let withEvent1 value routing = { routing with Event1 = value }

        /// Set the routing of the PATTRIG connector.
        let withPatternTrigger value routing = { routing with PatTrig = value }

        /// Set the routing of the SWEEPOUT connector.
        let withSweepOut value routing = { routing with SweepOut = value }

        /// Set the routing of the TRIG1 connector.
        let withTrigger1 value routing = { routing with Trig1 = value }
        /// Set the routing of the TRIG2 connector.
        let withTrigger2 value routing = { routing with Trig2 = value }

        /// Set the routing of the alternate amplitude function.
        let withAlternateAmplitude value routing = { routing with AltAmplitude = value }

        /// Set the routing of the ALC hold function.
        let withAlcHold value routing = { routing with AlcHold = value }

        /// Set the routing of the RF blanking function. This overwrites the ALC hold function.
        let withRfBlank value routing = { routing with RfBlank = value }

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

        /// Key to control the routing of the alternate amplitude function.
        /// Command reference p.329.
        let private alternateAmplitudeKey = ":RAD:ARB:MDES:AAMP"

        /// Key to control the routing of the ALC hold function.
        /// Command reference p.329.
        let private alcHoldKey = ":RAD:ARB:MDES:ALCH"

        /// Key to control the routing of the RF blanking function.  This automatically sets the ALC
        /// hold too, so sending both separately just uses the RF blanking one.
        let private rfBlankKey = ":RAD:ARB:MDES:PULS"

        /// Set a single output routing on the machine.
        let private setSignalRoute key instrument route =
            IO.setValueString signalString key instrument route

        /// Query a key for an IUserOutputSignal in internal representation.
        let private queryUserSignal = IO.queryKeyString parseUserSignal

        /// Query a key for an ISweepOutSignal in internal representation.
        let private querySweepOutSignal = IO.queryKeyString parseSweepOutSignal

        /// Query a key for an ITriggerSignal in internal representation.
        let private queryTriggerSignal = IO.queryKeyString parseTriggerSignal

        /// Query a key for an IMarkerSignal in internal representation.
        let private queryMarkerSignal = IO.queryKeyString parseMarkerSignal

        /// Set all the available output routes to the given values.
        let setOutputRouting instrument routing = asyncChoice {
            do! setSignalRoute basebandTrigger1Key instrument routing.BbTrig1
            do! setSignalRoute basebandTrigger2Key instrument routing.BbTrig2
            do! setSignalRoute event1Key instrument routing.Event1
            do! setSignalRoute patternTriggerKey instrument routing.PatTrig
            do! setSignalRoute sweepOutKey instrument routing.SweepOut
            do! setSignalRoute trigger1Key instrument routing.Trig1
            do! setSignalRoute trigger2Key instrument routing.Trig2 }

        /// Set all the available internal routes to the given values.
        let setInternalRouting instrument routing = asyncChoice {
            do! setSignalRoute alternateAmplitudeKey instrument routing.AltAmplitude
            do! setSignalRoute alcHoldKey instrument routing.AlcHold
            do! setSignalRoute rfBlankKey instrument routing.RfBlank }

        /// Query the machine for the currently setup output routing.
        let queryOutputRouting instrument = asyncChoice {
            let! bbTrig1  = queryUserSignal basebandTrigger1Key instrument
            let! bbTrig2  = queryUserSignal basebandTrigger2Key instrument
            let! event1   = queryUserSignal event1Key instrument
            let! patTrig  = queryUserSignal patternTriggerKey instrument
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

        /// Query the machine for the currently setup internal routing.
        let queryInternalRouting instrument = asyncChoice {
            let! altAmp  = queryMarkerSignal alternateAmplitudeKey instrument
            let! alcHold = queryMarkerSignal alcHoldKey instrument
            let! rfBlank = queryMarkerSignal rfBlankKey instrument
            return {
                AltAmplitude = altAmp
                AlcHold      = alcHold
                RfBlank      = rfBlank } }