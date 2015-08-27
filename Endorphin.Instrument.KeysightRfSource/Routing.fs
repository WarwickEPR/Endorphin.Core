namespace Endorphin.Instrument.Keysight

open ExtCore.Control

[<RequireQualifiedAccess>]
module Routing =
    [<AutoOpen>]
    module private Parsing =
        /// If a type claiming to provide an interface we're using is passed, but we don't know about
        /// it, we have to fail execution.
        let failIncorrectType signal =
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

        /// Convert an internal representation of a user BNC into a machine representation.
        let private userBncString = function
            | RouteBasebandTrigger1 -> "BBTRIGGER1"
            | RouteBasebandTrigger2 -> "BBTRIGGER2"
            | RouteEvent1  -> "EVENT1"
            | RoutePatternTrigger -> "PTRIGGER"

        /// Convert an internal representation of a marker signal into a machine representation.
        let private markerSignalString (signal : IMarkerSignal) =
            match signal with
            | :? UserSignalMarker as signal -> markerString signal
            | _ -> failIncorrectType signal

        /// Convert an internal representation of a user output route into a machine representation.
        let private userSignalString (signal : IUserSignal) =
            match signal with
            | :? UserSignalMarker as signal -> markerString signal
            | :? UserSignalAux as signal -> auxString signal
            | _ -> failIncorrectType signal

        /// Convert an internal representation of a user BNC signal route into a machine representation.
        let private userBncSignalString (signal : IUserBncSignal) =
            match signal with
            | :? UserBnc as signal -> userBncString signal
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

        /// Convert a machine representation of a user BNC routing into an internal representation.
        let parseUserBncSignal str =
            match String.toUpper str with
            | "BBTR" | "BBTR1" | "BBTRIGGER" | "BBTRIGGER1"
                                     -> RouteBasebandTrigger1 :> IUserBncSignal
            | "BBTR2" | "BBTRIGGER2" -> RouteBasebandTrigger2 :> IUserBncSignal
            | "EVEN" | "EVEN1" | "EVENT" | "EVENT1"
                                     -> RouteEvent1  :> IUserBncSignal
            | "PTR" | "PTRIGGER"     -> RoutePatternTrigger :> IUserBncSignal
            | "NONE"                 -> NoSignal     :> IUserBncSignal
            | _ -> failwithf "Unexpected user BNC signal string: %s" str

        /// Convert an internal representation of an output routing into a machine representation.
        let signalString (signal : ISignal) =
            match signal with
            | :? NoSignal                  -> "NONE"
            | :? IUserSignal     as signal -> userSignalString signal
            | :? ISweepOutSignal as signal -> sweepOutSignalString signal
            | :? ITriggerSignal  as signal -> triggerSignalString signal
            | :? IMarkerSignal   as signal -> markerSignalString signal
            | :? IUserBncSignal  as signal -> userBncSignalString signal
            | _ -> failIncorrectType signal

    /// The default output routing that the machine would use after a *RST command.
    let private defaultOutputRouting = {
        BbTrig1  = RouteMarker2
        BbTrig2  = NoSignal
        Event1   = RouteMarker1
        PatTrig  = NoSignal
        SweepOut = RouteSweepOut
        Trig1    = NoSignal
        Trig2    = RouteSweepTriggerOut }

    /// The default input routing that the machine would use after a *RST command.
    let private defaultInputRouting = {
        PatTrig1 = RoutePatternTrigger
        PatTrig2 = RoutePatternTrigger }

    /// The default internal routing that the machine would use after a *RST command.
    let private defaultInternalRouting = {
        AltAmplitude = NoSignal
        AlcHold = NoSignal
        RfBlank = NoSignal }

    /// Default polarities of the marker channels after a *RST command.
    let private defaultMarkerPolarities = {
        PolarityM1 = Positive
        PolarityM2 = Positive
        PolarityM3 = Positive
        PolarityM4 = Positive }

    /// The default routings for the machine, in use after a *RST command.
    let empty = {
        Output           = defaultOutputRouting
        Input            = defaultInputRouting
        Internal         = defaultInternalRouting
        MarkerPolarities = defaultMarkerPolarities }

    /// If any inputs are set to "value", then unset them.  Otherwise, leave them be.
    let private unsetRequiredInputs value routing =
        let patTrig1 =
            if routing.Input.PatTrig1 = value then NoSignal :> IUserBncSignal
            else routing.Input.PatTrig1
        let patTrig2 =
            if routing.Input.PatTrig2 = value then NoSignal :> IUserBncSignal
            else routing.Input.PatTrig2
        { routing with Input = { PatTrig1 = patTrig1; PatTrig2 = patTrig2 } }

    /// Set the routing of the BBTRIG1 connector. Overwrites any inputs set to come into this BNC.
    let withBasebandTrigger1 value routing =
        let routing' = unsetRequiredInputs RouteBasebandTrigger1 routing
        { routing' with Output = { routing'.Output with BbTrig1 = value } }
    /// Set the routing of the BBTRIG2 connector. Overwrites any inputs set to come into this BNC.
    let withBasebandTrigger2 value routing =
        let routing' = unsetRequiredInputs RouteBasebandTrigger2 routing
        { routing' with Output = { routing'.Output with BbTrig2 = value } }

    /// Set the routing of the EVENT1 connector. Overwrites any inputs set to come into this BNC.
    let withEvent1 value routing =
        let routing' = unsetRequiredInputs RouteEvent1 routing
        { routing' with Output = { routing'.Output with Event1 = value } }

    /// Set the routing of the PATTRIG connector. Overwrites any inputs set to come into this BNC.
    let withPatternTrigger value routing =
        let routing' = unsetRequiredInputs RoutePatternTrigger routing
        { routing' with Output = { routing'.Output with PatTrig = value } }

    /// Set the routing of the SWEEPOUT connector.
    let withSweepOut value routing =
        { routing with Output = { routing.Output with SweepOut = value } }

    /// Set the routing of the TRIG1 connector.
    let withTrigger1 value routing =
        { routing with Output = { routing.Output with Trig1 = value } }
    /// Set the routing of the TRIG2 connector.
    let withTrigger2 value routing =
        { routing with Output = { routing.Output with Trig2 = value } }

    /// Set a given BNC to have no output signal.
    let private unsetBncOutput routing (value : IUserBncSignal) =
        match value with
        | :? NoSignal -> routing
        | :? UserBnc as value ->
            match value with
            | RouteBasebandTrigger1 -> withBasebandTrigger1 NoSignal routing
            | RouteBasebandTrigger2 -> withBasebandTrigger2 NoSignal routing
            | RouteEvent1           -> withEvent1 NoSignal routing
            | RoutePatternTrigger   -> withPatternTrigger NoSignal routing
        | _ -> failIncorrectType value

    /// Set the routing of the internal signal pattern trigger 1.  This overwrites any output
    /// signal on the given BNC.
    let withPatternTrigger1 value routing =
        let routing' = unsetBncOutput routing value
        { routing' with Input = { routing'.Input with PatTrig1 = value } }

    /// Set the routing of the internal signal pattern trigger 2.  This overwrites any output
    /// signal on the given BNC.
    let withPatternTrigger2 value routing =
        let routing' = unsetBncOutput routing value
        { routing' with Input = { routing'.Input with PatTrig2 = value } }

    /// Set the routing of the alternate amplitude function.
    let withAlternateAmplitude value routing =
        { routing with Internal = { routing.Internal with AltAmplitude = value } }

    /// Set the routing of the ALC hold function. This overwrites the RF blank function.
    let withAlcHold value routing =
        let rfBlank =
            if value = (NoSignal :> IMarkerSignal) then routing.Internal.RfBlank
            else NoSignal :> IMarkerSignal
        { routing with Internal = { routing.Internal with AlcHold = value; RfBlank = rfBlank } }

    /// Set the routing of the RF blanking function. This overwrites the ALC hold function.
    let withRfBlank value routing =
        let alcHold =
            if value = (NoSignal :> IMarkerSignal) then routing.Internal.AlcHold
            else NoSignal :> IMarkerSignal
        { routing with Internal = { routing.Internal with RfBlank = value; AlcHold = alcHold } }

    /// Set the polarity of marker 1.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker1Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM1 = value; } }

    /// Set the polarity of marker 2.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker2Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM2 = value; } }

    /// Set the polarity of marker 3.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker3Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM3 = value; } }

    /// Set the polarity of marker 4.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker4Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM4 = value; } }

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

    /// Key to control the input routing of pattern trigger 1.
    /// Command reference p.163.
    let private patternTrigger1Key = ":ROUTE:LINE:PTRIGGER1:BNC:SOURCE"

    /// Key to control the input routing of pattern trigger 2.
    /// Command reference p.163.
    let private patternTrigger2Key = ":ROUTE:LINE:PTRIGGER2:BNC:SOURCE"

    /// Key to control the routing of the alternate amplitude function.
    /// Command reference p.329.
    let private alternateAmplitudeKey = ":RAD:ARB:MDES:AAMP"

    /// Key to control the routing of the ALC hold function.
    /// Command reference p.329.
    let private alcHoldKey = ":RAD:ARB:MDES:ALCH"

    /// Key to control the routing of the RF blanking function.  This automatically sets the ALC
    /// hold too, so sending both separately just uses the RF blanking one.
    let private rfBlankKey = ":RAD:ARB:MDES:PULS"

    /// Key to set the polarity of a given marker.
    let private markerPolarityKey = ":RAD:ARB:MPOL"

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

    /// Query a key for an IUserBncSignal in internal representation.
    let private queryUserBncSignal = IO.queryKeyString parseUserBncSignal

    /// Make the specific marker polarity key for a given marker.
    let private makeMarkerPolarityKey marker =
        sprintf "%s:%s" markerPolarityKey (
            match marker with
            | RouteMarker1 -> "MARK1"
            | RouteMarker2 -> "MARK2"
            | RouteMarker3 -> "MARK3"
            | RouteMarker4 -> "MARK4" )

    /// Set the polarity of a single marker.
    let internal setMarkerPolarity marker instrument value =
        IO.setPolarity (makeMarkerPolarityKey marker) instrument value

    /// Query the polarity of a single marker.
    let internal queryMarkerPolarity marker instrument =
        IO.queryPolarity (makeMarkerPolarityKey marker) instrument

    /// Set the routing of the RF blank pulse channel.  This single function is only for use
    /// internally in Endorphin, since we need to set a marker to be the RF blanking pulse.
    let internal setRfBlankRoute instrument route = setSignalRoute rfBlankKey instrument route

    /// Set all the available output routes to the given values.
    let private setOutputRouting instrument routing = asyncChoice {
        do! setSignalRoute basebandTrigger1Key instrument routing.BbTrig1
        do! setSignalRoute basebandTrigger2Key instrument routing.BbTrig2
        do! setSignalRoute event1Key instrument routing.Event1
        do! setSignalRoute patternTriggerKey instrument routing.PatTrig
        do! setSignalRoute sweepOutKey instrument routing.SweepOut
        do! setSignalRoute trigger1Key instrument routing.Trig1
        do! setSignalRoute trigger2Key instrument routing.Trig2 }

    /// Set all the available input routes to the given values.
    let private setInputRouting instrument routing = asyncChoice {
        do! setSignalRoute patternTrigger1Key instrument routing.PatTrig1
        do! setSignalRoute patternTrigger2Key instrument routing.PatTrig2 }

    /// Set all the available internal routes to the given values.
    let private setInternalRouting instrument routing = asyncChoice {
        do! setSignalRoute alternateAmplitudeKey instrument routing.AltAmplitude
        do! setSignalRoute alcHoldKey instrument routing.AlcHold
        do! setSignalRoute rfBlankKey instrument routing.RfBlank }

    /// Set the polarities of the markers.
    let private setMarkerPolarities instrument routing = asyncChoice {
        do! setMarkerPolarity RouteMarker1 instrument routing.PolarityM1
        do! setMarkerPolarity RouteMarker2 instrument routing.PolarityM2
        do! setMarkerPolarity RouteMarker3 instrument routing.PolarityM3
        do! setMarkerPolarity RouteMarker4 instrument routing.PolarityM4 }

    /// Set all the routings for the machine to the given values.
    let set instrument routing = asyncChoice {
        do! setOutputRouting instrument routing.Output
        do! setInputRouting instrument routing.Input
        do! setInternalRouting instrument routing.Internal
        do! setMarkerPolarities instrument routing.MarkerPolarities }

    /// Query the machine for the currently setup output routing.
    let private queryOutputRouting instrument = asyncChoice {
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

    /// Query the machine for the currently setup input routing.
    let private queryInputRouting instrument = asyncChoice {
        let! patTrig1 = queryUserBncSignal patternTrigger1Key instrument
        let! patTrig2 = queryUserBncSignal patternTrigger2Key instrument
        return {
            PatTrig1 = patTrig1
            PatTrig2 = patTrig2 } }

    /// Query the machine for the currently setup internal routing.
    let private queryInternalRouting instrument = asyncChoice {
        let! altAmp  = queryMarkerSignal alternateAmplitudeKey instrument
        let! alcHold = queryMarkerSignal alcHoldKey instrument
        let! rfBlank = queryMarkerSignal rfBlankKey instrument
        return {
            AltAmplitude = altAmp
            AlcHold      = alcHold
            RfBlank      = rfBlank } }

    /// Query the polarities of the marker signals.
    let private queryMarkerPolarities instrument = asyncChoice {
        let! m1 = queryMarkerPolarity RouteMarker1 instrument
        let! m2 = queryMarkerPolarity RouteMarker2 instrument
        let! m3 = queryMarkerPolarity RouteMarker3 instrument
        let! m4 = queryMarkerPolarity RouteMarker4 instrument
        return {
            PolarityM1 = m1
            PolarityM2 = m2
            PolarityM3 = m3
            PolarityM4 = m4 } }

    /// Query the machine for the currently setup routings.
    let query instrument = asyncChoice {
        let! output = queryOutputRouting instrument
        let! input = queryInputRouting instrument
        let! internal' = queryInternalRouting instrument
        let! marker = queryMarkerPolarities instrument
        return {
            Output = output
            Input = input
            Internal = internal'
            MarkerPolarities = marker } }