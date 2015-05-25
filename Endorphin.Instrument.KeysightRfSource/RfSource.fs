namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.NationalInstruments

[<RequireQualifiedAccess>]
module RfSource =
    let openInstrument visaAddress timeout = asyncChoice { 
        let rfSource = RfSource <| Visa.openInstrument visaAddress timeout 
        do! IO.verifyIdentity rfSource
        let! __ = IO.queryErrorQueue rfSource // clear the error queue before doing anything
        return rfSource }

    let closeInstrument (RfSource rfSource) = Visa.closeInstrument rfSource

    let queryIdentity = IO.queryIdentity
    let queryNextErrorInQueue = IO.queryNextErrorInQueue
    let queryErrorQueue = IO.queryErrorQueue

    let private outputStateKey = ":OUTPUT:STATE"
    let setOutputState = IO.setOnOffState outputStateKey
    let queryOutputState = IO.queryOnOffState outputStateKey
        
    let private modulationStateKey = ":OUTPUT:MODULATION"
    let setModulationState = IO.setOnOffState modulationStateKey
    let queryModulationState = IO.queryOnOffState modulationStateKey

    module Source =
        let private sourceKey key prefix src = sprintf "%s:%s:%s" prefix src key

        module Function =
            let private functionKey key prefix fg = sourceKey key prefix (sourceString fg)

            let private shapeKey = ":SHAPE"
            let private setFunctionShapeType prefix fg = IO.setFunctionShape (functionKey shapeKey prefix fg)
            let private queryFunctionShapeType prefix fg = IO.queryFunctionShape (functionKey shapeKey prefix fg)

            let private rampPolarityKey = ":SHAPE:RAMP"
            let private setRampPolarity prefix fg = IO.setPolarity (functionKey rampPolarityKey prefix fg)
            let private queryRampPolarity prefix fg = IO.queryPolarity (functionKey rampPolarityKey prefix fg)

            let internal setFunctionShape prefix fg rfSource (shape : FunctionShape) = asyncChoice {
                do! setFunctionShapeType prefix fg rfSource shape
                match shape with
                    | Ramp polarity ->
                            do! setRampPolarity prefix fg rfSource polarity
                    | _ -> () }

            let internal queryFunctionShape prefix fg rfSource = asyncChoice {
                let! shapeT = queryFunctionShapeType prefix fg rfSource
                match shapeT with
                    | SineType -> return Sine
                    | TriangleType -> return Triangle
                    | SquareType -> return Square
                    | RampType ->
                    let! polarity = queryRampPolarity prefix fg rfSource
                    return (Ramp polarity) }

            let private frequencyKey = ":FREQUENCY"
            let internal setFrequency prefix fg = IO.setFrequency (functionKey frequencyKey prefix fg)
            let internal queryFrequency prefix fg = IO.queryFrequency (functionKey frequencyKey prefix fg)

        module External =
            let private externalKey key prefix src = sourceKey key prefix (sourceString src)

            let private couplingKey = ":COUPLING"
            let internal setCoupling prefix src = IO.setCoupling (externalKey couplingKey prefix src)
            let internal queryCoupling prefix src = IO.queryCoupling (externalKey couplingKey prefix src)

            let private impedanceKey = ":IMPEDANCE"
            let internal setImpedance prefix src = IO.setImpedance (externalKey impedanceKey prefix src)
            let internal queryImpedance prefix src = IO.queryImpedance (externalKey impedanceKey prefix src)


    module Modulation =
        let private prefixMap pathStringMapping path = sprintf ":%s:%s" (pathStringMapping path)

        module Amplitude =
            let private modulationPrefix path key = sprintf ":%s:%s" (``AM Path String`` path) key

            let private stateKey path = modulationPrefix path ":STATE"
            let setState path = IO.setOnOffState (stateKey path)
            let queryState path = IO.queryOnOffState (stateKey path)

            let private sourceKey path = modulationPrefix path ":SOURCE" 
            let setSource path = IO.setModulationSource (sourceKey path)
            let internal querySource path = IO.queryModulationSource (sourceKey path)

            let private typeKey path = modulationPrefix path ":TYPE"
            let internal setType path = IO.setAmplitudeModulationType (typeKey path)
            let internal queryType path = IO.queryAmplitudeModulationType (typeKey path)

            let private depthLinearKey path = modulationPrefix path ":DEPTH"
            let setDepthLinear path = IO.setPercentage (depthLinearKey path)
            let queryDepthLinear path = IO.queryPercentage (depthLinearKey path)

            let private depthExponentialKey path = modulationPrefix path ":DEPTH:EXPONENTIAL"
            let setDepthExponential path = IO.setDecibelRatio (depthExponentialKey path)
            let queryDepthExponential path = IO.queryDecibelRatio (depthExponentialKey path)

            module External =
                let private externalPrefix path = modulationPrefix path ":EXTERNAL"

                let setCoupling path src = Source.External.setCoupling (externalPrefix path) src
                let queryCoupling path src = Source.External.queryCoupling (externalPrefix path) src

                let setImpedance path src = Source.External.setImpedance (externalPrefix path) src
                let queryImpedance path src = Source.External.queryImpedance (externalPrefix path) src

            module Internal =
                let private internalPrefix path = modulationPrefix path ":INTERNAL"

                let setFunctionShape path fg = Source.Function.setFunctionShape (internalPrefix path) fg
                let queryFunctionShape path fg = Source.Function.queryFunctionShape (internalPrefix path) fg

                let setFunctionFrequency path fg = Source.Function.setFrequency (internalPrefix path) fg
                let queryFunctionFrequency path fg = Source.Function.queryFrequency (internalPrefix path) fg

        module Frequency =

            let private modulationPrefix path key = sprintf ":%s:%s" (``FM Path String`` path) key

            let private stateKey path = modulationPrefix path ":STATE"
            let setState path = IO.setOnOffState (stateKey path)
            let queryState path = IO.queryOnOffState (stateKey path)

            let private sourceKey path = modulationPrefix path ":SOURCE" 
            let setSource path = IO.setModulationSource (sourceKey path)
            let internal querySource path = IO.queryModulationSource (sourceKey path)

            let private deviationKey path = modulationPrefix path ":DEVIATION"
            let setDepthLinear path = IO.setFrequency (deviationKey path)
            let queryDepthLinear path = IO.queryFrequency (deviationKey path)

            module External =
                let private externalPrefix path = modulationPrefix path ":EXTERNAL"

                let setCoupling path src = Source.External.setCoupling (externalPrefix path) src
                let queryCoupling path src = Source.External.queryCoupling (externalPrefix path) src

                let setImpedance path src = Source.External.setImpedance (externalPrefix path) src
                let queryImpedance path src = Source.External.queryImpedance (externalPrefix path) src

            module Internal =
                let private internalPrefix path = modulationPrefix path ":INTERNAL"

                let setFunctionShape path fg = Source.Function.setFunctionShape (internalPrefix path) fg
                let queryFunctionShape path fg = Source.Function.queryFunctionShape (internalPrefix path) fg

                let setFunctionFrequency path fg = Source.Function.setFrequency (internalPrefix path) fg
                let queryFunctionFrequency path fg = Source.Function.queryFrequency (internalPrefix path) fg

    module Frequency =
        let private cwFrequencyKey = ":FREQUENCY"
        let setCwFrequency = IO.setFrequency cwFrequencyKey
        let queryCwFrequency = IO.queryFrequency cwFrequencyKey

        let private startFrequencyKey = ":FREQUENCY:START"
        let setStartFrequency = IO.setFrequency startFrequencyKey
        let queryStartFrequncy = IO.queryFrequency startFrequencyKey

        let private stopFrequencyKey = ":FREQUENCY:STOP"
        let setStopFrequency = IO.setFrequency stopFrequencyKey
        let queryStopFrequency = IO.queryFrequency stopFrequencyKey

        let private frequencySpanKey = ":FREQUENCY:SPAN"
        let setFrequencySpan = IO.setFrequency frequencySpanKey
        let queryFrequencySpan = IO.queryFrequency frequencySpanKey

        let private sweepModeKey = ":FREQUENCY:MODE"
        let setSweepMode = IO.setSweepMode sweepModeKey
        let querySweepMode = IO.querySweepMode sweepModeKey

        let private phaseKey = ":PHASE"
        let setPhase = IO.setPhase phaseKey
        let queryPhase = IO.queryPhase phaseKey

    module Amplitude =
        let private cwAmplitudeKey = ":POWER"
        let setCwAmplitude = IO.setAmplitude cwAmplitudeKey
        let queryCwAmplitude = IO.queryAmplitude cwAmplitudeKey

        let private startAmplitudeKey = ":POWER:START"
        let setStartAmplitude = IO.setAmplitude startAmplitudeKey
        let queryStartAmplitude = IO.queryAmplitude startAmplitudeKey

        let private stopAmplitudeKey = ":POWER:STOP"
        let setStopAmplitude = IO.setAmplitude stopAmplitudeKey
        let queryStopAmplitude = IO.queryAmplitude stopAmplitudeKey

        let private sweepModeKey = ":POWER:MODE"
        let setSweepMode = IO.setSweepMode sweepModeKey
        let querySweepMode = IO.querySweepMode sweepModeKey

    module Triggering =
        let private sourceTypeKey trigger = sprintf "%s:TRIGGER:SOURCE" (triggerTypePrefix trigger)
        let setSourceType trigger = IO.setTriggerSourceType (sourceTypeKey trigger)
        let querySourceType trigger = IO.queryTriggerSourceType (sourceTypeKey trigger)

        let private externalSourceKey trigger = sprintf "%s:TRIGGER:EXTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setExternalSource trigger = IO.setExternalTriggerSource (externalSourceKey trigger)
        let queryExternalSource trigger = IO.queryExternalTriggerSource (externalSourceKey trigger)

        let private externalSlopePolarityKey trigger = sprintf "%s:TRIGGER:SLOPE" (triggerTypePrefix trigger)
        let setExternalSlopePolarity trigger = IO.setPolarity (externalSlopePolarityKey trigger)
        let queryExternalSlopePolarity trigger = IO.queryPolarity (externalSlopePolarityKey trigger)

        let private internalSourceKey trigger = sprintf "%s:TRIGGER:INTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setInternalSource trigger = IO.setInternalTriggerSource (internalSourceKey trigger)
        let queryInternalSource trigger = IO.queryInternalTriggerSource (internalSourceKey trigger)

        let private timerPeriodKey trigger = sprintf "%s:TRIGGER:TIMER" (triggerTypePrefix trigger)
        let setTimerPeriod trigger = IO.setDuration (timerPeriodKey trigger)
        let queryTimerPeriod trigger = IO.queryDuration (timerPeriodKey trigger)

        let setTriggerSource trigger rfSource triggerSource = asyncChoice {
            match triggerSource with
            | Immediate  -> do! setSourceType trigger rfSource ImmediateType
            | TriggerKey -> do! setSourceType trigger rfSource TriggerKeyType
            | Bus        -> do! setSourceType trigger rfSource BusType
            | TriggerSource.External (source, polarity) ->
                do! setSourceType trigger rfSource ExternalType
                do! setExternalSource trigger rfSource source
                do! setExternalSlopePolarity trigger rfSource polarity
            | Internal source ->
                do! setSourceType trigger rfSource InternalType
                do! setInternalSource trigger rfSource source
            | Timer period ->
                do! setSourceType trigger rfSource TimerType
                do! setTimerPeriod trigger rfSource period }

    module Sweep =
        let private typeKey = ":LIST:TYPE"
        /// set List or Step sweep
        let setSweepType = IO.setSweepType typeKey
        let querySweepType = IO.querySweepType typeKey

        let private directionKey = ":LIST:DIRECTION"
        /// Set direction of sweep from start -> stop for Up and stop -> start for Down
        let setDirection = IO.setDirection directionKey
        let queryDirection = IO.queryDirection directionKey

        let private continuousModeKey = ":INITIATE:CONTINUOUS"
        /// If On, rearms after each sweep. With immediate triggering, runs continuously
        let setContinousMode = IO.setOnOffState continuousModeKey
        let queryContinuousMode = IO.queryOnOffState continuousModeKey

        let private modeKey = ":LIST:MODE"
        /// Sets manual or automatic mode for progressing through points of a sweep
        let setMode = IO.setAutoManualState modeKey
        let queryMode = IO.queryAutoManualState modeKey

        let private dwellTimeKey = ":SWEEP:DWELL"
        /// Sets time to stay at a point once output has settled
        let setDwellTime = IO.setDuration dwellTimeKey
        let queryDwellTime = IO.queryDuration dwellTimeKey

        let private attenuationProtectionKey = ":SWEEP:ATTEN:PROTECTION"
        /// Turns on Atten Hold during sweeps. Off applies a minimum dwell time, but can apply ALC at each point  
        let setAttenuationProtection = IO.setOnOffState attenuationProtectionKey
        let queryAttenuationProtection = IO.queryOnOffState attenuationProtectionKey

        let private retraceKey = ":LIST:RETRACE"
        /// sets whether to fly back to start at the end of a single sweep
        let setRetrace = IO.setOnOffState retraceKey
        let queryRetrace = IO.queryOnOffState retraceKey

        let private setSweepOptions rfSource options = asyncChoice {
            do! setDirection rfSource options.Direction
            do! Triggering.setTriggerSource StepTrigger rfSource options.StepTrigger
            do! Triggering.setTriggerSource ListTrigger rfSource options.ListTrigger
            match options.DwellTime with
             | Some t -> do! setDwellTime rfSource t
             | None   -> if options.ListTrigger == Immediate
                         then return! fail "Dwell time required for free-running, immediate trigger sweep through a list of points"
            do! setRetrace rfSource options.Retrace 
            do! setAttenuationProtection rfSource options.AttentuationProtection
            do! setMode rfSource options.Mode }

        module Step =
            let private stepSpacingKey = ":SWEEP:SPACING"
            /// Choose linear or logarithmic spacing for regular step sweep
            let setStepSpacing = IO.setStepSpacing stepSpacingKey
            let queryStepSpacing = IO.queryStepSpacing stepSpacingKey

            let private pointsCountKey = ":SWEEP:POINTS"
            /// Set number of points to have in sweep. 2 to 65535
            let setPointsCount = IO.setInt pointsCountKey
            let queryPointsCount = IO.queryInt pointsCountKey

            /// set frequency / frequency range for sweep
            let setFrequencySweep rfSource frequencySweep = asyncChoice {
                match frequencySweep with
                | FixedFrequency f ->
                    do! Frequency.setCwFrequency rfSource f
                    do! Frequency.setSweepMode rfSource Fixed
                | FrequencySweep sweep ->
                    do! Frequency.setStartFrequency rfSource sweep.Start
                    do! Frequency.setStopFrequency rfSource sweep.Stop
                    do! Frequency.setSweepMode rfSource Swept }

            /// set amplitude / amplitude range for sweep
            let setAmplitudeSweep rfSource amplitudeSweep = asyncChoice {
                match amplitudeSweep with
                | FixedAmplitude a ->
                    do! Amplitude.setCwAmplitude rfSource a
                    do! Amplitude.setSweepMode rfSource Fixed
                | AmplitudeSweep sweep ->
                    do! Amplitude.setStartAmplitude rfSource sweep.Start
                    do! Amplitude.setStopAmplitude rfSource sweep.Stop
                    do! Amplitude.setSweepMode rfSource Swept }

            /// Set up an RF step sweep from a model
            let setup rfSource (stepSweep : StepSweep) = asyncChoice {
                do! setFrequencySweep rfSource stepSweep.Frequency
                do! setAmplitudeSweep rfSource stepSweep.Amplitude
                do! setPointsCount rfSource stepSweep.Points
                do! setStepSpacing rfSource stepSweep.Spacing
                do! setSweepOptions rfSource stepSweep.Options
                do! setSweepType rfSource Step }

        module List =
            let private dwellTimesKey = ":LIST:DWELL"
            let setDwellTimes key = IO.setDurationSeq dwellTimesKey key
            let queryDwellTimes = IO.queryDurationSeq dwellTimesKey

            let private dwellTimesCountKey = ":LIST:DWELL:POINTS"
            let queryDwellTimesCount = IO.queryInt dwellTimesCountKey

            let private dwellTypeKey = ":LIST:DWELL:TYPE"
            let setDwellType = IO.setSweepMode dwellTypeKey
            let queryDwellType = IO.querySweepMode dwellTypeKey

            let private powersKey = ":LIST:POWER"
            let setPowers key = IO.setAmplitudeSeq powersKey key
            let queryPowers = IO.queryAmplitudeSeq powersKey

            let private powersCountKey = ":LIST:POWER:POINTS"
            let queryPowersCount = IO.queryInt powersCountKey

            let private frequenciesKey = ":LIST:FREQUENCY"
            let setFrequencies key = IO.setFrequencySeq frequenciesKey key
            let queryFrequencies = IO.queryFrequencySeq frequenciesKey

            let private frequenciesCountKey = ":LIST:FREQUENCY:POINTS"
            let queryFrequenciesCount = IO.queryInt frequenciesCountKey

            let private waveformsKey = ":LIST:WAVEFORM"
            let setWaveforms key = IO.setWaveformSeq waveformsKey key
            let queryWaveforms = IO.queryWaveformSeq waveformsKey

            let private waveformSweepModeKey = ":RADIO:LARB"
            let setWaveformSweepMode = IO.setOnOffState waveformSweepModeKey
            let queryWaveformSweepMode = IO.queryOnOffState waveformSweepModeKey

            let private waveformsCountKey = ":LIST:WAVEFORM:POINTS"
            let queryWaveformsCount = IO.queryInt waveformsCountKey

        /// Commands used to control running sweeps
        module Run = 
            /// Starts an armed sweep waiting on Bus triggering
            let busTrigger = IO.postCommand "*TRG"
            
            let private immediateKey = ":TRIGGER"
            /// Starts an armed sweep immediately without waiting for selected trigger event
            let immediate = IO.postCommand immediateKey

            let private currentPointKey = ":SWEEP:CPOINT"
            /// Reports current point in List/Step sequence
            let queryCurrentPoint = IO.queryInt currentPointKey

            /// Abort current sweep. In continous mode, arms a new sweep
            let private abortKey = ":ABORT"
            let abort = IO.postCommand abortKey

            /// Aborts current sweep and rearms. With immediate triggering starts a new sweep
            let abortAndRearmSingle = IO.postCommand ":TSWEEP"

            let private rearmSingleKey = ":INITIATE"
            /// Arms a single sweep. In immediate mode, starts immediately
            let rearmSingle = IO.postCommand rearmSingleKey

