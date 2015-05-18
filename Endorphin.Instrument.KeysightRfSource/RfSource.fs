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
        
    module AmplitudeModulation =
        let private stateKey path = sprintf ":AM%s:STATE" (modulationPathString path)
        let setState = IO.setValueForDerivedPath IO.setOnOffState stateKey
        let queryState = IO.queryValueForDerivedPath IO.queryOnOffState stateKey

        let private typeKey path = sprintf ":AM%s:TYPE" (modulationPathString path)
        let setType = IO.setValueForDerivedPath IO.setAmplitudeModulationType typeKey
        let queryType = IO.queryValueForDerivedPath IO.queryAmplitudeModulationType typeKey

        let private depthLinearKey path = sprintf ":AM%s:DEPTH" (modulationPathString path)
        let setDepthLinear = IO.setValueForDerivedPath IO.setPercentage depthLinearKey
        let queryDepthLinear = IO.queryValueForDerivedPath IO.queryPercentage depthLinearKey

        let private depthExponentialKey path = sprintf ":AM%s:DEPTH:EXPONENTIAL" (modulationPathString path)
        let setDepthExponential = IO.setValueForDerivedPath IO.setDecibelRatio depthExponentialKey
        let queryDepthExponential = IO.queryValueForDerivedPath IO.queryDecibelRatio depthExponentialKey

        let private sourceKey path = sprintf ":AM%s:SOURCE" (modulationPathString path)
        let setSource = IO.setValueForDerivedPath IO.setModulationSource sourceKey
        let querySource = IO.queryValueForDerivedPath IO.queryModulationSource sourceKey

        module External =
            let private couplingKey path = sprintf ":AM%s:EXTERNAL:COUPLING" (modulationPathString path)
            let setCoupling = IO.setValueForDerivedPath IO.setCoupling couplingKey
            let queryCoupling = IO.queryValueForDerivedPath IO.queryCoupling couplingKey

            let private impedanceKey path = sprintf ":AM%s:EXTERNAL:IMPEDANCE" (modulationPathString path)
            let setImpedance = IO.setValueForDerivedPath IO.setImpedance impedanceKey
            let queryImpedance = IO.queryValueForDerivedPath IO.queryImpedance impedanceKey

        module Internal =
            let private frequencyKey path = sprintf ":AM%s:INTERNAL:FREQUENCY" (modulationPathString path)
            let setFrequency = IO.setValueForDerivedPath IO.setFrequency frequencyKey
            let queryFrequency = IO.queryValueForDerivedPath IO.queryFrequency frequencyKey

            let private functionShapeKey path = sprintf ":AM%s:INTERNAL:FUNCTION:SHAPE" (modulationPathString path)
            let setFunctionShape = IO.setValueForDerivedPath IO.setFunctionShape functionShapeKey
            let queryFunctionShape = IO.queryValueForDerivedPath IO.queryFunctionShape functionShapeKey

            let private rampPolarityKey path = sprintf ":AM%s:INTERNAL:FUNCTION:SHAPE:RAMP" (modulationPathString path)
            let setRampPolarity = IO.setValueForDerivedPath IO.setPolarity rampPolarityKey
            let queryRampPolarity = IO.queryValueForDerivedPath IO.queryPolarity rampPolarityKey

    module FrequencyModulation =
        let private stateKey path = sprintf ":FM%s:STATE" (modulationPathString path)
        let setState = IO.setValueForDerivedPath  IO.setOnOffState stateKey
        let queryState = IO.queryValueForDerivedPath IO.queryOnOffState stateKey

        let private sourceKey path = sprintf ":FM%s:SOURCE" (modulationPathString path)
        let setSource = IO.setValueForDerivedPath IO.setModulationSource sourceKey
        let querySource = IO.queryValueForDerivedPath IO.queryModulationSource sourceKey

        let private deviationKey path = sprintf ":FM%s:DEVIATION" (modulationPathString path)
        let setDeviation = IO.setValueForDerivedPath IO.setFrequency deviationKey
        let queryDeviation = IO.queryValueForDerivedPath IO.queryFrequency deviationKey

        module External =
            let private couplingKey path = sprintf ":FM%s:EXTERNAL:COUPLING" (modulationPathString path)
            let setCoupling = IO.setValueForDerivedPath IO.setCoupling couplingKey
            let queryCoupling = IO.queryValueForDerivedPath IO.queryCoupling couplingKey

            let private impedanceKey path = sprintf ":FM%s:EXTERNAL:IMPEDANCE" (modulationPathString path)
            let setImpedance = IO.setValueForDerivedPath IO.setImpedance impedanceKey
            let queryImpedance = IO.queryValueForDerivedPath IO.queryImpedance impedanceKey

        module Internal =
            let private frequencyKey path = sprintf ":FM%s:INTERNAL:FREQUENCY" (modulationPathString path)
            let setFrequency = IO.setValueForDerivedPath IO.setFrequency frequencyKey
            let queryFrequency = IO.queryValueForDerivedPath IO.queryFrequency frequencyKey

            let private functionShapeKey path = sprintf ":FM%s:INTERNAL:FUNCTION:SHAPE" (modulationPathString path)
            let setFunctionShape = IO.setValueForDerivedPath IO.setFunctionShape functionShapeKey
            let queryFunctionShape = IO.queryValueForDerivedPath IO.queryFunctionShape functionShapeKey

            let private rampPolarityKey path = sprintf ":FM%s:INTERNAL:FUNCTION:SHAPE:RAMP" (modulationPathString path)
            let setRampPolarity = IO.setValueForDerivedPath IO.setPolarity rampPolarityKey
            let queryRampPolarity = IO.queryValueForDerivedPath IO.queryPolarity rampPolarityKey

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

    module Trigger =
        let private sourceTypeKey trigger = sprintf "%s:TRIGGER:SOURCE" (triggerTypePrefix trigger)
        let setSourceType = IO.setValueForDerivedPath IO.setTriggerSourceType sourceTypeKey
        let querySourceType = IO.queryValueForDerivedPath IO.queryTriggerSourceType sourceTypeKey

        let private externalSourceKey trigger = sprintf "%s:TRIGGER:EXTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setExternalSource = IO.setValueForDerivedPath IO.setExternalTriggerSource externalSourceKey
        let queryExternalSource = IO.queryValueForDerivedPath IO.queryExternalTriggerSource externalSourceKey

        let private externalSlopePolarityKey trigger = sprintf "%s:TRIGGER:SLOPE" (triggerTypePrefix trigger)
        let setExternalSlopePolarity = IO.setValueForDerivedPath IO.setPolarity externalSlopePolarityKey
        let queryExternalSlopePolarity = IO.queryValueForDerivedPath IO.queryPolarity externalSlopePolarityKey

        let private internalSourceKey trigger = sprintf "%s:TRIGGER:INTERNAL:SOURCE" (triggerTypePrefix trigger)
        let setInternalSource = IO.setValueForDerivedPath IO.setInternalTriggerSource internalSourceKey
        let queryInternalSource = IO.queryValueForDerivedPath IO.queryInternalTriggerSource internalSourceKey

        let private timerPeriodKey trigger = sprintf "%s:TRIGGER:TIMER" (triggerTypePrefix trigger)
        let setTimerPeriod = IO.setValueForDerivedPath IO.setDuration timerPeriodKey
        let queryTimerPeriod = IO.queryValueForDerivedPath IO.queryDuration timerPeriodKey

        let setTriggerSource rfSource trigger triggerSource = asyncChoice {
            match triggerSource with
            | Immediate  -> do! setSourceType rfSource trigger ImmediateType
            | TriggerKey -> do! setSourceType rfSource trigger TriggerKeyType
            | Bus        -> do! setSourceType rfSource trigger BusType
            | External (source, polarity) ->
                do! setSourceType rfSource trigger ExternalType
                do! setExternalSource rfSource trigger source
                do! setExternalSlopePolarity rfSource trigger polarity
            | Internal source ->
                do! setSourceType rfSource trigger InternalType
                do! setInternalSource rfSource trigger source
            | Timer period ->
                do! setSourceType rfSource trigger TimerType
                do! setTimerPeriod rfSource trigger period }

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
            do! Trigger.setTriggerSource rfSource StepTrigger options.StepTrigger
            do! Trigger.setTriggerSource rfSource ListTrigger options.ListTrigger
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

