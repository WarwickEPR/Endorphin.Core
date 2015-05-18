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

    module Sweep =
        let private currentPointKey = ":SWEEP:CPOINT"
        let setCurrentPoint = IO.setInt currentPointKey
        let queryCurrentPoint = IO.queryInt currentPointKey

        let private directionKey = ":LIST:DIRECTION"
        let setDirection = IO.setDirection directionKey
        let queryDirection = IO.queryDirection directionKey

        let private modeKey = ":LIST:MODE"
        let setMode = IO.setAutoManualState modeKey
        let queryMode = IO.queryAutoManualState modeKey

        let private dwellTimeKey = ":SWEEP:DWELL"
        let setDwellTime = IO.setDuration dwellTimeKey
        let queryDwellTime = IO.queryDuration dwellTimeKey

        let private stepSpacingKey = ":SWEEP:SPACING"
        let setStepSpacing = IO.setStepSpacing stepSpacingKey
        let queryStepSpacing = IO.queryStepSpacing stepSpacingKey

        let private pointsCountKey = ":SWEEP:POINTS"
        let setPointsCount = IO.setInt pointsCountKey
        let queryPointsCount = IO.queryInt pointsCountKey

        let private attenuationProtectionKey = ":SWEEP:ATTEN:PROTECTION"
        let setAttenuationProtection = IO.setOnOffState attenuationProtectionKey
        let queryAttenuationProtection = IO.queryOnOffState attenuationProtectionKey

        let private stepSizeLinearKey = ":SWEEP:FREQUENCY:STEP:LINEAR"
        let private stepSizeLogarithmicKey = ":SWEEP:FREQUENCY:STEP:LOGARITHMIC"
        let setStepSize rfSource stepSize = asyncChoice {
            match stepSize with
            | LinearStep size ->
                do! setStepSpacing rfSource LinearStepSpacing
                do! IO.setFrequency stepSizeLinearKey rfSource size
            | LogarithmicStep size ->
                do! setStepSpacing rfSource LogarithmicStepSpacing
                do! IO.setPercentage stepSizeLogarithmicKey rfSource size }
        let queryStepSize rfSource = asyncChoice {
            let! spacing = queryStepSpacing rfSource
            match spacing with
            | LinearStepSpacing ->
               let! size = IO.queryFrequency stepSizeLinearKey rfSource
               return LinearStep size
            | LogarithmicStepSpacing ->
               let! size = IO.queryPercentage stepSizeLogarithmicKey rfSource
               return LogarithmicStep size }

        let setFrequencySweep rfSource frequencySweep = asyncChoice {
            match frequencySweep with
            | FixedFrequency f -> do! Frequency.setCwFrequency rfSource f
            | FrequencySweep sweep ->
                do! Frequency.setStartFrequency rfSource sweep.Begin
                do! Frequency.setStopFrequency rfSource sweep.End }

        let setAmplitudeSweep rfSource amplitudeSweep = asyncChoice {
            match amplitudeSweep with
            | FixedAmplitude a -> do! Amplitude.setCwAmplitude rfSource a
            | AmplitudeSweep sweep ->
                do! Amplitude.setStartAmplitude rfSource sweep.Begin
                do! Amplitude.setStopAmplitude rfSource sweep.End }

        let private continuousModeKey = ":INITIATE:CONTINUOUS"
        let setContinousMode = IO.setOnOffState continuousModeKey
        let queryContinuousMode = IO.queryOnOffState continuousModeKey

        let private abortKey = ":ABORT"
        let abort = IO.postCommand abortKey

        let private rearmSingleKey = ":INITIATE"
        let rearmSingle = IO.postCommand rearmSingleKey

        module Trigger =
            let private immediateKey = ":TRIGGER"
            let immediate = IO.postCommand immediateKey

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

        module List =
            let private typeKey = ":LIST:TYPE"
            let setType = IO.setSweepType typeKey
            let queryType = IO.querySweepType typeKey

            let private dwellTimesKey = ":LIST:DWELL"
            let setDwellTimes key = IO.setDurationSeq dwellTimesKey key
            let queryDwellTimes = IO.queryDurationSeq dwellTimesKey

            let private dwellTimesCountKey = ":LIST:DWELL:POINTS"
            let queryDweelTimesCount = IO.queryInt dwellTimesCountKey

            let private dwellTypeKey = ":LIST:DWELL:TYPE"
            let setDwellType = IO.setSweepMode dwellTypeKey
            let queryDwellType = IO.querySweepMode dwellTypeKey

            let private powersKey = ":LIST:POWER"
            let setPowers key = IO.setAmplitudeSeq powersKey key
            let queryPowers = IO.queryAmplitudeSeq powersKey

            let private powersCountKey = ":LIST:POWER:POINTS"
            let queryPowersCount = IO.queryInt powersCountKey

            let private retraceKey = ":LIST:RETRACE"
            let setRetrace = IO.setOnOffState retraceKey
            let queryRetrace = IO.queryOnOffState retraceKey

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

        let private setSweepOptions rfSource options = asyncChoice {
            do! setDirection rfSource options.Direction
            do! Trigger.setTriggerSource rfSource StepTrigger options.StepTrigger
            do! Trigger.setTriggerSource rfSource ListTrigger options.ListTrigger
            match options.DwellTime with
             | Some t -> do! setDwellTime rfSource t
             | None   -> if options.ListTrigger == Immediate
                         then return! fail "Dwell time required for free-running, immediate trigger sweep through a list of points"
            do! List.setRetrace rfSource options.Retrace 
            do! setAttenuationProtection rfSource options.AttentuationProtection }

        let setStepSweep rfSource (stepSweep : StepSweep) = asyncChoice {
            do! setFrequencySweep rfSource stepSweep.Frequency
            do! setAmplitudeSweep rfSource stepSweep.Amplitude
            do! setPointsCount rfSource stepSweep.Points
            do! setStepSpacing rfSource stepSweep.Spacing
            do! setSweepOptions rfSource stepSweep.Options}