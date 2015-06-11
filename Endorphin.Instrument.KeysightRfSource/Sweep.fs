namespace Endorphin.Instrument.Keysight

open ExtCore.Control

module Sweep =
    module internal Translate =
        open Endorphin.Core.StringUtils

        let parseSweepMode str =
            match upperCase str with
            | "CW"
            | "FIX"
            | "FIXED" -> Fixed
            | "LIST"  -> Swept
            | str     -> failwithf "Unexpected sweep mode string: %s." str

        let sweepModeString =
            function
            | Fixed -> "FIX"
            | Swept -> "LIST"

        let parseStepSpacing str =
            match upperCase str with
            | "LIN" | "LINEAR"      -> LinearStepSpacing
            | "LOG" | "LOGARITHMIC" -> LogarithmicStepSpacing
            | _                     -> failwithf "Unexpected step spacing string: %s." str
    
        let stepSpacingString =
            function
            | LinearStepSpacing      -> "LIN"
            | LogarithmicStepSpacing -> "LOG"

        let parseSweepType str =
            match upperCase str with
            | "LIST" -> List
            | "STEP" -> Step
            | _      -> failwithf "Unexpected sweep type string: %s." str

        let sweepTypeString =
            function
            | List -> "LIST"
            | Step -> "STEP"

    module Control =
        open Translate

        [<AutoOpen>]
        module Frequency =
            let private frequencySweepModeKey = ":FREQUENCY:MODE"
            let setFrequencySweepMode = IO.setValue sweepModeString frequencySweepModeKey
            let queryFrequencySweepMode = IO.queryValue parseSweepMode frequencySweepModeKey

            let private startFrequencyKey = ":FREQUENCY:START"
            let setStartFrequency = IO.setFrequency startFrequencyKey
            let queryStartFrequncy = IO.queryFrequency startFrequencyKey

            let private stopFrequencyKey = ":FREQUENCY:STOP"
            let setStopFrequency = IO.setFrequency stopFrequencyKey
            let queryStopFrequency = IO.queryFrequency stopFrequencyKey

            let private frequencySpanKey = ":FREQUENCY:SPAN"
            let setFrequencySpan = IO.setFrequency frequencySpanKey
            let queryFrequencySpan = IO.queryFrequency frequencySpanKey

        [<AutoOpen>]
        module Amplitude =
            let private amplitudeSweepModeKey = ":POWER:MODE"
            let setAmplitudeSweepMode = IO.setValue sweepModeString amplitudeSweepModeKey
            let queryAmplitudeSweepMode = IO.queryValue parseSweepMode amplitudeSweepModeKey

            let private startAmplitudeKey = ":POWER:START"
            let setStartAmplitude = IO.setAmplitude startAmplitudeKey
            let queryStartAmplitude = IO.queryAmplitude startAmplitudeKey

            let private stopAmplitudeKey = ":POWER:STOP"
            let setStopAmplitude = IO.setAmplitude stopAmplitudeKey
            let queryStopAmplitude = IO.queryAmplitude stopAmplitudeKey


        let private typeKey = ":LIST:TYPE"
        /// set List or Step sweep
        let setSweepType = IO.setValue sweepTypeString typeKey
        let querySweepType = IO.queryValue parseSweepType typeKey

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

        let internal setSweepOptions rfSource options = asyncChoice {
            do! setDirection rfSource options.Direction
            do! Triggering.Control.setTriggerSource StepTrigger rfSource options.StepTrigger
            do! Triggering.Control.setTriggerSource ListTrigger rfSource options.ListTrigger
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
            let setStepSpacing = IO.setValue stepSpacingString stepSpacingKey
            let queryStepSpacing = IO.queryValue parseStepSpacing stepSpacingKey

            let private pointsCountKey = ":SWEEP:POINTS"
            /// Set number of points to have in sweep. 2 to 65535
            let setPointsCount = IO.setInt pointsCountKey
            let queryPointsCount = IO.queryInt pointsCountKey

            /// set frequency / frequency range for sweep
            let setFrequencySweep rfSource frequencySweep = asyncChoice {
                match frequencySweep with
                | FixedFrequency f ->
                    do! setCwFrequency rfSource f
                    do! setFrequencySweepMode rfSource Fixed
                | FrequencySweep sweep ->
                    do! Frequency.setStartFrequency rfSource sweep.Start
                    do! Frequency.setStopFrequency rfSource sweep.Stop
                    do! setFrequencySweepMode rfSource Swept }

            /// set amplitude / amplitude range for sweep
            let setAmplitudeSweep rfSource amplitudeSweep = asyncChoice {
                match amplitudeSweep with
                | FixedAmplitude a ->
                    do! setCwAmplitude rfSource a
                    do! setAmplitudeSweepMode rfSource Fixed
                | AmplitudeSweep sweep ->
                    do! Amplitude.setStartAmplitude rfSource sweep.Start
                    do! Amplitude.setStopAmplitude rfSource sweep.Stop
                    do! setAmplitudeSweepMode rfSource Swept }

        module List =
            let private dwellTimesKey = ":LIST:DWELL"
            let setDwellTimes key = IO.setDurationSeq dwellTimesKey key
            let queryDwellTimes = IO.queryDurationSeq dwellTimesKey

            let private dwellTimesCountKey = ":LIST:DWELL:POINTS"
            let queryDwellTimesCount = IO.queryInt dwellTimesCountKey

            let private dwellTypeKey = ":LIST:DWELL:TYPE"
            let setDwellType = IO.setValue sweepModeString dwellTypeKey
            let queryDwellType = IO.queryValue parseSweepMode dwellTypeKey

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
    module Runtime = 
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

    // Build a configuration using the data model
    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        let fixedPowerInDbm power = FixedAmplitude <| PowerInDbm power
        let fixedFrequencyInHz frequency = FixedFrequency <| FrequencyInHz frequency
        let frequencySweepInHz a b = FrequencySweep <| range (FrequencyInHz a) (FrequencyInHz b)
        let powerSweepInDbm a b = AmplitudeSweep <| range (PowerInDbm a) (PowerInDbm b)

        // Taking defaults from the documented *RST values
        let private defaultStepSweep = {
            Frequency = fixedFrequencyInHz 1e9<Hz>
            Amplitude = fixedPowerInDbm -110.0<dBm>
            Points = 101
            Spacing = LinearStepSpacing
            Options = { Direction = Up
                        StepTrigger = Immediate
                        ListTrigger = Immediate
                        DwellTime = Some ( DurationInSec 2e-3<s> )
                        Retrace = On
                        AttentuationProtection = On
                        Mode = Auto } }

        let withPoints points config       = { config with Points = points }
        let withSpacing spacing config     = { config with Spacing = spacing }
        let withDirection direction config = { config with Options = { config.Options with Direction = direction } }
        let withDwellTime time config      = { config with Options = { config.Options with DwellTime = time } }
        let withStepTrigger trigger config = { config with Options = { config.Options with StepTrigger = trigger } }
        let withListTrigger trigger config = { config with Options = { config.Options with ListTrigger = trigger } }
        let withRetrace state config       = { config with Options = { config.Options with Retrace = state } }
        let withAttenuationProtection state config =
            { config with Options = { config.Options with AttentuationProtection = state } }
        let withFixedPowerInDbm power config =
            { config with StepSweep.Amplitude = fixedPowerInDbm power }
        let withFixedFrequencyInHz frequency config =
            { config with StepSweep.Frequency = fixedFrequencyInHz frequency }
        let frequencyStepSweepInHz start finish =
            { defaultStepSweep with Frequency = frequencySweepInHz start finish }
        let powerStepSweepInDbm start finish =
            { defaultStepSweep with Amplitude = powerSweepInDbm start finish }

    // Apply a configuration
    module Apply =
        open Control
        /// Set up an RF step sweep from a model
        let stepSweep rfSource (stepSweep : StepSweep) = asyncChoice {
            do! Step.setFrequencySweep rfSource stepSweep.Frequency
            do! Step.setAmplitudeSweep rfSource stepSweep.Amplitude
            do! Step.setPointsCount rfSource stepSweep.Points
            do! Step.setStepSpacing rfSource stepSweep.Spacing
            do! setSweepOptions rfSource stepSweep.Options
            do! setSweepType rfSource Step }
