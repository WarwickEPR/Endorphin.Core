namespace Endorphin.Instrument.Keysight

open ExtCore.Control

module Sweep =
    module internal Translate =
        /// Convert the machine representation of a sweep mode into the internal representation.
        let parseSweepMode str =
            match String.toUpper str with
            | "CW"
            | "FIX"
            | "FIXED" -> Fixed
            | "LIST"  -> Swept
            | str     -> failwithf "Unexpected sweep mode string: %s." str

        /// Convert the internal representation of the sweep mode into the machine representation.
        let sweepModeString = function
            | Fixed -> "FIX"
            | Swept -> "LIST"

        /// Convert the machine representation of the step spacing into the internal representation.
        let parseStepSpacing str =
            match String.toUpper str with
            | "LIN" | "LINEAR"      -> LinearStepSpacing
            | "LOG" | "LOGARITHMIC" -> LogarithmicStepSpacing
            | _                     -> failwithf "Unexpected step spacing string: %s." str

        /// Convert the internal representation of the step spacing into the machine representation.
        let stepSpacingString = function
            | LinearStepSpacing      -> "LIN"
            | LogarithmicStepSpacing -> "LOG"

        /// Convert the machine representation of the sweep type to the internal representation.
        let parseSweepType str =
            match String.toUpper str with
            | "LIST" -> List
            | "STEP" -> Step
            | _      -> failwithf "Unexpected sweep type string: %s." str

        /// Convert the internal representation of the sweep type to the machine representation.
        let sweepTypeString = function
            | List -> "LIST"
            | Step -> "STEP"

        /// Extract the string identifier from a stored sweep type.
        let storedSweepString (StoredSweep str) = str
        /// Convert an identifier into a StoredString identifier.
        let toStoredSweep str = StoredSweep str

    module Control =
        open Translate

        [<AutoOpen>]
        module Frequency =
            /// Key to set the frequency sweep mode.
            /// Command reference p.45.
            let private frequencySweepModeKey = ":FREQUENCY:MODE"
            /// Set the frequency sweep mode to the given type.
            let setFrequencySweepMode = IO.setValueString sweepModeString frequencySweepModeKey
            /// Query the frequency sweep mode the machine is currently in.
            let queryFrequencySweepMode = IO.queryKeyString parseSweepMode frequencySweepModeKey

            /// Key needed to set the first value in a frequency sweep.
            /// Command reference p.48.
            let private startFrequencyKey = ":FREQUENCY:START"
            /// Set the first value in a frequency sweep.
            let setStartFrequency = IO.setFrequency startFrequencyKey
            /// Query the currently set first value of the frequency sweep.
            let queryStartFrequncy = IO.queryFrequency startFrequencyKey

            /// Key needed to set the end value of the frequency sweep.
            /// Command reference p.48.
            let private stopFrequencyKey = ":FREQUENCY:STOP"
            /// Set the value of the end frequency in a frequency sweep.
            let setStopFrequency = IO.setFrequency stopFrequencyKey
            /// Query the currently set last value of the frequency sweep.
            let queryStopFrequency = IO.queryFrequency stopFrequencyKey

            /// Key needed to set the length of the frequency range for a step sweep.
            /// Command reference p.48.
            let private frequencySpanKey = ":FREQUENCY:SPAN"
            /// Set the range of a frequency step sweep.
            let setFrequencySpan = IO.setFrequency frequencySpanKey
            /// Query the currently set range of a frequency step sweep.
            let queryFrequencySpan = IO.queryFrequency frequencySpanKey

        [<AutoOpen>]
        module Amplitude =
            /// Key needed for the amplitude sweep type.
            /// Command reference p.84.
            let private amplitudeSweepModeKey = ":POWER:MODE"
            /// Set the type of the amplitude sweep.
            let setAmplitudeSweepMode = IO.setValueString sweepModeString amplitudeSweepModeKey
            /// Query the currently set type of the amplitude sweep.
            let queryAmplitudeSweepMode = IO.queryKeyString parseSweepMode amplitudeSweepModeKey

            /// Key needed for setting the first value in an amplitude sweep,
            /// Command reference p.86.
            let private startAmplitudeKey = ":POWER:START"
            /// Set the first value of an amplitude sweep.
            let setStartAmplitude = IO.setAmplitude startAmplitudeKey
            /// Query the currently set first value in an amplitude sweep.
            let queryStartAmplitude = IO.queryAmplitude startAmplitudeKey

            /// Key needed to set the end amplitude in an amplitude sweep.
            /// Command reference p.86.
            let private stopAmplitudeKey = ":POWER:STOP"
            /// Set the final value of an amplitude sweep.
            let setStopAmplitude = IO.setAmplitude stopAmplitudeKey
            /// Query the currently set value of the amplitude sweep.
            let queryStopAmplitude = IO.queryAmplitude stopAmplitudeKey

        /// Key needed to set the type of the sweep.
        let private typeKey = ":LIST:TYPE"
        /// Set the sweep type to either List or Step.
        let setSweepType = IO.setValueString sweepTypeString typeKey
        /// Query the current sweep type of the machine.
        let querySweepType = IO.queryKeyString parseSweepType typeKey

        /// Key needed to set the direction of the sweep.
        /// Command reference p.54.
        let private directionKey = ":LIST:DIRECTION"
        /// Set direction of sweep from start -> stop for Up and stop -> start for Down.
        let setDirection = IO.setDirection directionKey
        /// Query the currently set direction of the sweep.
        let queryDirection = IO.queryDirection directionKey

        /// Key needed to set whether sweeps will repeat continuously or simply stop.
        /// Command reference p.211.
        let private continuousModeKey = ":INITIATE:CONTINUOUS"
        /// Set whether sweeps should immediately repeat, or wait for some trigger to begin
        /// replaying.
        let setContinousMode = IO.setOnOffState continuousModeKey
        /// Query whterh sweeps are set to immediately repeat, or if they need some trigger
        /// to begin replaying.
        let queryContinuousMode = IO.queryOnOffState continuousModeKey

        /// Key needed to set the sweep mode to either automatic (sweep), or manual (select a
        /// single point from a sweep.
        /// Command reference p.56.
        let private modeKey = ":LIST:MODE"
        /// Sets manual or automatic mode for progressing through points of a sweep.
        let setMode = IO.setAutoManualState modeKey
        /// Query whether the current sweep mode is in automatic or manual selection.
        let queryMode = IO.queryAutoManualState modeKey

        /// Key needed to set the dwell time of each sweep step.
        /// Command reference p.63.
        let private dwellTimeKey = ":SWEEP:DWELL"
        /// Set the time to stay on each point of a sweep for.
        let setDwellTime = IO.setDuration dwellTimeKey
        /// Query the current time taken for each sweep point.
        let queryDwellTime = IO.queryDuration dwellTimeKey

        /// Key needed to turn attentuation protection on and off.
        let private attenuationProtectionKey = ":SWEEP:ATTEN:PROTECTION"
        /// Set the state of the attentuation protection for frequency and power step sweeps.
        /// Disabling allows the sweep to more optimally set the attenuation leveling control
        /// and the output attentuation at each sweep point, but sets the dwell time to a minimum
        /// of 50ms for safety.
        let setAttenuationProtection = IO.setOnOffState attenuationProtectionKey
        /// Query the current state of the attentuation protection.
        let queryAttenuationProtection = IO.queryOnOffState attenuationProtectionKey

        /// Key needed to set whether to remain on the last point of a sweep, or return to the
        /// first point.
        let private retraceKey = ":LIST:RETRACE"
        /// Set whether to retrace back to the first point of a sweep (On), or remain at the end
        /// point of the sweep (Off).
        let setRetrace = IO.setOnOffState retraceKey
        /// Query whether the machine will retrace back to the first point in a sweep (On), or if
        /// it will remain at the end (Off).
        let queryRetrace = IO.queryOnOffState retraceKey

        /// Apply the given sweep options to the machine.
        let internal setSweepOptions rfSource options = asyncChoice {
            do! setDirection rfSource options.Direction
            match options.StepTrigger with
            | Some trig -> do! Triggering.Control.setTriggerSource StepTrigger rfSource trig
            | None -> ()
            match options.ListTrigger with
            | Some trig -> do! Triggering.Control.setTriggerSource ListTrigger rfSource trig
            | None -> ()
            match options.DwellTime with
                | Some t -> do! setDwellTime rfSource t
                | None   -> if options.ListTrigger == Some Immediate
                            then return! fail "Dwell time required for free-running, immediate trigger sweep through a list of points"
            do! setRetrace rfSource options.Retrace 
            do! setAttenuationProtection rfSource options.AttentuationProtection
            do! setMode rfSource options.Mode }

        module Step =
            /// Key needed to set the step spacing of sweeps.
            /// Command reference p.65.
            let private stepSpacingKey = ":SWEEP:SPACING"
            /// Set the step spacing of sweeps to be either logarithmic or linear.
            let setStepSpacing = IO.setValueString stepSpacingString stepSpacingKey
            /// Query whether sweeps are set to use logarithmic or linear spacing.
            let queryStepSpacing = IO.queryKeyString parseStepSpacing stepSpacingKey

            /// Key needed to set the number of points in a sweep.
            /// Command reference p.65.
            let private pointsCountKey = ":SWEEP:POINTS"
            /// Set the number of points in a sweep to an integer between 2 and 65535.
            let setPointsCount = IO.setInt pointsCountKey
            /// Query how many points the sweep is set up to use.
            let queryPointsCount = IO.queryInt pointsCountKey

            /// Set the frequency sweep to use the given frequency sweep settings.
            let setFrequencySweep rfSource frequencySweep = asyncChoice {
                match frequencySweep with
                | FixedFrequency f ->
                    do! setCarrierFrequency rfSource f
                    do! setFrequencySweepMode rfSource Fixed
                | FrequencySweep sweep ->
                    do! Frequency.setStartFrequency rfSource sweep.Start
                    do! Frequency.setStopFrequency rfSource sweep.Stop
                    do! setFrequencySweepMode rfSource Swept }

            /// Set the amplitude sweep to use the given amplitude sweep settings.
            let setAmplitudeSweep rfSource amplitudeSweep = asyncChoice {
                match amplitudeSweep with
                | FixedAmplitude a ->
                    do! setCarrierAmplitude rfSource a
                    do! setAmplitudeSweepMode rfSource Fixed
                | AmplitudeSweep sweep ->
                    do! Amplitude.setStartAmplitude rfSource sweep.Start
                    do! Amplitude.setStopAmplitude rfSource sweep.Stop
                    do! setAmplitudeSweepMode rfSource Swept }

        module List =
            /// Key needed to set the time to dwell on each point in a list sweep.
            /// Command reference p.54.
            let private dwellTimesKey = ":LIST:DWELL"
            /// Set a sequence of times to dwell on each point in a list sweep.
            let setDwellTimes key = IO.setDurationSeq dwellTimesKey key
            /// Query the currently set sequence of times to dwell on each point in a list sweep.
            let queryDwellTimes = IO.queryDurationSeq dwellTimesKey

            /// Key needed to find out how many different dwell times are set.
            /// Command reference p.54.
            let private dwellTimesCountKey = ":LIST:DWELL:POINTS"
            /// Query how many dwell times are currently set for the sweep.
            let queryDwellTimesCount = IO.queryInt dwellTimesCountKey

            /// Key needed to set the dwell type from list or step sweeps.
            /// Command reference p.55.
            let private dwellTypeKey = ":LIST:DWELL:TYPE"
            /// Set the dwell type to either list or step sweeps.
            let setDwellType = IO.setValueString sweepModeString dwellTypeKey
            /// Query the currently set dwell type (either list or step).
            let queryDwellType = IO.queryKeyString parseSweepMode dwellTypeKey

            /// Key needed to set a list of amplitudes for the current sweep points.
            /// Command reference p.57.
            let private powersKey = ":LIST:POWER"
            /// Set the amplitudes of the current list sweep points.
            let setPowers key = IO.setAmplitudeSeq powersKey key
            /// Query the amplitudes of the current list sweep points.
            let queryPowers = IO.queryAmplitudeSeq powersKey

            /// Key needed to find how many amplitudes are in the current list sweep file.
            /// Command reference p.58.
            let private powersCountKey = ":LIST:POWER:POINTS"
            /// Query how many amplitudes are in the current list sweep file.
            let queryPowersCount = IO.queryInt powersCountKey

            /// Key needed to set a list of frequencies for the current sweep points.
            /// Command reference p.55.
            let private frequenciesKey = ":LIST:FREQUENCY"
            /// Set the frequencies of the current list sweep points.
            let setFrequencies key = IO.setFrequencySeq frequenciesKey key
            /// Query the frequencies of the current list sweep points.
            let queryFrequencies = IO.queryFrequencySeq frequenciesKey

            /// Key needed to find how many frequencies are in the current list sweep file.
            /// Command reference p.55.
            let private frequenciesCountKey = ":LIST:FREQUENCY:POINTS"
            /// Query how many frequencies are in the current list sweep file.
            let queryFrequenciesCount = IO.queryInt frequenciesCountKey

            /// Key needed to add waveform entries to the current list sweep.
            let private listWaveformKey = ":LIST:WAVEFORM"
            /// Set a single waveform as an element of a list sweep.
            let setListWaveform instrument stored =
                stored
                |> extractStoredWaveformFolderAndId
                |> IO.setFile listWaveformKey instrument

            /// Set a sequence of waveforms as elements of a list sweep.
            let setListWaveformSequence instrument sequence =
                sequence
                |> Seq.map extractStoredWaveformFolderAndId
                |> IO.setFileSequence listWaveformKey instrument

            /// Key needed to load a list file from memory.
            /// Command reference p.149, p.154.
            let private loadListKey = ":MMEM:LOAD:LIST"
            /// Load a list file into the current list sweep settings.
            let loadListFile instrument id =
                IO.setFile loadListKey instrument (listFolder, storedSweepString id)

            /// Key needed to store a list file into memory.
            /// Command reference p.150, p.155.
            let private storeListKey = ":MMEM:STORE:LIST"
            /// Store the currently loaded list sweep into a file in the non-volatile memory of the
            /// machine, so it can be reloaded later.
            let storeListFileById instrument id = asyncChoice {
                do! IO.setFile storeListKey instrument (listFolder, id)
                return toStoredSweep id }

    /// Commands used to control running sweeps.
    module Runtime = 
        /// Starts an armed sweep waiting on Bus triggering.
        let busTrigger = IO.writeKey "*TRG"

        /// Key needed to begin sweeping immediately.
        /// Command reference p.215.
        let private immediateKey = ":TRIGGER"
        /// Starts an armed sweep immediately without waiting for selected trigger event.
        let immediate = IO.writeKey immediateKey

        /// Key needed to get the current sweep point.
        /// Command reference p.63.
        let private currentPointKey = ":SWEEP:CPOINT"
        /// Reports current point in List/Step sequence.
        let queryCurrentPoint = IO.queryInt currentPointKey

        /// Key needed to immediately abort a sweep.
        /// Command reference p.211.
        let private abortKey = ":ABORT"
        /// Abort current sweep. In continous mode, arms a new sweep.
        let abort = IO.writeKey abortKey

        /// Aborts current sweep and rearms. With immediate triggering starts a new sweep.
        let abortAndRearmSingle = IO.writeKey ":TSWEEP"

        /// Key needed to rearm a single sweep.
        /// Command reference p.211.
        let private rearmSingleKey = ":INITIATE"
        /// Arms a single sweep. In immediate mode, starts immediately.
        let rearmSingle = IO.writeKey rearmSingleKey

    /// Build a configuration using the data model.
    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        /// Create an absolute value of fixed amplitude.
        let fixedPowerInDbm power = FixedAmplitude <| PowerInDbm power
        /// Create an amplitude sweep between two values, both in absolute dBm.
        let powerSweepInDbm a b = AmplitudeSweep <| range (PowerInDbm a) (PowerInDbm b)

        /// Create a fixed frequency value in Hz.
        let fixedFrequencyInHz frequency = FixedFrequency <| FrequencyInHz frequency
        /// Create a frequency sweep between two values of frequency, both in Hz.
        let frequencySweepInHz a b = FrequencySweep <| range (FrequencyInHz a) (FrequencyInHz b)

        /// A default set of options for a sweep.  These are the values the machine uses after a
        /// (*RST) command.
        let internal defaultSweepOptions = {
            Direction = Up
            StepTrigger = Some Immediate
            ListTrigger = Some Immediate
            DwellTime = Some ( DurationInSec 2e-3<s> )
            Retrace = On
            AttentuationProtection = On
            Mode = Auto }

        /// A default step sweep, using the values the machine would default to if issued the reset
        /// (*RST) command.
        let private defaultStepSweep = {
            Frequency = fixedFrequencyInHz 1e9<Hz>
            Amplitude = fixedPowerInDbm -110.0<dBm>
            Points = 101
            Spacing = LinearStepSpacing
            Options = defaultSweepOptions }

        /// Create a step sweep with the given points.
        let withPoints points (config : StepSweep) = { config with Points = points }

        /// Create a step sweep with the given spacing.
        let withSpacing spacing config = { config with Spacing = spacing }

        /// Create a step sweep with the given sweep direction.
        let withDirection direction config =
            { config with Options = { config.Options with Direction = direction } }

        /// Create a step sweep with the given sweep dwell time.
        let withDwellTime time config =
            { config with Options = { config.Options with DwellTime = time } }

        /// Create a step sweep with a given step trigger source.
        let withStepTrigger trigger config =
            { config with Options = { config.Options with StepTrigger = trigger } }

        /// Create a step sweep with a given list trigger source.
        let withListTrigger trigger config =
            { config with Options = { config.Options with ListTrigger = trigger } }

        /// Create a step sweep with retracing set to the specified state.
        let withRetrace state config =
            { config with Options = { config.Options with Retrace = state } }

        /// Create a step sweep with attenuation protection set to the specified state.
        let withAttenuationProtection state config =
            { config with Options = { config.Options with AttentuationProtection = state } }

        /// Create a step sweep with a fixed absolute power in dBm.
        let withFixedPowerInDbm power config =
            { config with StepSweep.Amplitude = fixedPowerInDbm power }
        /// Create a step sweep with a fixed frequency in Hz.
        let withFixedFrequencyInHz frequency config =
            { config with StepSweep.Frequency = fixedFrequencyInHz frequency }
        /// Create a step sweep with a frequency sweep between the specified start and end frequencies,
        /// measured in Hz.
        let frequencyStepSweepInHz start finish =
            { defaultStepSweep with Frequency = frequencySweepInHz start finish }
        /// Create a step sweep with an amplitude sweep between the specified start and end
        /// absolute amplitudes, measured in dBm.
        let powerStepSweepInDbm start finish =
            { defaultStepSweep with Amplitude = powerSweepInDbm start finish }

        /// Set the StepTrigger in a sweep options record.
        let internal optionsWithStepTrigger value options =
            { options with StepTrigger = value }

        /// Set the ListTrigger in a sweep options record.
        let internal optionsWithListTrigger value options =
            { options with ListTrigger = value }

        /// Set the dwell time in a sweep options record.
        let internal optionsWithDwellTime value options =
            { options with DwellTime = value }

        /// Set the state of the retrace in a step sweep options.
        let internal optionsWithRetrace value options =
            { options with Retrace = value }

    // Apply a configuration.
    module Apply =
        open Control
        /// Set up an RF step sweep from a model.
        let stepSweep rfSource (stepSweep : StepSweep) = asyncChoice {
            do! Step.setFrequencySweep rfSource stepSweep.Frequency
            do! Step.setAmplitudeSweep rfSource stepSweep.Amplitude
            do! Step.setPointsCount rfSource stepSweep.Points
            do! Step.setStepSpacing rfSource stepSweep.Spacing
            do! setSweepOptions rfSource stepSweep.Options
            do! setSweepType rfSource Step }