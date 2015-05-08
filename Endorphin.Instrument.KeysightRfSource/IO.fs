namespace Endorphin.Instrument.Keysight

open Endorphin.Core.NationalInstruments

[<RequireQualifiedAccess>]
module internal IO =
    let postCommand key (RfSource rfSource) = rfSource.Write key

    let private setValue stringFunc key (RfSource rfSource) value = 
        sprintf "%s %s" key (stringFunc value) |> rfSource.Write

    let private queryValue parseFunc key (RfSource rfSource) = async {
        let! response = rfSource.Query (sprintf "%s?" key)
        return parseFunc response }

    let setValueForModulationPath (setFunc : string -> RfSource -> 'a -> unit) (keyFunc : ModulationPath -> string) rfSource path =
        setFunc (keyFunc path) rfSource

    let queryValueForModulationPath (queryFunc : string -> RfSource -> 'a) (keyFunc : ModulationPath -> string) rfSource path =
        queryFunc (keyFunc path) rfSource            

    let queryDeviceId = queryValue parseDeviceId

    let setInt = setValue (fun (i : int) -> i.ToString())
    let queryInt = queryValue int

    let setFrequency = setValue frequencyString
    let queryFrequency = queryValue parseFrequencyInHz
    let setFrequencySeq key = setValue (csvSeqString frequencyString) key
    let queryFrequencySeq = queryValue (parseCsvSeq parseFrequencyInHz)

    let setAmplitude = setValue amplitudeString
    // TODO: Handle other units?
    let queryAmplitude key (RfSource rfSource) = async {
        // Leaves units in original state
        let! powerUnit = rfSource.Query ":UNIT:POW?"
        let! response = sprintf ":UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> rfSource.Query 
        return parseAmplitudeInDbm response }

    let setAmplitudeSeq key = setValue (csvSeqString amplitudeString) key 
    let queryAmplitudeSeq key (RfSource rfSource) = async {
        let! powerUnit = rfSource.Query ":UNIT:POW?"
        let! response = sprintf "UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> rfSource.Query
        return parseCsvSeq parseAmplitudeInDbm <| response }

    let setDuration = setValue durationString 
    let queryDuration = queryValue parseDurationInSec
    let setDurationSeq key = setValue (csvSeqString durationString) key
    let queryDurationSeq = queryValue (parseCsvSeq parseDurationInSec)

    let setPhase = setValue phaseString
    let queryPhase = queryValue parsePhaseInRad

    let setOnOffState = setValue onOffStateString
    let queryOnOffState = queryValue parseOnOffState

    let setAutoManualState = setValue autoManualStateString
    let queryAutoManualState = queryValue parseAutoManualState

    let setDirection = setValue directionString
    let queryDirection = queryValue parseDirection

    let setStepSpacing = setValue stepSpacingString
    let queryStepSpacing = queryValue parseStepSpacing

    let setPercentage = setValue percentageString
    let queryPercentage = queryValue parsePercentage

    let setDecibelRatio = setValue decibelRatioString
    let queryDecibelRatio = queryValue parseDecibelRatio

    let setModulationSource = setValue modulationSourceString
    let queryModulationSource = queryValue parseModulationSource

    let setAmplitudeModulationType = setValue amplitudeModulationTypeString
    let queryAmplitudeModulationType = queryValue parseAmplitudeModulationType

    let setFunctionShape = setValue functionShapeString
    let queryFunctionShape = queryValue parseFunctionShapeType

    let setPolarity = setValue polarityString
    let queryPolarity = queryValue parsePolarity

    let setCoupling = setValue couplingString
    let queryCoupling = queryValue parseCoupling

    let setImpedance = setValue impedanceString
    let queryImpedance = queryValue parseImpedance

    let setSweepMode = setValue sweepModeString
    let querySweepMode = queryValue parseSweepMode

    let setSweepType = setValue sweepTypeString
    let querySweepType = queryValue parseSweepType

    let setTriggerSourceType = setValue triggerSourceTypeString
    let queryTriggerSourceType = queryValue parseTriggerSourceType

    let setExternalTriggerSource = setValue externalTriggerSourceString
    let queryExternalTriggerSource = queryValue parseExternalTriggerSource

    let setInternalTriggerSource = setValue internalTriggerSourceString
    let queryInternalTriggerSource = queryValue parseInternalTriggerSource

    let setWaveform = setValue waveformIdString
    let queryWaveform = queryValue parseWaveformId
    let setWaveformSeq key = setValue (csvSeqString waveformIdString) key
    let queryWaveformSeq = queryValue (parseCsvSeq parseWaveformId)

