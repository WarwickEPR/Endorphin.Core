namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Endorphin.Core.NationalInstruments

[<RequireQualifiedAccess>]
module internal IO =
    let postCommand key (RfSource rfSource) = Visa.writeString rfSource key

    let private queryValue parseFunc key (RfSource rfSource) = asyncChoice {
        let! response = sprintf "%s?" key |> Visa.queryInstrument rfSource
        return parseFunc response }

    let private tryQueryValue (tryParseFunc : string -> Choice<'T, string>) key rfSource = asyncChoice {
        let! response = queryValue id key rfSource 
        return! tryParseFunc response }

    let queryError = queryValue parseError

    let private nextErrorInQueueKey = ":SYSTEM:ERROR"
    let queryNextErrorInQueue = queryError nextErrorInQueueKey

    let queryErrorQueue rfSource = 
        let rec errorQueueLoop errorList = asyncChoice {
            let! nextError = queryNextErrorInQueue rfSource
            if nextError.Code <> 0 then return! errorQueueLoop (nextError :: errorList)
            else return Seq.ofList <| List.rev errorList  }
        errorQueueLoop List.empty

    let private checkErrorQueueIsEmpty errors =
        if Seq.length errors <> 0 then
            errors
            |> Seq.map errorString
            |> String.concat "\n" 
            |> fail 
        else succeed ()

    let private setValue (valueMap : 'v -> string) key (RfSource rfSource) (value : 'v) = asyncChoice {
        sprintf "%s %s" key (valueMap value) |> Visa.writeString rfSource
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }

    let tryQueryDeviceId = tryQueryValue tryParseDeviceId
    let queryDeviceId = queryValue parseDeviceId

    let private identityKey = "*IDN"
    let queryIdentity = queryDeviceId identityKey
    let tryQueryIdentity = tryQueryDeviceId identityKey


    let verifyModelNumber =
        function
        | "N5172B" -> succeed ()
        | serial   -> fail <| sprintf "Unexpected RF source serial number: %s." serial

    let verifyIdentity rfSource = asyncChoice {
        let! identity = tryQueryIdentity rfSource
        do! verifyModelNumber (identity.ModelNumber) }

    let setInt = setValue (fun (i : int) -> i.ToString())
    let queryInt = queryValue int

    let setFrequency = setValue frequencyString
    let queryFrequency = queryValue parseFrequencyInHz
    let setFrequencySeq key = setValue (csvSeqString frequencyString) key
    let queryFrequencySeq = queryValue (parseCsvSeq parseFrequencyInHz)

    let setAmplitude = setValue amplitudeString
    // TODO: Handle other units?
    let queryAmplitude key (RfSource rfSource) = asyncChoice {
        // Leaves units in original state
        let! powerUnit = ":UNIT:POW?" |> Visa.queryInstrument rfSource
        let! response = sprintf ":UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryInstrument rfSource 
        return parseAmplitudeInDbm response }

    let setAmplitudeSeq key = setValue (csvSeqString amplitudeString) key 
    let queryAmplitudeSeq key (RfSource rfSource) = asyncChoice {
        let! powerUnit = ":UNIT:POW?" |> Visa.queryInstrument rfSource
        let! response = sprintf "UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryInstrument rfSource
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

    let setModulationSource = setValue sourceString
    let queryModulationSource = queryValue parseSource

    let internal setAmplitudeModulationType = setValue depthTypeString
    let internal queryAmplitudeModulationType = queryValue parseDepthType

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