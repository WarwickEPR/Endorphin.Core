namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Endorphin.Core.NationalInstruments
open System.Text

// Common functions to set/query values of a VISA Keysight instrument
// Includes functions to access values such as numbers, frequencies etc
// which are common to different subsystems

[<RequireQualifiedAccess>]
module internal IO =

    let postCommand key (RfSource rfSource) = Visa.writeString rfSource key

    let internal queryValue parseFunc key (RfSource rfSource) = asyncChoice {
        let! response = sprintf "%s?" key |> Visa.queryString rfSource
        return parseFunc response }

    let internal tryQueryValue (tryParseFunc : string -> Choice<'T, string>) key rfSource = asyncChoice {
        let! response = queryValue id key rfSource
        return! tryParseFunc response }

    [<AutoOpen>]
    module Error =

        let internal parseError (str : string) =
            let parts = str.Split [|','|]
            if Array.length parts <> 2 then failwithf "Unexpected error string: %s." str
        
            match parts.[0] with
            | ParseInteger code -> { Code = code ; Message = parts.[1] }
            | _                 -> failwithf "Unexpected error code string: %s." parts.[0]

        let internal errorString error = sprintf "%d: %s" error.Code error.Message

        let queryError = queryValue parseError

        let private nextErrorInQueueKey = ":SYSTEM:ERROR"
        let queryNextErrorInQueue = queryError nextErrorInQueueKey

        let queryErrorQueue rfSource = 
            let rec errorQueueLoop errorList = asyncChoice {
                let! nextError = queryNextErrorInQueue rfSource
                if nextError.Code <> 0 then return! errorQueueLoop (nextError :: errorList)
                else return Seq.ofList <| List.rev errorList  }
            errorQueueLoop List.empty

        let internal checkErrorQueueIsEmpty errors =
            if Seq.length errors <> 0 then
                errors
                |> Seq.map errorString
                |> String.concat "\n" 
                |> fail 
            else succeed ()

    let internal setValue (valueMap : 'v -> string) key (RfSource rfSource) (value : 'v) = asyncChoice {
        sprintf "%s %s" key (valueMap value) |> Visa.writeString rfSource
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }

    let internal setBytesValue (valueMap : 'v -> byte []) (key : string) (RfSource rfSource) (value : 'v) = asyncChoice {
        let bytesKey = Encoding.ASCII.GetBytes key
        Array.concat [bytesKey; " "B; (valueMap value); "\n"B] |> Visa.writeBytes rfSource
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors
        }

    let internal writeKey key (RfSource rfSource) = asyncChoice {
        postCommand key (RfSource rfSource)
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }
 
    module Identify =

        let internal tryParseDeviceId (str : string) =
            let trimWhiteSpace (str : string) = str.TrimStart([|' '|]).TrimEnd([|' '|])
            let parts = str.Split [|','|]
            if Array.length parts <> 4 then fail <| sprintf "Unexpected device ID string: %s." str
            else succeed <| { Manufacturer = parts.[0] |> trimWhiteSpace
                              ModelNumber = parts.[1] |> trimWhiteSpace
                              SerialNumber = parts.[2] |> trimWhiteSpace
                              Version = parts.[3] |> trimWhiteSpace }
          
        let internal parseDeviceId (str : string) =
            match tryParseDeviceId str with
            | Success id    -> id
            | Failure error -> failwith error

        let private identityKey = "*IDN"
        let queryIdentity = queryValue parseDeviceId identityKey
        let tryQueryIdentity = tryQueryValue tryParseDeviceId identityKey

        let private checkModelNumber =
            function
            | "N5172B" -> succeed N5172B
            | serial   -> fail <| sprintf "Unexpected RF source serial number: %s." serial

        let identity rfSource = asyncChoice {
            let! identity = tryQueryIdentity rfSource
            return checkModelNumber (identity.ModelNumber) }


    [<AutoOpen>]
    module Connect =

        let openInstrument visaAddress timeout = asyncChoice {
            let visaInstrument = Visa.openInstrument visaAddress timeout
            let rfSource = RfSource <| visaInstrument
            let! _ = Identify.identity rfSource
            let! _ = Error.queryErrorQueue rfSource // clear the error queue before doing anything
            return rfSource }

        let closeInstrument (RfSource rfSource) = Visa.closeInstrument rfSource

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
        let! powerUnit = ":UNIT:POW?" |> Visa.queryString rfSource
        let! response = sprintf ":UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryString rfSource
        return parseAmplitudeInDbm response }

    let setAmplitudeSeq key = setValue (csvSeqString amplitudeString) key 
    let queryAmplitudeSeq key (RfSource rfSource) = asyncChoice {
        let! powerUnit = ":UNIT:POW?" |> Visa.queryString rfSource
        let! response = sprintf "UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryString rfSource
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

    let setPercentage = setValue percentageString
    let queryPercentage = queryValue parsePercentage

    let setDecibelRatio = setValue decibelRatioString
    let queryDecibelRatio = queryValue parseDecibelRatio
    
    let setPolarity = setValue polarityString
    let queryPolarity = queryValue parsePolarity

    let setWaveform = setValue waveformIdString
    let queryWaveform = queryValue parseWaveformId
    let setWaveformSeq key = setValue (csvSeqString waveformIdString) key
    let queryWaveformSeq = queryValue (parseCsvSeq parseWaveformId)