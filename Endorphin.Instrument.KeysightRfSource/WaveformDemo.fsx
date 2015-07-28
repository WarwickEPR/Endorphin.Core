#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Release\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Release\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open log4net.Config
open ExtCore.Control
open Control

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

// BasicConfigurator.Configure()

open Waveform.Configure

let printResult =
    function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let numSamples = 1000

let generateSegment value count =
    let sample =
        defaultIqSample
        |> withAmplitudeAndPhase (float value / float System.Int16.MaxValue) (PhaseInRad 0.0<rad>)
        |> withMarkers { M1 = true; M2 = false; M3 = true; M4 = true }
    [| for _ in 1 .. count -> sample |]

let segmentSequence = seq { for i in 1 .. 100
    -> generateSegment (int16 (32000.0 * float i / 100.0)) numSamples }

let segment1 = generateSegment 30000s numSamples
let segment2 = generateSegment 10000s numSamples

asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000

    let! storedSegment1 = storeSegment keysight segment1
    let! storedSegment2 = storeSegment keysight segment2

    printfn "%A" storedSegment1
    printfn "%A" storedSegment2

    let! storedSegmentArray = storeSegmentSequence keysight segmentSequence

    printfn "%A" storedSegmentArray

    let sequenceSequence = seq { for i in 1 .. 100
        -> [ Segment(storedSegment1, 10us) ; Segment(storedSegmentArray.[i-1], (uint16 i)) ] }

    let sequence1 = [ Segment(storedSegment1, 10us) ; Segment(storedSegment2, 20us) ]
    let! storedSequence1 = storeSequence keysight sequence1

    printfn "%A" storedSequence1

    let sequence2 = [ Segment(storedSegment1, 65509us) ; Sequence(storedSequence1, 100us) ]
    let! storedSequence2 = storeSequence keysight sequence2

    printfn "%A" storedSequence2

    let sequence3 = [ Segment(storedSegment2, 1us) ; Sequence(storedSequence1, 1024us) ]
    let! storedSequence3 = storeSequence keysight sequence3

    printfn "%A" storedSequence3

    let! storedSequenceArray = storeSequenceSequence keysight sequenceSequence

    printfn "%A" storedSequenceArray

    // do! deleteStoredSegment keysight storedSegment2
    // do! deleteStoredSequence keysight storedSequence2

    // do! deleteAllStoredSegments keysight
    // do! deleteAllStoredSequences keysight

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously
|> printResult