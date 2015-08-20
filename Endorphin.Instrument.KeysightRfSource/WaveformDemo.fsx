#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open ExtCore.Control
open Control
open ARB
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols


let printResult = function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let numSamples = 1000us

let generateSegment value count =
    let sample =
        defaultIqSample
        |> withAmplitudeAndPhase (float value / float System.Int16.MaxValue) (PhaseInRad 0.0<rad>)
        |> withMarkers { M1 = true; M2 = false; M3 = true; M4 = true }
    Segment { Samples = [| (sample, SampleCount (uint32 count)) |]; Length = count }

let segmentSequence = seq { for i in 1 .. 100
    -> generateSegment (int16 (32000.0 * float i / 100.0)) numSamples }

let segment1 = generateSegment 30000s numSamples
let segment2 = generateSegment 10000s numSamples

asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 10000

    let! storedSegment1 = storeWaveform keysight segment1
    let! storedSegment2 = storeWaveform keysight segment2

    printfn "%A" storedSegment1
    printfn "%A" storedSegment2

    let! storedSegmentArray = storeWaveformSequence keysight segmentSequence

    printfn "%A" storedSegmentArray

    let sequenceSequence = seq { for i in 1 .. 100
        -> Sequence [ (storedSegment1, 10us) ; (storedSegmentArray.[i-1], (uint16 i)) ] }

    let sequence1 = Sequence [ (storedSegment1, 10us) ; (storedSegment2, 20us) ]
    let! storedSequence1 = storeWaveform keysight sequence1

    printfn "%A" storedSequence1

    let sequence2 = Sequence [ (storedSegment1, 65509us) ; (storedSequence1, 100us) ]
    let! storedSequence2 = storeWaveform keysight sequence2

    printfn "%A" storedSequence2

    let sequence3 = Sequence [ (storedSegment2, 1us) ; (storedSequence1, 1024us) ]
    let! storedSequence3 = storeWaveform keysight sequence3

    printfn "%A" storedSequence3

    let! storedSequenceArray = storeWaveformSequence keysight sequenceSequence

    printfn "%A" storedSequenceArray

    // do! deleteStoredWaveform keysight storedSegment2
    // do! deleteStoredWaveform keysight storedSequence2

    // do! deleteAllStoredSegments keysight
    // do! deleteAllStoredSequences keysight
    // do! deleteAllStoredWaveforms keysight

    do RfSource.closeInstrument |> ignore }
|> Async.RunSynchronously
|> printResult