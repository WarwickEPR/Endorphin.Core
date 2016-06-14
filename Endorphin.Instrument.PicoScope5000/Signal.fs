namespace Endorphin.Instrument.PicoScope5000

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Core
open FSharp.Control.Reactive
open System.Reactive.Concurrency

/// Functions for obtaining the signal projections for an acquisition.
module Signal =
    /// Completes the given observable when the acquisition completes.
    let private takeUntilFinished acquisition =
        acquisition |> (Acquisition.status >> Observable.last >> Observable.takeUntilOther)

    /// Computes the timestamp of a sample for the given index and sample interval.
    let private time interval (downsampling : DownsamplingRatio option) index =
        let ratio =
            match downsampling with
            | None -> 1.0
            | Some ratio -> float ratio
//        printfn "Time: %s" (Interval.asString interval)
        (Interval.asSeconds interval) * (float index) * ratio

    /// Returns a function which converts an ADC count to a voltage for the given input sampling in an
    /// acquisition.
    let private adcCountToVoltage (inputChannel, _) (acquisition:AcquisitionParameters) = 
        let channelSettings = Inputs.settingsForChannel inputChannel acquisition.Inputs
        match channelSettings with
        | EnabledChannel settings ->
            let voltageRange   = Range.voltage settings.Range
            let analogueOffset = settings.AnalogueOffset
            (fun (adcCounts : int16) ->
                    voltageRange * (float32 adcCounts) / (float32 <| Resolution.maximumAdcCounts acquisition.Resolution)
                    - analogueOffset)
        | DisabledChannel -> failwithf "Cannot calculate voltage for channel %A as it is not enabled." inputChannel

    /// Returns a function which converts an array of samples from ADC counts to voltages according to
    /// the given array of input channel acquisition settings.
    let private adcCountsToVoltages (inputs : (InputChannel * BufferDownsampling) array) acquisition =
        Array.mapi (fun i adcCount -> adcCountToVoltage inputs.[i] acquisition adcCount)

    let private rawToByte raw =
        uint16 raw &&& uint16 0xff |> uint8

    let private rawToBit bit raw =
        let selector = uint16 1 <<< bit
        uint16 raw &&& uint16 0xff &&& selector > 0us

    /// Returns a sequence of sample arrays in the given sample block for the specified array of inputs.
    let private takeInputs inputs samples = seq {
        let length = samples.Length
        for i in 0 .. length - 1 ->
            samples.Samples
            |> Map.findArray inputs
            |> Array.map (fun block -> block.[i]) }
        
    /// Returns the n-th signal from an observable which emits an array of signals at each observation.
    let nth n = Observable.map (fun samples -> Array.get samples n)

    /// Takes the n-th and m-th signals from an observable which emits an array of signals at each
    /// observation and combines them into a tuple.
    let takeXY n m = Observable.map (fun samples -> (Array.get samples n, Array.get samples m))

    /// Takes the n-th and m-th signals from an observable which emits an array of indexed or timestamped
    /// signals at each observation and combines the values into a tuple, discarding the indecies.
    let takeXYFromIndexed n m = Observable.map (fun (_, samples) -> (Array.get samples n, Array.get samples m))

    /// Accumulates a signal, emitting the sequence of samples observed so far at each observation.
    let scan signal = Observable.scanInit List.empty List.cons signal |> Observable.map Seq.ofList

    let private samplesObserved acquisition =
        (common acquisition).SamplesObserved.Publish

    let private acquisitionParameters acquisition =
        (common acquisition).Parameters

    let private sampleInterval a =
        (acquisitionParameters a).SampleInterval

    let private sampleIntervalEvent acquisition =
        Acquisition.status acquisition
        |> Observable.choose (function | Acquiring interval -> Some interval | _ -> None)
        |> Observable.first
        |> Observable.repeat

    let private previousSamplesCount acquisition =
        samplesObserved acquisition
        |> Observable.scanInit 0 (fun c x -> c + x.Length)
        |> Observable.startWith [0]
        |> Observable.skipLast 1

    let private adcCountIndexedEvent input acquisition =
        let indexBlock (offset,block) =
            Seq.ofArray block |> Seq.mapi (fun i x -> (offset+i,x))
        samplesObserved acquisition
        |> Event.map (fun samples -> samples.Samples |> Map.find input)
        |> Observable.zip (previousSamplesCount acquisition)
        |> Observable.flatmapSeq indexBlock

    let private adcCountTimedEvent input acquisition =
        let downsamplingRatio = (acquisitionParameters acquisition).DownsamplingRatio
        let timestampBlock (offset,interval,block) =
            Seq.ofArray block
            |> Seq.mapi (fun i x -> (offset+i,x))
            |> Seq.map (fun (i,x) -> (time interval downsamplingRatio i, x))
        samplesObserved acquisition
        |> Event.map (fun samples -> samples.Samples |> Map.find input)
        |> Observable.zip3 (previousSamplesCount acquisition) (sampleIntervalEvent acquisition)
        |> Observable.flatmapSeq timestampBlock

    /// Helper function for constructing observables based on the ADC count samples for a given input.
    let private adcCountEvent input acquisition =
        samplesObserved acquisition
        |> Event.map (fun samples -> samples.Samples |> Map.find input)
        |> Observable.flatmapSeq Seq.ofArray
    
    let private timedAdcCountEvent input acquisition =
        adcCountEvent input acquisition
        |> Observable.combineLatest (fun interval adc -> (interval,adc)) (sampleIntervalEvent acquisition)

    let blockSampleCount input acquisition =
        samplesObserved acquisition
        |> Event.map (fun block -> block.Length)

    /// Returns an observable which emits the ADC count for every sample observed on the specified input
    /// in an acquisition.
    let adcCount input acquisition =
        adcCountEvent input acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits the voltage for every sample observed on the specified input in
    /// an acquisition.
    let voltage input acquisition =
        adcCountEvent input acquisition
        |> Observable.map (adcCountToVoltage input (acquisitionParameters acquisition))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits the byte representation of every sample observed on a digital port
    let digitalByte input acquisition =
        adcCountEvent input acquisition
        |> Observable.map rawToByte
        |> takeUntilFinished acquisition

    /// Returns an observable which emits the state of a single bit for every sample observed on a digital port
    let digitalBit bit input acquisition =
        adcCountEvent input acquisition
        |> Observable.map (rawToBit bit)
        |> takeUntilFinished acquisition


    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and ADC count for every sample observed on the specified input in an acquisition.
    let adcCountBy indexMapping input acquisition =
        adcCountEvent input acquisition
        |> Observable.mapi (fun i adcCount -> (indexMapping i, adcCount))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and voltage for every sample observed on the specified input in an acquisition.
    let voltageBy indexMapping input acquisition =
        adcCountEvent input acquisition
        |> Observable.mapi (fun i adcCount -> (indexMapping i, adcCountToVoltage input (acquisitionParameters acquisition) adcCount))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and voltage for every sample observed on the specified input in an acquisition.
    let digitalByteBy indexMapping input acquisition =
        adcCountEvent input acquisition
        |> Observable.mapi (fun i raw -> (indexMapping i, rawToByte raw))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and voltage for every sample observed on the specified input in an acquisition.
    let digitalBitBy bit indexMapping input acquisition =
        adcCountEvent input acquisition
        |> Observable.mapi (fun i raw -> (indexMapping i, (rawToBit bit) raw))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index and ADC count for every sample observed
    /// on the specified input in an acquisition.
    let adcCountByIndex = adcCountBy id

    /// Returns an observable which emits a tuple of timestamp and ADC count for every sample observed on
    /// the specified input in an acquisition.
    let adcCountByTime input (acquisition:Acquisition) =
        // Use Observable.defer to wait on status to get the Acquisition interval
        // then return the adcCounts with the time applied? Or Observable.combineLatest with a filtered, mapped time
        let p = acquisitionParameters acquisition
        adcCountBy <| time (sampleInterval acquisition) p.DownsamplingRatio
                   <| input
                   <| acquisition

    /// Returns an observable which emits a tuple of sample index and voltage for every sample observed on
    /// the specified input in an acquisition.
    let voltageByIndex = voltageBy id

    /// Returns an observable which emits a tuple of timestamp and voltage for every sample observed on the
    /// specified input in an acquisition.
    let voltageByTime input acquisition =
        let p = acquisitionParameters acquisition
        voltageBy
        <| time (sampleInterval acquisition) p.DownsamplingRatio
        <| input
        <| acquisition
        
    /// Returns an observable which emits a tuple of sample index and voltage for every sample observed on
    /// the specified input in an acquisition.
    let digitalByteByIndex = digitalByteBy id

    /// Returns an observable which emits a tuple of timestamp and voltage for every sample observed on the
    /// specified input in an acquisition.
    let digitalByteByTime input acquisition =
        let p = acquisitionParameters acquisition
        digitalByteBy
        <| time (sampleInterval acquisition) p.DownsamplingRatio
        <| input
        <| acquisition

    /// Returns an observable which emits a tuple of sample index and voltage for every sample observed on
    /// the specified input in an acquisition.
    let digitalBitByIndex bit = digitalBitBy bit id

    /// Returns an observable which emits a tuple of timestamp and voltage for every sample observed on the
    /// specified input in an acquisition.
    let digitalBitByTime bit input acquisition =
        let p = acquisitionParameters acquisition
        digitalBitBy bit
        <| time (sampleInterval acquisition) p.DownsamplingRatio
        <| input
        <| acquisition

    /// Helper function for constructing observables based on the ADC count samples for a given array of
    /// inputs.
    let private adcCountsEvent inputs acquisition =
        samplesObserved acquisition
        |> Observable.flatmapSeq (takeInputs inputs)

    /// Returns an observable which emits an array of ADC counts for every sample observed on the specified
    /// array of inputs in an acquisition.
    let adcCounts inputs acquisition =
        adcCountsEvent inputs acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits an array of voltages for every sample observed on the specified
    /// array of inputs in an acquisition.
    let voltages inputs acquisition =
        adcCountsEvent inputs acquisition
        |> Observable.map (adcCountsToVoltages inputs (acquisitionParameters acquisition))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and array of ADC counts for every sample observed on the specified array of inputs
    /// in an acquisition.
    let adcCountsBy indexMapping inputs acquisition =
        adcCountsEvent inputs acquisition
        |> Observable.mapi (fun i samples -> (indexMapping i, samples))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index mapped with the given index mapping
    /// function and array of voltages for every sample observed on the specified array of inputs in an
    /// acquisition.
    let voltagesBy indexMapping inputs acquisition =
        adcCountsEvent inputs acquisition
        |> Observable.mapi (fun i adcCounts -> (indexMapping i, adcCountsToVoltages inputs (acquisitionParameters acquisition) adcCounts))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a tuple of sample index and array of ADC counts for every sample
    /// observed on the specified array of inputs in an acquisition.
    let adcCountsByIndex = adcCountsBy id

    /// Returns an observable which emits a tuple of timestamp and array of ADC counts for every sample
    /// observed on the specified array of inputs in an acquisition.
    let adcCountsByTime inputs acquisition =
        let p = acquisitionParameters acquisition
        adcCountsBy
        <| time (sampleInterval acquisition) p.DownsamplingRatio
        <| inputs
        <| acquisition

    /// Returns an observable which emits a tuple of sample index and array of voltages for every sample
    /// observed on the specified array of inputs in an acquisition.
    let voltagesByIndex = voltagesBy id

    /// Returns an observable which emits a tuple of timestamp and array of voltages for every sample
    /// observed on the specified array of inputs in an acquisition.
    let voltagesByTime inputs acquisition =
        let p = acquisitionParameters acquisition
        voltagesBy
        <| time (sampleInterval acquisition) p.DownsamplingRatio
        <| inputs
        <| acquisition

    /// Returns an observable which emits a tuple of ADC counts for each pair of samples observed on the
    /// specified inputs in an acquisition.
    let adcCountXY xInput yInput acquisition =
        adcCounts [| xInput ; yInput |] acquisition
        |> takeXY 0 1

    /// Returns an observable which emits a tuple of voltages for each pair of samples observed on the
    /// specified inputs in an acquisition.
    let voltageXY xInput yInput acquisition =
        voltages [| xInput ; yInput |] acquisition
        |> takeXY 0 1

    /// Helper function for constructing observables based on sample blocks for a given input in an
    /// acquisition.
    let private adcCountByBlockEvent input acquisition =
        samplesObserved acquisition
        |> Event.map (fun samples -> samples.Samples |> Map.find input)

    /// Returns an observable which emits an array of ADC counts for each block of samples observed on the
    /// specified input in an acquisition.
    let adcCountByBlock input acquisition =
        adcCountByBlockEvent input acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits an array of voltages for each block of samples observed on the
    /// specified input in an acquisition.
    let voltageByBlock input acquisition =
        adcCountByBlockEvent input acquisition
        |> Event.map (Array.map (adcCountToVoltage input (acquisitionParameters acquisition)))
        |> takeUntilFinished acquisition

    /// Helper function for constructing observables based on the sample blocks for a given array of inputs
    /// in an acquisition.
    let private adcCountsByBlockEvent inputs acquisition =
        samplesObserved acquisition
        |> Event.map (fun samples -> samples.Samples |> Map.findArray inputs)

    /// Returns an observable which emits an array of ADC count blocks for each sample block observed for
    /// the given array of inputs in an acquisition.
    let adcCountsByBlock inputs acquisition =
        adcCountsByBlockEvent inputs acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits an array of voltage blocks for each sample block observed for the
    /// given array of inputs in an acquisition.
    let voltagesByBlock inputs acquisition = 
        adcCountsByBlockEvent inputs acquisition
        |> Event.map (Array.map (fun adcCounts -> adcCountsToVoltages inputs (acquisitionParameters acquisition) adcCounts))
        |> takeUntilFinished acquisition

    /// Helper function for constructing observables which buffer a specified number of the latest samples
    /// on a given input in acquisition.
    let private adcCountBufferedEvent count input acquisition =
        samplesObserved acquisition
        |> Observable.flatmapSeq (fun samples -> samples.Samples |> Map.find input |> Array.toSeq)
        |> Observable.ringBuffer count

    /// Returns an observable which emits the latest specified number of ADC counts sampled on a given
    /// input after each sample block in an acquisition.
    let adcCountBuffered count input acquisition =
        adcCountBufferedEvent count input acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits the latest specified number of voltages sampled on a given input
    /// after each sample block in an acquisition.
    let voltageBuffered windowSize input acquisition =
        adcCountBufferedEvent windowSize input acquisition
        |> Observable.map (Seq.map (adcCountToVoltage input (acquisitionParameters acquisition)))
        |> takeUntilFinished acquisition

    /// Helper function for constructing observables which buffer a specified number of the latest samples
    /// on a given array of inputs in an acquisition.
    let private adcCountsBufferedEvent count inputs acquisition =
        samplesObserved acquisition
        |> Observable.flatmapSeq (takeInputs inputs)
        |> Observable.ringBuffer count

    /// Returns an observable which emits an array of the latest specified number of ADC counts sampled on
    /// a given array of inputs after each sample block in an acquisition.
    let adcCountsBuffered count inputs acquisition =
        adcCountBufferedEvent count inputs acquisition
        |> takeUntilFinished acquisition

    /// Returns an observable which emits an array of the latest specified number of voltages sampled on a
    /// given array of inputs after each sample block in an acquisition.
    let voltagesBuffered count inputs acquisition =
        adcCountsBufferedEvent count inputs acquisition
        |> Observable.map (Seq.map (fun adcCounts -> adcCountsToVoltages inputs (acquisitionParameters acquisition) adcCounts))
        |> takeUntilFinished acquisition

    /// Returns an observable which emits a set of channels on which voltage overflow occurred if voltage 
    /// overflow occurs on any input channel in an acquisition. 
    let voltageOverflow acquisition =
        samplesObserved acquisition
        |> Event.filter (fun block -> not <| Set.isEmpty block.VoltageOverflows)
        |> Event.map (fun block -> block.VoltageOverflows)
        |> takeUntilFinished acquisition

    type EdgeDirection = RisingEdge | FallingEdge

    let digitalEdge direction (signal : IObservable<float<s>*bool>) =
        signal |> Observable.pairwise
        |> Observable.filter (fun ((_,a),(_,b)) ->
                                match direction with
                                | RisingEdge -> (not a) && b
                                | FallingEdge -> a && not b )
        |> Observable.map (fun ((at,_),(bt,_)) -> at)

    let debounce (d:float<s>) =
        Observable.scan (fun s t -> if t-s < d then s else t)
        >> Observable.distinctUntilChanged
