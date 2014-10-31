namespace Endorphin.Experiment.CwEpr

open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.PicoScope5000.PicoScope5000FSharp
open Endorphin.Instrument.TwickenhamSmc
open Endorphin.Instrument.TwickenhamSmc.TwickenhamSmcFSharp
open Endorphin.Instrument.TwickenhamSmc.MagnetRampFSharp
open FSharp.Control.Observable
open System
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Threading
open System.Collections.Generic

type Experiment =
    { startingFieldIndex : int
      numberOfPoints : int
      stepsPerPoint : int
      rampRateIndex : int
      quadratureDetection : bool
      numberOfScans : int
      readyToStart : IObservable<bool> }

type ExperimentStatus =
    | PreparingFieldForScan of scan : int
    | PreparingAcquisitionForScan of scan : int
    | StartedScan of scan : int
    | FinishedScan of scan : int
    | FinishedExperiment
    | StoppedExperiment

type ExperimentObservables =
    { fieldInMillitelsa : IObservable<float>
      inPhasePoints : IObservable<float> array
      quadraturePoints : IObservable<float> array
      status : IObservable<ExperimentStatus> }

type ExperimentSubjects =
    { fieldInMillitesla : ISubject<float>
      inPhasePoints : ISubject<float> array
      quadraturePoints : ISubject<float> array
      status : ISubject<ExperimentStatus> }
    
    member this.asObservables() =
        { fieldInMillitelsa = this.fieldInMillitesla.AsObservable()
          inPhasePoints = Array.map (fun (subject : ISubject<float>) -> subject.AsObservable()) this.inPhasePoints
          quadraturePoints = Array.map (fun (subject : ISubject<float>) -> subject.AsObservable()) this.quadraturePoints
          status = this.status.AsObservable() }

type Command =
    | StartExperiment of experiment : Experiment * replyChannel : AsyncReplyChannel<ExperimentObservables>
    | StopExperiment of finishScan : bool
    // TODO: add pause / resume

type CwEprAgent(picoScope : PicoScope5000Agent, magnetController : MagnetController, rampAgent : MagnetRampAgent) = 
    let mailboxProcessor =
        (fun (mailbox : MailboxProcessor<Command>) ->
            let asyncExperiment (experiment : Experiment) (subjects : ExperimentSubjects) =
                let prepareForRamp readyForRamp = async { 
                    let ramp = 
                        { startingFieldIndex = experiment.startingFieldIndex
                          numberOfSteps = experiment.numberOfPoints * experiment.stepsPerPoint
                          rampRateIndex = experiment.rampRateIndex
                          returnToZero = false
                          readyForRamp = readyForRamp }
                    return performRamp ramp rampAgent }

                let prepareForAcquisition = async { 
                    let channelSettings enabled = 
                        { enabled = enabled
                          coupling = Coupling.DC
                          range = Range._500mV 
                          analogueOffsetInVolts = 0.0 }
                    
                    setChannelSettings Channel.A (channelSettings true) picoScope
                    setChannelSettings Channel.B (channelSettings true) picoScope
                    setChannelSettings Channel.C (channelSettings experiment.quadratureDetection) picoScope
                    setChannelSettings Channel.D (channelSettings false) picoScope
                    
                    setAutoTrigger 1s picoScope
                    
                    let adcToVoltage = adcCountToVoltsConversion Range._500mV 0.0 picoScope
                    let streamAgent = createStreamAgent picoScope

                    let b = observe Channel.A Downsampling.Averaged streamAgent
                            |> observeSamples
                            |> Observable.map adcToVoltage

                    let x = observe Channel.B Downsampling.Averaged streamAgent
                            |> observeSamples
                            |> Observable.map adcToVoltage

                    let signals = Observable.Zip(b, x);
                
                    let streamingParameters =
                        { sampleInterval = Microseconds(100u)
                          downsamplingRatio = 200u
                          maximumPreTriggerSamples = 0u 
                          maximumPostTriggerSamples = 10000u
                          autoStop = false }

                    let streamStatus = runStream streamingParameters streamAgent
                    return (streamAgent, signals, streamStatus) }

                let subscribeToAcquisition (signals : IObservable<IList<float>>) = async {
                        return signals
                        |> Observable.subscribe (
                            fun samples ->
                                let outputIndex = nearestDigitisedOutputIndexForShuntVoltage (samples.Item(0)) magnetController
                                
                                let minimumOutputIndex = 
                                    if experiment.stepsPerPoint > 0
                                    then experiment.startingFieldIndex
                                    else experiment.startingFieldIndex + experiment.stepsPerPoint * (experiment.numberOfPoints - 1)
                                
                                let maximumOutputIndex = 
                                    if experiment.stepsPerPoint < 0
                                    then experiment.startingFieldIndex
                                    else experiment.startingFieldIndex + experiment.stepsPerPoint * (experiment.numberOfPoints - 1)

                                if outputIndex >= minimumOutputIndex && outputIndex < maximumOutputIndex
                                then let observableIndex = (outputIndex - experiment.startingFieldIndex) / experiment.stepsPerPoint
                                     subjects.inPhasePoints.[observableIndex].OnNext(samples.Item(1))) }


                async {
                    use readyForRamp = new BehaviorSubject<bool>(false)
                    let! rampStatus = prepareForRamp readyForRamp
                    let! _ = rampStatus
                             |> Observable.filter (fun status -> status = ReadyToRamp)
                             |> Async.AwaitObservable
                    
                    let! (streamAgent, signals, streamStatus) = prepareForAcquisition
                    let! _ = streamStatus
                             |> Observable.filter(
                                 function
                                 | Started(_) -> true
                                 | _ -> false)
                             |> Async.AwaitObservable

                    use! processData = subscribeToAcquisition signals
                    readyForRamp.OnNext(true)
                    
                    let! _ = rampStatus
                             |> Observable.filter (fun status -> status = FinishedRamp)
                             |> Async.AwaitObservable

                    stopStream streamAgent
                    (streamAgent :> IDisposable).Dispose() }

            let rec waiting() = async {
                let! message = mailbox.Receive()
                match message with
                | StartExperiment(experiment, replyChannel) ->
                    let (subjects : ExperimentSubjects) = 
                        { fieldInMillitesla = new Subject<float>() 
                          inPhasePoints =  Array.init (experiment.numberOfPoints) (fun _ -> (new Subject<_>()) :> ISubject<_> )
                          quadraturePoints = 
                              if experiment.quadratureDetection
                              then Array.init (experiment.numberOfPoints) (fun _ -> (new Subject<_>()) :> ISubject<_> )
                              else null
                          status = new BehaviorSubject<ExperimentStatus>(PreparingFieldForScan(1)) }
                    replyChannel.Reply(subjects.asObservables())
                    return! runningExperiment experiment subjects
                | StopExperiment(_) -> return! waiting() }
            
            and runningExperiment experiment subjects = async {
                do!
                    use experimentCts = new CancellationTokenSource()
                    use mailboxCts = new CancellationTokenSource()
                    use cancelMailboxToken = 
                        subjects.status.AsObservable().IgnoreElements()
                        |> Observable.subscribe (fun _ -> mailboxCts.Cancel())

                    let rec waitForStopMessage() = async {
                        if (not mailboxCts.IsCancellationRequested)
                        then if mailbox.CurrentQueueLength <> 0
                             then let! message = mailbox.Receive()
                                  match message with 
                                  | StopExperiment(finishScan) -> 
                                      if finishScan
                                      then subjects.status.AsObservable()
                                           |> Observable.filter(
                                               function
                                               | FinishedScan(_) -> true
                                               | _ -> false)
                                           |> Observable.add (fun _ -> experimentCts.Cancel())
                                           // wait for experiment to finish
                                      else experimentCts.Cancel()
                                  | _ -> failwith "Attempted to start a CW EPR experiment when one is already in progress."
                             else do! Async.Sleep(100)
                                  do! waitForStopMessage() }

                    Async.Start(asyncExperiment experiment subjects, experimentCts.Token)
                    waitForStopMessage()

                return! waiting() }

            waiting ())
        |> MailboxProcessor.Start

    member this.Post(message) = mailboxProcessor.Post(message)