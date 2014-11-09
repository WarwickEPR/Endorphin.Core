namespace Endorphin.Experiment.MagnetRampAcquisition

open Endorphin.Instrument.PicoScope5000
open Endorphin.Instrument.PicoScope5000.PicoScope5000FSharp
open Endorphin.Instrument.TwickenhamSmc
open Endorphin.Instrument.TwickenhamSmc.TwickenhamSmcFSharp
open Endorphin.Instrument.TwickenhamSmc.MagnetRampFSharp
open FSharp.Control.Observable
open System
open System.Reactive.Linq
open System.Reactive.Subjects

type Test =
    { startingFieldIndex : int
      numberOfPoints : int
      stepsPerPoint : int
      rampRateIndex : int }

type TestStatus =
    | PreparingField
    | PreparingAcquisition
    | StartedRamp
    | Finished
    | Stopped

type TestObservables =
    { fieldInMillitelsa : IObservable<float>
      pointCounts : IObservable<int> array
      status : IObservable<TestStatus> }

type TestSubjects =
    { fieldInMillitesla : ISubject<float>
      pointCountAccumulators : int array
      pointCounts : ISubject<int> array
      status : ISubject<TestStatus> }
    
    member this.getObservables() =
        { fieldInMillitelsa = this.fieldInMillitesla.AsObservable()
          pointCounts = Array.map (fun (subject : ISubject<int>) -> subject.AsObservable()) this.pointCounts
          status = this.status.AsObservable() }

type Command =
    | StartRamp of test : Test * replyChannel : AsyncReplyChannel<TestObservables>

type MagnetRampAcquisitionAgent(picoScope : PicoScope5000Agent, magnetController : MagnetController, rampAgent : MagnetRampAgent) = 
    let mailboxProcessor =
        (fun (mailbox : MailboxProcessor<Command>) ->
            let asyncTest (test : Test) (subjects : TestSubjects) =
                let prepareForRamp readyForRamp = async { 
                    let ramp = 
                        { startingFieldIndex = test.startingFieldIndex
                          numberOfSteps = test.numberOfPoints * test.stepsPerPoint
                          rampRateIndex = test.rampRateIndex
                          returnToZero = false
                          readyForRamp = readyForRamp }
                    return performRamp ramp rampAgent }

                let setupChannels() = 
                    let channelSettings enabled = 
                        { enabled = enabled
                          coupling = Coupling.DC
                          range = Range._500mV 
                          analogueOffsetInVolts = 0.0 }
                        
                    setChannelSettings Channel.A (channelSettings true) picoScope
                    setChannelSettings Channel.B (channelSettings false) picoScope
                    setChannelSettings Channel.C (channelSettings false) picoScope
                    setChannelSettings Channel.D (channelSettings false) picoScope
                        
                    setAutoTrigger 1s picoScope
                        
                let processSample voltage =
                    let outputIndex = nearestDigitisedOutputIndexForShuntVoltage voltage magnetController
                    let outputField = nearestDigitisedFieldInMilliteslaForShuntVoltage voltage magnetController
                    subjects.fieldInMillitesla.OnNext(outputField)

                    let minimumOutputIndex = 
                        if test.stepsPerPoint > 0
                        then test.startingFieldIndex
                        else test.startingFieldIndex + test.stepsPerPoint * (test.numberOfPoints - 1)
                                
                    let maximumOutputIndex = 
                        if test.stepsPerPoint < 0
                        then test.startingFieldIndex
                        else test.startingFieldIndex + test.stepsPerPoint * (test.numberOfPoints - 1)

                    if outputIndex >= minimumOutputIndex && outputIndex < maximumOutputIndex
                    then let observableIndex = (outputIndex - test.startingFieldIndex) / test.stepsPerPoint
                         subjects.pointCountAccumulators.[observableIndex] <- 1 + subjects.pointCountAccumulators.[observableIndex]
                                       
                         subjects.pointCountAccumulators.[observableIndex]
                         |> subjects.pointCounts.[observableIndex].OnNext

                async {
                    use readyForRamp = new BehaviorSubject<bool>(false)
                    let! rampStatus = prepareForRamp readyForRamp
                    let! _ = rampStatus
                             |> Observable.filter (fun status -> status = ReadyToRamp)
                             |> Async.AwaitObservable
                    setupChannels()

                    let adcToVoltage = adcCountToVoltsConversion Range._500mV 0.0 picoScope
                        
                    use streamAgent = createStreamAgent picoScope

                    use processShuntVoltages = 
                        observe Channel.A Downsampling.None streamAgent
                        |> observeSamples
                        |> Observable.map adcToVoltage
                        |> Observable.subscribe processSample

                    let streamingParameters =
                        { sampleInterval = Microseconds(100u)
                          downsamplingRatio = 1u
                          maximumPreTriggerSamples = 0u 
                          maximumPostTriggerSamples = 10000u
                          autoStop = false }

                    let streamStatus = runStream streamingParameters streamAgent

                    let! _ = streamStatus
                             |> Observable.filter(
                                 function
                                 | Started(_) -> true
                                 | _ -> false)
                             |> Async.AwaitObservable

                    readyForRamp.OnNext(true)
                    
                    let! _ = rampStatus
                             |> Observable.filter (fun status -> status = FinishedRamp)
                             |> Async.AwaitObservable

                    stopStream streamAgent }

            let rec waiting() = async {
                let! message = mailbox.Receive()
                match message with
                | StartRamp(test, replyChannel) ->
                    let (subjects : TestSubjects) = 
                        { fieldInMillitesla = new Subject<float>() 
                          pointCountAccumulators = Array.zeroCreate (test.numberOfPoints)
                          pointCounts = Array.init (test.numberOfPoints) (fun _ -> (new Subject<int>()) :> ISubject<int> )
                          status = new BehaviorSubject<TestStatus>(PreparingField) }
                    replyChannel.Reply(subjects.getObservables())
                    do! asyncTest test subjects
                    return! waiting() }
            
            waiting ())
        |> MailboxProcessor.Start

    member this.Start(test) =
        fun replyChannel -> StartRamp(test, replyChannel)
        |> mailboxProcessor.PostAndReply