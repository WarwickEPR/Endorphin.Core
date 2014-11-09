namespace Endorphin.Test

open Endorphin.Instrument.TwickenhamSmc
open Devices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open TestUtils
open System.Threading

[<TestFixture>]
type ``Magnet ramp tests``() = 
    let mutable magnetControllerOpt : MagnetController option = None
    member this.magnetController 
        with get() : MagnetController = magnetControllerOpt.Value
        and set(value : MagnetController) = magnetControllerOpt <- Some(value)

    [<SetUp>]
    member this.``Connect to magnet controller and initialise``() =
        this.magnetController <-
            new MagnetController(magnetControllerVisaAddress, magnetControllerParameters)
        initialiseDefaultMagnetControllerState this.magnetController
        
    [<TearDown>]
    member this.``Disccnnect from magnet controller``() =
        (this.magnetController :> IDisposable).Dispose()

    [<TestFixtureTearDown>]
    member this.``Connect return magnet controller to initial state after tests``() =
        this.magnetController <-
            new MagnetController(magnetControllerVisaAddress, magnetControllerParameters)
        initialiseDefaultMagnetControllerState this.magnetController
        (this.magnetController :> IDisposable).Dispose()

    [<Test>]
    member this.``Ramp rate out of range causes exception``() =
        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 0
                finalFieldIndex = 4095
                rampRateIndex = -7
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 0
                finalFieldIndex = 4095
                rampRateIndex = 44
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 0
                finalFieldIndex = 4095
                rampRateIndex = 78
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

    [<Test>]
    member this.``Ramp with zero steps causes exception``() = 
        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 0
                finalFieldIndex = 0
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 4095
                finalFieldIndex = 4095
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = -231
                finalFieldIndex = -231
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore
    
    [<Test>]
    member this.``Ramp exceeding current limit causes exception``() = 
        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = -16384
                finalFieldIndex = 234
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 16384 
                finalFieldIndex = -124
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            let ramp = { 
                startingFieldIndex = 6
                finalFieldIndex = -32750
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)
        |> ignore
        
    [<Test>]
    member this.``Can initialise ramp worker``() = 
        Assert.DoesNotThrow(fun () ->
            let ramp = { 
                startingFieldIndex = -16383
                finalFieldIndex = 16383
                rampRateIndex = 0
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)

        Assert.DoesNotThrow(fun () ->
            let ramp = { 
                startingFieldIndex = 0
                finalFieldIndex = 16383
                rampRateIndex = 43
                returnToZero = true }
            new RampWorker(ramp, this.magnetController) |> ignore)

        Assert.DoesNotThrow(fun () ->
            let ramp = { 
                startingFieldIndex = 16383
                finalFieldIndex = 0
                rampRateIndex = 34
                returnToZero = false }
            new RampWorker(ramp, this.magnetController) |> ignore)

    [<Test>]
    member this.``Can cancel ramp immediately immediately after initiating preparation``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 500
            finalFieldIndex = 4595
            rampRateIndex = 0
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> rampStatusArray := Array.append !rampStatusArray [| newStatus |])
        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        rampWorker.Cancel(true)
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        if Array.length !rampStatusArray = 2 then
            Assert.ArrayElementsAreEqual( [| PreparingForRamp ; CancellingRamp |], !rampStatusArray )
        else
            Assert.ArrayElementsAreEqual( [| CancellingRamp |], !rampStatusArray )

        Assert.IsTrue(!canceledDidFire)

    [<Test>]
    member this.``Cancelling ramp before initiating preparation causes exception``() =
        let ramp = { 
            startingFieldIndex = 500
            finalFieldIndex = 4595
            rampRateIndex = 0
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        Assert.Throws<Exception>(fun () ->
            rampWorker.Cancel(true)) 
        |> ignore

    [<Test>]
    member this.``Can prepare to zero initial current with negative final current``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 0
            finalFieldIndex = -2341
            rampRateIndex = 14
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(true))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously

        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)        

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Zero, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.AreEqual(0.09800<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.000<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.715<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare to positive initial current``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 500
            finalFieldIndex = 4595
            rampRateIndex = 7
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Lower, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00054<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.152<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.403<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare to negative initial current``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = -1023
            finalFieldIndex = 412
            rampRateIndex = 18
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(true))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Zero, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)        
        Assert.AreEqual(0.09800<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.126<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.313<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare for positive ramp from negative initial current``() =
        this.magnetController.SetCurrentDirection Reverse
        this.magnetController.SetLowerSetPoint 0.5<A>
        this.magnetController.SetRampTarget Lower
        this.magnetController.WaitToReachTargetAsync() |> Async.RunSynchronously
        
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 245
            finalFieldIndex = 4126
            rampRateIndex = 18
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Lower, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00260<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.074<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.260<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare for negative ramp from positive initial current``() =
        this.magnetController.SetLowerSetPoint 0.5<A>
        this.magnetController.SetRampTarget Lower
        this.magnetController.WaitToReachTargetAsync() |> Async.RunSynchronously
        
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = -4231
            finalFieldIndex = -124
            rampRateIndex = 9
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00072<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.037<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.292<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare if magnet controller is initially ramping``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        this.magnetController.SetRampRate 0.0084<A/s>
        this.magnetController.SetRampTarget Upper

        let ramp = { 
            startingFieldIndex = 784
            finalFieldIndex = 215
            rampRateIndex = 22
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00480<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.065<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.240<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare if magnet controller is initially paused``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        this.magnetController.SetRampRate 0.0840<A/s>
        this.magnetController.SetRampTarget Upper
        Thread.Sleep(10000)
        this.magnetController.SetPause true

        let ramp = { 
            startingFieldIndex = 928
            finalFieldIndex = 1536
            rampRateIndex = 12
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Lower, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00110<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.283<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.469<A>, state.setPointParameters.upperLimit)
        
    [<Test>]
    member this.``Can cancel ramp while performing``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 5421
            finalFieldIndex = 623
            rampRateIndex = 39
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = PerformingRamp then
                rampWorker.Cancel(false))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.AreEqual(0.05400<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.190<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.655<A>, state.setPointParameters.upperLimit)
    
    [<Test>]
    member this.``Can cancel ramp while performing and return to zero``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = -280
            finalFieldIndex = -2345
            rampRateIndex = 12
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = PerformingRamp then
                rampWorker.Cancel(true))

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; CancellingRamp |], !rampStatusArray )
        Assert.IsTrue(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Zero, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.AreEqual(0.09800<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.085<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.716<A>, state.setPointParameters.upperLimit)
    
    [<Test>]
    member this.``Can perform ramp``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 200
            finalFieldIndex = 6002
            rampRateIndex = 43
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |])

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; FinishedRamp |], !rampStatusArray )
        Assert.IsFalse(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.09800<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.061<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.832<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can perform ramp and return to zero``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 204
            finalFieldIndex = 4921
            rampRateIndex = 43
            returnToZero = true }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |])

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; FinishedRamp |], !rampStatusArray )
        Assert.IsFalse(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Zero, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.AreEqual(0.09800<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.062<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.502<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can prepare ramp and start when ready``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = -235
            finalFieldIndex = -993
            rampRateIndex = 36
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                rampWorker.SetReadyToStart())

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; FinishedRamp |], !rampStatusArray )
        Assert.IsFalse(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.AreEqual(0.03600<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.071<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.304<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can cancel ramp when it is prepared but not started``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false
        let readyToRamp = new Event<unit>()

        let ramp = { 
            startingFieldIndex = -77
            finalFieldIndex = 4456
            rampRateIndex = 12
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |]
            if newStatus = ReadyToRamp then
                readyToRamp.Trigger())

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.Prepare()
        Async.AwaitEvent(readyToRamp.Publish) |> Async.RunSynchronously
        
        Assert.Throws<TimeoutException>(fun () ->
            Async.RunSynchronously(Async.AwaitEvent(rampWorker.Completed), 10000))
        |> ignore
        
        Assert.IsFalse(!canceledDidFire)
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp |], !rampStatusArray )

        async {
            do! Async.Sleep(2000)
            rampWorker.Cancel(false) }
        |> Async.Start

        Async.AwaitEvent(rampWorker.Canceled) |> Async.Ignore |> Async.RunSynchronously
        
        Assert.IsTrue(!canceledDidFire)
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; CancellingRamp |], !rampStatusArray )

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Lower, state.outputParameters.rampTarget)
        Assert.IsTrue(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.00110<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.024<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(1.360<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can perform ramp up across zero``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = -205
            finalFieldIndex = 623
            rampRateIndex = 33
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |])

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; FinishedRamp |], !rampStatusArray )
        Assert.IsFalse(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.02400<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Forward, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.063<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.191<A>, state.setPointParameters.upperLimit)

    [<Test>]
    member this.``Can perform ramp down across zero``() =
        let rampStatusArray = ref Array.empty
        let canceledDidFire = ref false

        let ramp = { 
            startingFieldIndex = 382
            finalFieldIndex = -1000
            rampRateIndex = 37
            returnToZero = false }
            
        let rampWorker = new RampWorker(ramp, this.magnetController)
        
        rampWorker.StatusChanged.Add(fun newStatus -> 
            rampStatusArray := Array.append !rampStatusArray [| newStatus |])

        rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

        rampWorker.PrepareAndStart()
        
        Async.AwaitEvent(rampWorker.Completed) |> Async.RunSynchronously
        
        Assert.ArrayElementsAreEqual( [| PreparingForRamp ; ReadyToRamp ; PerformingRamp ; FinishedRamp |], !rampStatusArray )
        Assert.IsFalse(!canceledDidFire)

        let state = this.magnetController.GetAllParametersAsync() |> Async.RunSynchronously
        Assert.AreEqual(Upper, state.outputParameters.rampTarget)
        Assert.IsFalse(state.currentParameters.isPaused)
        Assert.IsTrue(state.currentParameters.reachedTarget)
        Assert.AreEqual(0.04200<A/s>, state.operatingParameters.rampRate)
        Assert.AreEqual(Reverse, state.operatingParameters.currentDirection)
        Assert.AreEqual(0.117<A>, state.setPointParameters.lowerLimit)
        Assert.AreEqual(0.306<A>, state.setPointParameters.upperLimit)