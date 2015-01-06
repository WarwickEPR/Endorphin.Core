namespace Endorphin.Test.TwickenhamSmc

open Endorphin.Instrument.TwickenhamSmc
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open NUnit.Framework
open System
open Config
open System.Threading

[<TestFixture>]
type ``Magnet ramp tests``() = 
    let magnetControllerSession = new MagnetControllerSession(magnetControllerVisaAddress, magnetControllerParameters)
    let _ = log4netConfig()

    [<SetUp>]
    member __.``Prepare magnet controller state``() = 
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously

    [<TestFixtureTearDown>]
    member __.``Close magnet controller session``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
            initialiseDefaultMagnetControllerState magnetController }
        |> Async.RunSynchronously

        magnetControllerSession.CloseSessionAsync() 
        |> Async.RunSynchronously
        
    [<Test>]
    member __.``Ramp rate out of range causes exception``() =
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 0
                  finalFieldIndex = 4095
                  rampRateIndex = -7
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 0
                  finalFieldIndex = 4095
                  rampRateIndex = 44
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 0
                  finalFieldIndex = 4095
                  rampRateIndex = 78
                  returnToZero = true }) |> ignore)
        |> ignore

    [<Test>]
    member __.``Ramp with zero steps causes exception``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 0
                  finalFieldIndex = 0
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 4095
                  finalFieldIndex = 4095
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = -231
                  finalFieldIndex = -231
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore
    
    [<Test>]
    member __.``Ramp exceeding current limit causes exception``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = -16384
                  finalFieldIndex = 234
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 16384 
                  finalFieldIndex = -124
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 6
                  finalFieldIndex = -32750
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)
        |> ignore
        
    [<Test>]
    member __.``Can initialise ramp worker``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController, 
                { startingFieldIndex = -16383
                  finalFieldIndex = 16383
                  rampRateIndex = 0
                  returnToZero = true }) |> ignore)

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 0
                  finalFieldIndex = 16383
                  rampRateIndex = 43
                  returnToZero = true }) |> ignore)

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController,
                { startingFieldIndex = 16383
                  finalFieldIndex = 0
                  rampRateIndex = 34
                  returnToZero = false }) |> ignore)

    [<Test>]
    member __.``Can cancel ramp immediately immediately after initiating preparation``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 
            
            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker = 
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 1500
                      finalFieldIndex = 4595
                      rampRateIndex = 0
                      returnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForPreparing =
                rampWorker.StatusChanged
                |> Event.filter ((=) PreparingRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForPreparing

            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Cancel true
            do! waitForCanceled

            Assert.AreEqual( 
                [ PreparingRamp ; CanceledRamp true ], 
                List.rev !rampStatusList)
            Assert.IsTrue(!canceledDidFire) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare to zero initial current with negative final current``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 0
                      finalFieldIndex = -2341
                      rampRateIndex = 41
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp

            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel true
            do! waitForCanceled

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp true ], 
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)        
            
            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.000<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.714<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare to positive initial current``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 500
                      finalFieldIndex = 4595
                      rampRateIndex = 7
                      returnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
        
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00054<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.153<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.402<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare to negative initial current``() =
        async { 
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false

            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -1023
                      finalFieldIndex = 412
                      rampRateIndex = 18
                      returnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
        
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel true
            do! waitForCanceled
                    
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp true ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync() 
            Assert.AreEqual(Zero, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)        
            Assert.AreEqual(0.09800<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.126<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.312<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare for positive ramp from negative initial current``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            magnetController.SetCurrentDirection Reverse
            magnetController.SetLowerSetPoint 0.5<A>
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
        
            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker = 
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 245
                      finalFieldIndex = 4126
                      rampRateIndex = 18
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00260<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.075<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.259<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare for negative ramp from positive initial current``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            magnetController.SetLowerSetPoint 0.5<A>
            magnetController.SetRampTarget Lower
            do! magnetController.WaitToReachTargetAsync()
        
            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -4231
                      finalFieldIndex = -124
                      rampRateIndex = 9
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled
        
            Assert.AreEqual( 
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00072<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.038<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.291<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare if magnet controller is initially ramping``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false

            magnetController.SetRampRate 0.0084<A/s>
            magnetController.SetRampTarget Upper
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 784
                      finalFieldIndex = 215
                      rampRateIndex = 22
                      returnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00480<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.066<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.239<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare if magnet controller is initially paused``() =
        async { 
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false

            magnetController.SetRampRate 0.0840<A/s>
            magnetController.SetRampTarget Upper
            do! Async.Sleep(10000)
            magnetController.SetPause true
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 928
                      finalFieldIndex = 1536
                      rampRateIndex = 12
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList)
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00110<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.283<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.469<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously
        
    [<Test>]
    member __.``Can cancel ramp while performing``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 5421
                      finalFieldIndex = 623
                      rampRateIndex = 39
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForRamping =
                rampWorker.StatusChanged
                |> Event.filter ((=) (Ramping Forward))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.PrepareAndStart()
            do! waitForRamping
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
                    
            rampWorker.Cancel false
            do! waitForCanceled
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; Ramping Forward ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.AreEqual(0.05400<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.190<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.654<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously
    
    [<Test>]
    member __.``Can cancel ramp while performing and return to zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -280
                      finalFieldIndex = -2345
                      rampRateIndex = 12
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForRamping =
                rampWorker.StatusChanged
                |> Event.filter ((=) (Ramping Reverse))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.PrepareAndStart()
            do! waitForRamping
                
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
                    
            rampWorker.Cancel true
            do! waitForCanceled
        
            Assert.AreEqual( 
                [ PreparingRamp ; ReadyToRamp ; Ramping Reverse ; CanceledRamp true ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.086<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.716<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform ramp``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 200
                      finalFieldIndex = 6002
                      rampRateIndex = 43
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                   
            let! waitForSuccess =
                rampWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; Ramping Forward ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.09800<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.061<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.832<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform ramp and return to zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()
        
            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 204
                      finalFieldIndex = 4921
                      rampRateIndex = 43
                      returnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; Ramping Forward; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.062<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.502<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare ramp and start later``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -235
                      finalFieldIndex = -993
                      rampRateIndex = 36
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            Assert.Throws<TimeoutException>(fun () ->
                Async.RunSynchronously(Async.AwaitEvent(rampWorker.StatusChanged) |> Async.Ignore, 10000))
            |> ignore

            let! waitForSuccess =
                rampWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.SetReadyToStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; Ramping Reverse; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.AreEqual(0.03600<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.072<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.303<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can cancel ramp when it is prepared but not started``() =
        async { 
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -77
                      finalFieldIndex = 4456
                      rampRateIndex = 12
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) ReadyToRamp)
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
        
            Assert.IsFalse(!canceledDidFire)
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ], 
                List.rev !rampStatusList )

            Assert.Throws<TimeoutException>(fun () ->
                Async.RunSynchronously(Async.AwaitEvent(rampWorker.StatusChanged) |> Async.Ignore, 10000))
            |> ignore
        
            let! waitForCanceled =
                rampWorker.Canceled
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild
            
            rampWorker.Cancel false
            do! waitForCanceled

            Assert.IsTrue(!canceledDidFire)
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; CanceledRamp false ],
                List.rev !rampStatusList )

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.outputParameters.rampTarget)
            Assert.IsTrue(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.00110<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.024<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(1.360<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform ramp up across zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = -205
                      finalFieldIndex = 623
                      rampRateIndex = 33
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual( 
                [ PreparingRamp ; ReadyToRamp ; Ramping Reverse ; ChangingCurrentDirection ; Ramping Forward ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.02400<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Forward, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.063<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.190<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can perform ramp down across zero``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { startingFieldIndex = 382
                      finalFieldIndex = -1000
                      rampRateIndex = 37
                      returnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Success
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp ; Ramping Forward ; ChangingCurrentDirection ; Ramping Reverse ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.outputParameters.rampTarget)
            Assert.IsFalse(parameters.currentParameters.isPaused)
            Assert.IsTrue(parameters.currentParameters.reachedTarget)
            Assert.AreEqual(0.04200<A/s>, parameters.operatingParameters.rampRate)
            Assert.AreEqual(Reverse, parameters.operatingParameters.currentDirection)
            Assert.AreEqual(0.117<A>, parameters.setPointParameters.lowerSetPoint)
            Assert.AreEqual(0.305<A>, parameters.setPointParameters.upperSetPoint) }
        |> Async.RunSynchronously