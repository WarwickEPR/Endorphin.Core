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
                { StartingFieldIndex = 0
                  FinalFieldIndex = 4095
                  RampRateIndex = -7
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 0
                  FinalFieldIndex = 4095
                  RampRateIndex = 44
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 0
                  FinalFieldIndex = 4095
                  RampRateIndex = 78
                  ReturnToZero = true }) |> ignore)
        |> ignore

    [<Test>]
    member __.``Ramp with zero steps causes exception``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 0
                  FinalFieldIndex = 0
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 4095
                  FinalFieldIndex = 4095
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = -231
                  FinalFieldIndex = -231
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore
    
    [<Test>]
    member __.``Ramp exceeding current limit causes exception``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously
        
        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = -16384
                  FinalFieldIndex = 234
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 16384 
                  FinalFieldIndex = -124
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore

        Assert.Throws<Exception>(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 6
                  FinalFieldIndex = -32750
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)
        |> ignore
        
    [<Test>]
    member __.``Can initialise ramp worker``() = 
        use magnetController =
            magnetControllerSession.RequestControlAsync()
            |> Async.RunSynchronously

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController, 
                { StartingFieldIndex = -16383
                  FinalFieldIndex = 16383
                  RampRateIndex = 0
                  ReturnToZero = true }) |> ignore)

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 0
                  FinalFieldIndex = 16383
                  RampRateIndex = 43
                  ReturnToZero = true }) |> ignore)

        Assert.DoesNotThrow(fun () ->
            new RampWorker(
                magnetController,
                { StartingFieldIndex = 16383
                  FinalFieldIndex = 0
                  RampRateIndex = 34
                  ReturnToZero = false }) |> ignore)

    [<Test>]
    member __.``Can cancel ramp immediately immediately after initiating preparation``() =
        async {
            use! magnetController = magnetControllerSession.RequestControlAsync() 
            
            let rampStatusList = ref List.empty
            let canceledDidFire = ref false
            
            let rampWorker = 
                new RampWorker(
                    magnetController,
                    { StartingFieldIndex = 1500
                      FinalFieldIndex = 4595
                      RampRateIndex = 0
                      ReturnToZero = true })
        
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
                    { StartingFieldIndex = 0
                      FinalFieldIndex = -2341
                      RampRateIndex = 41
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Reverse))
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
                [ PreparingRamp ; ReadyToRamp Reverse ; CanceledRamp true ], 
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)        
            
            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.000<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.714<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 500
                      FinalFieldIndex = 4595
                      RampRateIndex = 7
                      ReturnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Forward))
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
                [ PreparingRamp ; ReadyToRamp Forward ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00054<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.153<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.402<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -1023
                      FinalFieldIndex = 412
                      RampRateIndex = 18
                      ReturnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Reverse))
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
                [ PreparingRamp ; ReadyToRamp Reverse ; CanceledRamp true ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync() 
            Assert.AreEqual(Zero, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)        
            Assert.AreEqual(0.09800<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.126<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.312<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 245
                      FinalFieldIndex = 4126
                      RampRateIndex = 18
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Forward))
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
                [ PreparingRamp ; ReadyToRamp Forward ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00260<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.075<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.259<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -4231
                      FinalFieldIndex = -124
                      RampRateIndex = 9
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Reverse))
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
                [ PreparingRamp ; ReadyToRamp Reverse ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00072<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.038<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.291<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 784
                      FinalFieldIndex = 215
                      RampRateIndex = 22
                      ReturnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Forward))
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
                [ PreparingRamp ; ReadyToRamp Forward ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00480<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.066<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.239<A>, parameters.SetPointParameters.UpperSetPoint) }
        |> Async.RunSynchronously

    [<Test>]
    member __.``Can prepare if magnet controller is initially paused``() =
        async { 
            use! magnetController = magnetControllerSession.RequestControlAsync()

            let rampStatusList = ref List.empty
            let canceledDidFire = ref false

            magnetController.SetRampRate 0.0840<A/s>
            magnetController.SetRampTarget Upper
            do! Async.Sleep 10000
            magnetController.SetPause true
            
            let rampWorker =
                new RampWorker(
                    magnetController,
                    { StartingFieldIndex = 928
                      FinalFieldIndex = 1536
                      RampRateIndex = 12
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Forward))
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
                [ PreparingRamp ; ReadyToRamp Forward ; CanceledRamp false ],
                List.rev !rampStatusList)
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00110<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.283<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.469<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 5421
                      FinalFieldIndex = 623
                      RampRateIndex = 39
                      ReturnToZero = false })
        
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
                [ PreparingRamp ; ReadyToRamp Forward ; Ramping Forward ; CanceledRamp false ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.AreEqual(0.05400<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.190<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.654<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -280
                      FinalFieldIndex = -2345
                      RampRateIndex = 12
                      ReturnToZero = false })
        
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
                [ PreparingRamp ; ReadyToRamp Reverse ; Ramping Reverse ; CanceledRamp true ],
                List.rev !rampStatusList )
            Assert.IsTrue(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.086<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.716<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 200
                      FinalFieldIndex = 6002
                      RampRateIndex = 43
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                   
            let! waitForSuccess =
                rampWorker.Completed
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Forward ; Ramping Forward ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.09800<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.061<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.832<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 204
                      FinalFieldIndex = 4921
                      RampRateIndex = 43
                      ReturnToZero = true })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Completed
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Forward ; Ramping Forward; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Zero, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.AreEqual(0.09800<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.062<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.502<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -235
                      FinalFieldIndex = -993
                      RampRateIndex = 36
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
                
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Reverse))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp

            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Reverse ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            Assert.Throws<TimeoutException>(fun () ->
                Async.RunSynchronously(Async.AwaitEvent(rampWorker.StatusChanged) |> Async.Ignore, 10000))
            |> ignore

            let! waitForSuccess =
                rampWorker.Completed
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.SetReadyToStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Reverse ; Ramping Reverse; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.AreEqual(0.03600<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.072<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.303<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -77
                      FinalFieldIndex = 4456
                      RampRateIndex = 12
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)
        
            let! waitForReadyToRamp =
                rampWorker.StatusChanged
                |> Event.filter ((=) (ReadyToRamp Reverse))
                |> Async.AwaitEvent
                |> Async.Ignore
                |> Async.StartChild

            rampWorker.Prepare()
            do! waitForReadyToRamp
        
            Assert.IsFalse(!canceledDidFire)
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Reverse ], 
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
                [ PreparingRamp ; ReadyToRamp Reverse ; CanceledRamp false ],
                List.rev !rampStatusList )

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Lower, parameters.OutputParameters.RampTarget)
            Assert.IsTrue(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.00110<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.024<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(1.360<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = -205
                      FinalFieldIndex = 623
                      RampRateIndex = 33
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Completed
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual( 
                [ PreparingRamp ; ReadyToRamp Reverse ; Ramping Reverse ; ChangingCurrentDirection ; ReadyToContinue Forward ; Ramping Forward ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.02400<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Forward, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.063<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.190<A>, parameters.SetPointParameters.UpperSetPoint) }
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
                    { StartingFieldIndex = 382
                      FinalFieldIndex = -1000
                      RampRateIndex = 37
                      ReturnToZero = false })
        
            rampWorker.StatusChanged.Add(fun newStatus -> rampStatusList := newStatus :: !rampStatusList)
            rampWorker.Canceled.Add(fun _ -> canceledDidFire := true)

            let! waitForSuccess =
                rampWorker.Completed
                |> Async.AwaitEvent
                |> Async.StartChild
            
            rampWorker.PrepareAndStart()
            do! waitForSuccess
        
            Assert.AreEqual(
                [ PreparingRamp ; ReadyToRamp Forward ; Ramping Forward ; ChangingCurrentDirection ; ReadyToContinue Reverse ; Ramping Reverse ; FinishedRamp ],
                List.rev !rampStatusList )
            Assert.IsFalse(!canceledDidFire)

            let! parameters = magnetController.GetAllParametersAsync()
            Assert.AreEqual(Upper, parameters.OutputParameters.RampTarget)
            Assert.IsFalse(parameters.CurrentParameters.IsPaused)
            Assert.IsTrue(parameters.CurrentParameters.ReachedTarget)
            Assert.AreEqual(0.04200<A/s>, parameters.OperatingParameters.RampRate)
            Assert.AreEqual(Reverse, parameters.OperatingParameters.CurrentDirection)
            Assert.AreEqual(0.117<A>, parameters.SetPointParameters.LowerSetPoint)
            Assert.AreEqual(0.305<A>, parameters.SetPointParameters.UpperSetPoint) }
        |> Async.RunSynchronously