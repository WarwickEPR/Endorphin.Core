#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "bin/Debug/Endorphin.Instrument.TwickenhamSmc.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Endorphin.Instrument.TwickenhamSmc

log4net.Config.BasicConfigurator.Configure()

let settings = 
    { HardwareParameters =
        { MaximumCurrent = 20.0M<A>
          CalibratedRampRates =
            [ 0.00020; 0.00024; 0.00026; 0.00030; 0.00036; 0.00042; 0.00048; 0.00054; 
              0.00064; 0.00072; 0.00084; 0.00098; 0.00110; 0.00130; 0.00150; 0.00170;
              0.0020;  0.0024;  0.0026;  0.0030;  0.0036;  0.0042;  0.0048;  0.0054; 
              0.0064;  0.0072;  0.0084;  0.0098;  0.0110;  0.0130;  0.0150;  0.0170;
              0.020;   0.024;   0.026;   0.030;   0.036;   0.042;   0.048;   0.054; 
              0.064;   0.072;   0.084;   0.098;   0.110;   0.130;   0.150;   0.170;
              0.20;    0.24;    0.26;    0.30;    0.36;    0.42;    0.48;    0.54; 
              0.64;    0.72;    0.84;    0.98;    1.10;    1.30;    1.50;    1.70; 
              2.0 ]
            |> List.map (fun x -> (decimal x) * 1.0M<A/s>) }
              
      Limits = 
        { RampRateLimit    = 0.1M<A/s>
          TripVoltageLimit = 2.5M<V>
          CurrentLimit     = 17.5M<A> }
          
      FieldCalibration =
        { StaticField       = 14.146M<T>
          LinearCoefficient = -0.002845M<T/A> }
          
      ShuntCalibration = 
        { VoltageOffset     = 0.002M<V>
          LinearCoefficient = 0.400M<V/A> 
          RmsVoltageNoise   = 0.100M<V> } }

async {
    let! magnetController = MagnetController.openInstrument "GPIB0::4" 3000<ms> settings

    try
        let sweepParameters = 
            FieldSweep.Parameters.create Forward
            <| MagnetController.Output.Current.toStepIndex magnetController 0.5M<A>
            <| MagnetController.Output.Current.toStepIndex magnetController 0.9M<A>
            <| MagnetController.Ramp.Rate.nearestIndex magnetController 0.05M<A/s>
    
        let sweep = FieldSweep.create magnetController sweepParameters

        FieldSweep.status sweep |> Observable.add (printfn "%A")
    
        let cts = new System.Threading.CancellationTokenSource () // replace with, e.g. (10000) to cancel after 10000 ms
        let sweepHandle = FieldSweep.prepareWithCancellationToken sweep cts.Token 
    
        FieldSweep.setReadyToSweep sweep
        let! result = FieldSweep.waitToFinish sweepHandle
        printfn "Magnetic field sweep finished with result: %A" result
    
    finally 
        MagnetController.closeInstrument magnetController |> Async.StartImmediate }
|> Async.RunSynchronously