#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/ExtCore.0.8.45/lib/net45/ExtCore.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../Endorphin.Instrument.PiezosystemNV40\Piezojena.Protocols.dll"
#r "../Endorphin.Instrument.PiezosystemNV40\Piezojena.Protocols.Nv40Multi.dll"
#r "bin/Debug/Endorphin.Instrument.PiezosystemNV40.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open System.Threading
open System.Windows.Forms
open ExtCore.Control
open FSharp.Control.Reactive
open Endorphin.Core
open Endorphin.Instrument.PiezosystemNV40

SerialConnection.connect() 

let piezojena = Piezojena ""

module SingleMovement =
    
    let initalise = asyncChoice{
        do! PiezojenaNV40.SetParameters.setAllRemoteControl piezojena On
        do! PiezojenaNV40.SetParameters.setLoopModeallChannels piezojena ClosedLoop
        }

    let measurement = asyncChoice {
        do! PiezojenaNV40.SetParameters.setAllRemoteControl piezojena On
        do! PiezojenaNV40.SetParameters.setLoopModeallChannels piezojena ClosedLoop
        do! PiezojenaNV40.SetParameters.setAllOutputs piezojena (1.0 , 2.0 , 3.0)
        do! PiezojenaNV40.Query.queryAllPositions piezojena 
        }

SingleMovement.measurement |> Async.RunSynchronously

     


    

      

