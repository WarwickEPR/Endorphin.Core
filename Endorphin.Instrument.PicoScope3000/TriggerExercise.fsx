#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "../packages/FSharp.Control.Reactive.3.2.0/lib/net40/FSharp.Control.Reactive.dll"
#r "../packages/Rx-Linq.2.2.5/lib/net45/System.Reactive.Linq.dll"
#r "../packages/Rx-Interfaces.2.2.5/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/Rx-Core.2.2.5/lib/net45/System.Reactive.Core.dll"
#r "../packages/FSharp.Charting.0.90.13/lib/net40/FSharp.Charting.dll"
#r "bin/Debug/Endorphin.Instrument.PicoScope3000.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r "../packages/log4net.2.0.3/lib/net40-full/log4net.dll"

open Endorphin.Instrument.PicoScope3000
open Model.Triggering.Complex

let triggerA = Require <| (Channel <| AnalogueTrigger ChannelA, true)
let triggerB = Require <| (Channel <| AnalogueTrigger ChannelB, true)
let digitalTrigger = Require <| (DigitalTrigger, false)
let andTrigger = (And (triggerA,triggerB))
let orTrigger = (Or (triggerA,triggerB))
let andOrTriggerR = (And (digitalTrigger,Or (triggerA,triggerB)))
let andOrTriggerL = (And (Or (triggerA,triggerB),digitalTrigger))
let orAndTriggerR = (Or (digitalTrigger,And (triggerA,triggerB)))
let orAndTriggerL = (Or (And (triggerA,triggerB),digitalTrigger))

let testFlatten txt =
    printfn "About to %s" txt
    PicoScope.Triggering.Complex.flattenConditionTreeToStrings >> List.map (printfn "%s: %A" txt)

triggerA |> testFlatten "triggerA"
printfn "Que?"
andTrigger |> testFlatten "andTrigger"
orTrigger |> testFlatten "orTrigger"
andOrTriggerL |> testFlatten "andOrTriggerL"
andOrTriggerR |> testFlatten "andOrTriggerR"
orAndTriggerL |> testFlatten "orAndTriggerL"
orAndTriggerR |> testFlatten "orAndTriggerR"
