module SligningPoop

// This is a simulated cancellable computation. It checks the token source 
// to see whether the cancel signal was received. 
let computation (tokenSource:System.Threading.CancellationTokenSource) =
    async {
        use! cancelHandler = Async.OnCancel(fun () -> 
            printfn "Canceling operation from %A." (System.Threading.Thread.CurrentThread.ManagedThreadId))
        // Async.Sleep checks for cancellation at the end of the sleep interval, 
        // so loop over many short sleep intervals instead of sleeping 
        // for a long time. 
        while true do 
            printfn "Still doing stuff on %A." (System.Threading.Thread.CurrentThread.ManagedThreadId)
            do! Async.Sleep(100)
    }

let tokenSource1 = new System.Threading.CancellationTokenSource()
let tokenSource2 = new System.Threading.CancellationTokenSource()

Async.Start(computation tokenSource1, tokenSource1.Token)
Async.Start(computation tokenSource2, tokenSource2.Token)
printfn "Started computations from %A." (System.Threading.Thread.CurrentThread.ManagedThreadId)
System.Threading.Thread.Sleep(1000)
printfn "Sending cancellation signal from %A." (System.Threading.Thread.CurrentThread.ManagedThreadId)
tokenSource1.Cancel()
tokenSource2.Cancel()

// Wait for user input to prevent application termination.
System.Console.ReadLine() |> ignore