namespace Endorphin.Core.Async

open System.Threading

type CancellationCapability = CancellationTokenSource

type CancellationCapability<'Options>() =
    let cancellationCapability = new CancellationCapability()
    let cancellationOptions = ref None
     
    member this.Options =
        match !cancellationOptions with
        | Some(options) -> options
        | None -> failwith "No cancellation options are specified as cancellation has not been requested."

    member this.Cancel (options : 'Options) =
        cancellationOptions := Some(options)
        cancellationCapability.Cancel()

    member this.IsCancellationRequested =
        cancellationCapability.IsCancellationRequested

    member this.Token =
        cancellationCapability.Token
