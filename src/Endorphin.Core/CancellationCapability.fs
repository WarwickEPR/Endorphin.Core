// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open System
open System.Threading

module CancellationCapability =
    /// Type alias for System.Threading.CancellationTokenSource.
    type CancellationCapability = CancellationTokenSource

    /// Cancellation capability with generic options which must be specified at cancellation.
    type CancellationCapability<'T>() =
        let cancellationCapability = new CancellationCapability()
        let cancellationOptions = ref None

        /// Cancels the cancellation capability after setting the specified cancellation options. Can only be
        /// called once.
        member __.Cancel options =
            match !cancellationOptions with
            | None ->
                cancellationOptions := Some options
                cancellationCapability.Cancel()
            | Some _ ->
                failwith "Cancellation has already been requested."

        /// Returns the options argument specifed at cancellation. Throws an exception if cancellation has not
        /// taken place.
        member __.Options : 'T =
            match !cancellationOptions with
            | Some options -> options
            | None -> failwith "Cannot obtain cancellation options as cancellation has not yet been requested."

        /// Checks if cancellation has already been requested.
        member __.IsCancellationRequested =
            cancellationCapability.IsCancellationRequested

        /// Returns the cancellation token.
        member __.Token =
            cancellationCapability.Token

        interface IDisposable with
            /// Disposes of the System.Threading.CancellationCapability object.
            member __.Dispose() =
                cancellationCapability.Dispose()
