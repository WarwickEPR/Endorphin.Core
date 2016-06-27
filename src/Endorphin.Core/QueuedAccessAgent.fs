// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

open System

[<AutoOpen>]
module QueuedAccessAgent =
    type Agent<'T> = MailboxProcessor<'T>

    type DisposableBox<'T>(contents : 'T, disposeAction) =
        let mutable disposed = false

        member __.Contents =
            if disposed
            then failwith "Cannot access disposable box contents after it has been disposed."
            else contents

        interface System.IDisposable with
            member __.Dispose() =
                disposed <- true
                disposeAction()

    type private Message<'T> =
        | RequestControl of replyChannel : AsyncReplyChannel<DisposableBox<'T>>
        | RelinquishControl
        | Stop

    type QueuedAccessAgent<'T> =
        private QueuedAccessAgent of agent : Agent<Message<'T>>
        with interface System.IDisposable with
                member agent.Dispose () =
                    let (QueuedAccessAgent agent') = agent
                    Stop |> agent'.Post

    let create payload = QueuedAccessAgent <| Agent.Start(fun mailbox ->
        let rec waiting () = async {
            let! message = mailbox.Receive ()
            match message with
            | RequestControl replyChannel ->
                replyChannel.Reply
                    (new DisposableBox<_>(payload, (fun () -> mailbox.Post RelinquishControl)))

                return! inUse ()
            | RelinquishControl ->
                failwith "Unexpected message: relinquish control."
            | Stop ->
                if mailbox.CurrentQueueLength <> 0
                then failwith "Stopped queued access agent when message queue was non-empty."
                else return () }

        and inUse () = async {
            do! mailbox.Scan (function
                | RelinquishControl -> Some (async { return () })
                | _                 -> None)
            return! waiting () }


        waiting ())

    let requestControl (QueuedAccessAgent agent) = agent.PostAndAsyncReply RequestControl