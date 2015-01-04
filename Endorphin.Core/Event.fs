namespace Endorphin.Core

[<AutoOpen>]
module Event =
    let concatSeq (source: IEvent<'a seq>) =
        let event = new Event<_>()
        source |> Event.add (fun xs -> for x in xs do event.Trigger x)
        event.Publish

    let collectSeq f =
        Event.map f >> concatSeq