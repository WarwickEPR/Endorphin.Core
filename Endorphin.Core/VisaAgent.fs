namespace Endorphin.Core

open NationalInstruments.VisaNS
open System
open log4net

[<AutoOpen>]
module VisaAgent =
    [<AutoOpen>]
    module Model =
        type internal ReplyChannel<'T> = AsyncReplyChannel<Choice<'T, exn>>

        type internal VisaMessage =
            | Read   of replyChannel : ReplyChannel<ValueType>
            | Write  of command : Choice<unit, exn>
            | Query  of command : Choice<unit, exn> * replyChannel : ReplyChannel<ValueType>
            | Close