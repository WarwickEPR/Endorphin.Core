namespace Endorphin.Instrument.PicoScope3000

[<AutoOpen>]
module Exception =
    /// The program received a reply from the machine that it wasn't expecting given the design documents.
    exception UnexpectedReplyException of string

    /// Use the given string to create an UnexpectedReplyException and then raise it.
    let unexpectedReply str = UnexpectedReplyException str |> raise