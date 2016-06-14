// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.PicoScope3000

[<AutoOpen>]
module Exception =
    /// The program received a reply from the machine that it wasn't expecting given the design documents.
    exception UnexpectedReplyException of string

    /// Use the given string to create an UnexpectedReplyException and then raise it.
    let unexpectedReply str = UnexpectedReplyException str |> raise