// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Core

/// Functions for working with SCPI commands and instruments.
module SCPI =
    /// An instrument capable of receiving and acting on SCPI commands.
    type IScpiInstrument =
        /// Write an array of bytes to a SCPI instrument asynchronously,
        /// expecting no response.
        abstract member Write : value : byte [] -> Async<unit>
        /// Write an array of bytes to a SCPI instrument, then asynchronously wait
        /// for a response as an ASCII string.
        abstract member Query : query : byte [] -> Async<byte []>
        /// The line termination characters to use.
        abstract member Terminator : byte [] with get, set

    /// Types which interface IScpiFormatable implement a member ToScpiString ()
    /// which is analogous to obj.ToString (), but specifically for the SCPI interface,
    /// since the two strings may well be different.
    type IScpiFormatable =
        /// Get the SCPI string representation of this type.
        abstract member ToScpiString : unit -> string

    /// A SCPI error returned from a machine.  Negative error codes are defined
    /// in the SCPI specification, while positive ones are device-dependant.
    type Error = {
        Code    : int
        Message : string }

    /// The identity structure returned from a SCPI instrument in response to
    /// the "*IDN?" query.
    type Identity = {
        Manufacturer : string
        Model        : string
        Serial       : string
        Version      : string }

    /// The machine returned an invalid response to a query.
    exception InvalidResponseException of string

    /// There were errors in the error queue after calling a SCPI.Checked function.
    exception InstrumentErrorException of Error list

    /// Trim any leading or ending whitespace from a string.
    let private trim (str : string) = str.Trim ()
    /// Convert a .NET string into an array of bytes.
    let private bytes (str : string) = System.Text.ASCIIEncoding.ASCII.GetBytes str
    /// Convert an array of bytes into a .NET string.
    let private utf8 bytes = System.Text.ASCIIEncoding.ASCII.GetString bytes
    /// Hex encode and truncate an array of bytes for display as a string of length <= n
    let private displayBytes n arr =
        let bytes = n/2
        if Array.length arr > bytes then
            Array.truncate (bytes-1) arr |> String.hexOfBytes |> sprintf "%s.."
        else
            String.hexOfBytes arr

    /// The logger for the SCPI module.
    let private logger = Log.named "SCPI"

    /// Log a message on the logger at DEBUG level, if it's enabled for a write event.
    let private wdebugf func msg =
        if not logger.IsDebugEnabled then ()
        else
            Log.debugf logger func "Writing bytes: %A" msg
            Log.debugf logger func "...  and UTF8: %s" <| utf8 msg

    /// Log a message on the logger at DEBUG level, if it's enabled for a received event.
    let private rdebugf func msg =
        if not logger.IsDebugEnabled then ()
        else
            Log.debugf logger func "Received bytes: %A" msg
            Log.debugf logger func "...   and UTF8: %s" <| utf8 msg

    /// Convert an object to a string using its IScpiFormatable interface if it
    /// implements it, or its System.Object.ToString () method if not.
    let format (value : obj) =
        match value with
        | :? IScpiFormatable as scpi -> scpi.ToScpiString ()
        | :? (byte[]) as bytes -> displayBytes 30 bytes
        | obj -> obj.ToString ()

    /// Keys for use with SCPI commands.
    module Key =
        /// SCPI string to query an instrument for its identity, without the trailing
        /// question mark (for use with String.query or Query.identity).
        [< Literal >]
        let identify = "*IDN"
        /// SCPI string to instruct an instrument to reset.
        [< Literal >]
        let reset    = "*RST"
        /// SCPI string to instruct an instrument to clear its status.
        [< Literal >]
        let clear    = "*CLS"
        /// SCPI string to query the error queue for the next error.
        [< Literal >]
        let error    = ":SYSTEM:ERROR"

    /// Functions for building SCPI commands into ASCII byte arrays for sending to
    /// machines.
    module String =
        // TODO: there must be a better way than type dispatching here, but I didn't
        // want to have two separate functions.

        /// Add a value onto a currently existing key or query, where the text
        /// representation of the value is found by calling its ToSCPIString()
        /// method if it has one (i.e., implements IScpiFormatable), or ToString ()
        /// if it does not.
        ///
        /// Byte arrays are writing in as ASCII strings verbatim, with no ToString()
        /// conversion, unless they implement a IScpiFormatable with a different
        /// ToScpiString() method.
        let private addValue (value : obj) str =
            match value with
            | :? IScpiFormatable as scpi ->
                String.concat " " [ str ; scpi.ToScpiString () ] |> bytes
            | :? (byte []) as arr ->
                Array.concat [ bytes (str + " ") ; arr ]
            | _ ->
                String.concat " " [ str ; value.ToString () ] |> bytes

        /// Create ASCII strings to set keys to values.
        module Set =
            /// Create the string to set a given key to a given value, where the value to be
            /// written is found by calling its ToScpiString() method if it implements
            /// IScpiFormatable, verbatim if a byte array, or its obj.ToString() method
            /// otherwise.
            let value key value = trim key |> addValue value

            /// Create the string for a key and no value.
            let key key = trim key |> bytes

        /// Create ASCII strings to query the instrument with keys and values.
        module Query =
            /// Create the string to query a given key with the given value, where the value
            /// string is found by calling its ToScpiString() method if it implements
            /// IScpiFormatable, verbatim if a byte array, or its obj.ToString() method
            /// otherwise.
            let value key value = trim key + "?" |> addValue value

            /// Create the string to query a key with no value.
            let key key = trim key + "?" |> bytes

        /// Append one SCPI command to another.  The command passed as the first argument
        /// ("before"), will be before the command passed as the second argument ("after").
        /// For use in F# pipes, see SCPI.String.append.
        let join before after = Array.concat [ before ; ";"B ; after ]
        /// Append one SCPI command to another.  The command passed as the first argument
        /// ("after"), will be after the command passed as the second argument ("before").
        /// This is intended for F# piping.  For the other order, see SCPI.String.join.
        let append after before = join before after
        /// Concatenate a list of SCPI commands into one single ASCII string that may be
        /// sent to a machine in one go.
        let concat commands =
            let mutable pos = 0
            let out =
                commands
                |> Seq.fold (fun x -> Array.length >> (+) x) (Seq.length commands - 1)
                |> Array.zeroCreate<byte>
            let action command =
                let len = Array.length command
                out.[pos .. pos + len - 1] <- command
                if pos + len < Array.length out then
                    out.[pos + len] <- ';'B
                    pos <- pos + len + 1
            Seq.iter action commands
            out

    /// Add the instrument's terminator to the byte array.
    let private addTerminator (instrument : IScpiInstrument) arr =
        match instrument.Terminator with
        | [||]  -> arr
        | value -> Array.append arr value

    /// Convert a block of data into the SCPI datablock format by prepending the datablock
    /// character, the number of digits in the length and the length.
    let datablock (data : byte []) =
        let ndigits x =
            if   x >  999999999 then failwithf "Can't make a data block more than or equal to 1 billion bytes."
            elif x >  99999999  then "#9"B
            elif x >  9999999   then "#8"B
            elif x >  999999    then "#7"B
            elif x >  99999     then "#6"B
            elif x >  9999      then "#5"B
            elif x >  999       then "#4"B
            elif x >  99        then "#3"B
            elif x >  9         then "#2"B
            elif x >= 0         then "#1"B
            else failwithf "Found an array of negative length! %A" data
        let length = Array.length data
        let digits = ndigits length
        Array.concat [ digits ; string length |> bytes ; data ]

    /// Functions for writing "set" commands to SCPI instruments with no expected responses,
    /// e.g., "*RST".
    module Set =
        /// Send a byte array to the instrument verbatim, with no preprocessing.
        let verbatim str (instrument : IScpiInstrument) =
            wdebugf "Set.verbatim" str
            instrument.Write str

        /// Generic write function, which adds a terminator to the byte array, then sends
        /// it to the instrument.
        let private writer instrument = addTerminator instrument >> (fun x -> verbatim x instrument)

        /// Write a key with a value to an instrument, where the value will be converted
        /// by calling its ToScpiString() method if it implements IScpiFormatable,
        /// verbatim if it is a byte array, or its obj.ToString() method otherwise.
        ///
        /// SCPI command e.g., ":POWER 12.0".
        let value<'In> key (value : 'In) instrument = String.Set.value key value |> writer instrument

        /// Write a key with no value to an instrument, e.g., "*RST".
        let key   key instrument = String.Set.key key |> writer instrument

        /// Issue the reset command to the instrument.
        let reset instrument = key Key.reset instrument
        /// Issue the clear state command to the instrument.
        let clear instrument = key Key.clear instrument

    /// Functions for querying SCPI instruments, expecting an asynchronus response.
    module Query =
        /// Send a byte array to the instrument verbatim, and asynchronously await its
        /// response.  No additional processing happens at either end.
        let verbatim str (instrument : IScpiInstrument) = async {
            do wdebugf "Query.verbatim" str
            let! response = instrument.Query str
            do rdebugf "Query.verbatim" response
            return response }

        /// Generic query function for internal use - adds the instrument's terminator
        /// then submits the block.
        let private querier instrument = addTerminator instrument >> (fun x -> verbatim x instrument)

        /// Functions to query SCPI instruments where the query takes some value as a parameter,
        /// e.g., ":FILE:EXISTS? 'testfile.out'".
        module Value =
            /// Query the instrument with a given key and value, then return the raw ASCII
            /// string read from the machine.  The `data` type is converted to a string by
            /// calling its ToScpiString () method if it implements IScpiFormatable,
            /// verbatim if it is a byte array, or its obj.ToString () method otherwise.
            let raw<'In> key (data : 'In) instrument = String.Query.value key data |> querier instrument

            /// Query the isntrument with a given key and value, then parse the result from
            /// a UTF-8 string using the passed parser.
            let parsed<'In, 'Out> (parser : string -> 'Out) key (data : 'In) instrument = raw key data instrument |> Async.map (utf8 >> parser)

            // We can't use complete partial application here because data and ScpiInstrument
            // can be generalised, and so the compiler cannot tell if it is safe to totally
            // generalise the value into closure creation.
            //
            // See: https://blogs.msdn.microsoft.com/mulambda/2010/05/01/finer-points-of-f-value-restriction/
            // for more details.

            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "string".
            let string key data = parsed string key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "int8".
            let int8 key data = parsed int8 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "int16".
            let int16 key data = parsed int16 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "int32".
            let int32 key data = parsed int32 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "int64".
            let int64 key data = parsed int64 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "uint8".
            let uint8 key data = parsed uint8 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "uint16".
            let uint16 key data = parsed uint16 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "uint32".
            let uint32 key data = parsed uint32 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "uint64".
            let uint64 key data = parsed uint64 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "char".
            let char key data = parsed char key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "byte".
            let byte key data = parsed byte key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "sbyte".
            let sbyte key data = parsed sbyte key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "decimal".
            let decimal key data = parsed decimal key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "float32".
            let float32 key data = parsed float32 key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "float".
            let float key data = parsed float key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "single".
            let single key data = parsed single key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "double".
            let double key data = parsed double key data
            /// Query the instrument with a given key and value, then parse the result into
            /// the primitive .NET type "bool".
            let bool key data = parsed bool.Parse key data

        /// Functions for querying SCPI instruments by key only, e.g., ":SYSTEM:ERROR?".
        module Key =
            /// Query the instrument with a given key, then return the raw ASCII
            /// string read from the machine.
            let raw key instrument = String.Query.key key |> querier instrument

            /// Query the isntrument with a given key, then parse the result from
            /// a UTF-8 string using the passed parser.
            let parsed<'Out> (parser : string -> 'Out) key instrument = raw key instrument |> Async.map (utf8 >> parser)

            // We can't use complete partial application here because ScpiInstrument
            // can be generalised, and so the compiler cannot tell if it is safe to totally
            // generalise the value into closure creation.
            //
            // See: https://blogs.msdn.microsoft.com/mulambda/2010/05/01/finer-points-of-f-value-restriction/
            // for more details.

            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "string".
            let string key = parsed string key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "int8".
            let int8 key = parsed int8 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "int16".
            let int16 key = parsed int16 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "int32".
            let int32 key = parsed int32 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "int64".
            let int64 key = parsed int64 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "uint8".
            let uint8 key = parsed uint8 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "uint16".
            let uint16 key = parsed uint16 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "uint32".
            let uint32 key = parsed uint32 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "uint64".
            let uint64 key = parsed uint64 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "char".
            let char key = parsed char key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "byte".
            let byte key = parsed byte key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "sbyte".
            let sbyte key = parsed sbyte key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "decimal".
            let decimal key = parsed decimal key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "float32".
            let float32 key = parsed float32 key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "float".
            let float key = parsed float key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "single".
            let single key = parsed single key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "double".
            let double key = parsed double key
            /// Query the instrument with a given key, then parse the result into
            /// the primitive .NET type "bool".
            let bool key = parsed bool.Parse key

        /// Get a single error from the error queue.
        ///
        /// Throws SCPI.InvalidResponseException if the response does not match the SCPI spec.
        let private error instrument =
            let parser (str : string) =
                let  parts = str.Split ([| ',' |], 2) |> Array.map trim
                try  { Code = int parts.[0] ; Message = parts.[1] }
                with _ -> raise (InvalidResponseException str)
            Key.parsed parser Key.error instrument

        /// Check the queue for errors, returning None if no errors are found or a Some value
        /// with the entire sequence of errors if some exist.
        let errors instrument =
            let rec loop errors = async {
                let! next = error instrument
                if next.Code <> 0 then return! loop (next :: errors)
                else
                    match errors with
                    | []   -> return None
                    | list -> return List.rev list |> Some }
            loop []

        /// Get the identity of an instrument.
        ///
        /// Throws SCPI.InvalidResponseException if the response does not match the SCPI spec.
        let identity instrument =
            let parser (str : string) =
                let  parts = str.Split ([| ',' |], 4) |> Array.map trim
                try
                    { Manufacturer = parts.[0]
                      Model        = parts.[1]
                      Serial       = parts.[2]
                      Version      = parts.[3] }
                with _ -> raise (InvalidResponseException str)
            Key.parsed parser Key.identify instrument

    /// IO SCPI functions which check the error queue afterwards and raise an exception if any
    /// were found.
    module Checked =
        /// Check the error queue and raise an exception if any are found.
        let private errors instrument = async {
            let! errors = Query.errors instrument
            match errors with
            | None -> ()
            | Some failures -> InstrumentErrorException failures |> raise }

        /// Functions for writing "set" commands to SCPI instruments with no expected responses,
        /// e.g., "*RST".
        module Set =
            /// Send a byte array to the instrument verbatim, with no preprocessing.
            let verbatim str (instrument : IScpiInstrument) = async {
                do wdebugf "Checked.Set.verbatim" str
                do! instrument.Write str
                do! errors instrument }

            /// Generic write function, which adds a terminator to the byte array, then sends
            /// it to the instrument.
            let private writer instrument = addTerminator instrument >> (fun x -> verbatim x instrument)

            /// Write a key with a value to an instrument, where the value will be converted
            /// by calling its ToScpiString() method if it implements IScpiFormatable,
            /// verbatim if it is a byte array, or its obj.ToString() method otherwise.
            ///
            /// SCPI command e.g., ":POWER 12.0".
            let value<'In> key (value : 'In) instrument = String.Set.value key value |> writer instrument

            /// Write a key with no value to an instrument, e.g., "*RST".
            let key   key instrument = String.Set.key key |> writer instrument

            /// Issue the reset command to the instrument.
            let reset instrument = key Key.reset instrument
            /// Issue the clear state command to the instrument.
            let clear instrument = key Key.clear instrument

        /// Functions for querying SCPI instruments, expecting an asynchronus response.
        module Query =
            // Literally just a passthrough to the private function declared earlier, but in the
            // correct module.
            /// Raise an exception if any errors are present in the queue.
            let errors instrument = errors instrument

            /// Send a byte array to the instrument verbatim, and asynchronously await its
            /// response.  No additional processing happens at either end.
            let verbatim str (instrument : IScpiInstrument) = async {
                do wdebugf "Checked.Query.verbatim" str
                let! result = instrument.Query str
                do rdebugf "Checked.Query.verbatim" result
                do! errors instrument
                return result }

            /// Generic query function for internal use - adds the instrument's terminator
            /// then submits the block.
            let private querier instrument = addTerminator instrument >> (fun x -> verbatim x instrument)

            /// Functions to query SCPI instruments where the query takes some value as a parameter,
            /// e.g., ":FILE:EXISTS? 'testfile.out'".
            module Value =
                /// Query the instrument with a given key and value, then return the raw ASCII
                /// string read from the machine.  The `data` type is converted to a string by
                /// calling its ToScpiString () method if it implements IScpiFormatable,
                /// verbatim if it is a byte array, or its obj.ToString () method otherwise.
                let raw<'In> key (data : 'In) instrument = String.Query.value key data |> querier instrument

                /// Query the isntrument with a given key and value, then parse the result from
                /// a UTF-8 string using the passed parser.
                let parsed<'In, 'Out> (parser : string -> 'Out) key (data : 'In) instrument = raw key data instrument |> Async.map (utf8 >> parser)

                // We can't use complete partial application here because data and ScpiInstrument
                // can be generalised, and so the compiler cannot tell if it is safe to totally
                // generalise the value into closure creation.
                //
                // See: https://blogs.msdn.microsoft.com/mulambda/2010/05/01/finer-points-of-f-value-restriction/
                // for more details.

                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "string".
                let string key data = parsed string key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "int8".
                let int8 key data = parsed int8 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "int16".
                let int16 key data = parsed int16 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "int32".
                let int32 key data = parsed int32 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "int64".
                let int64 key data = parsed int64 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "uint8".
                let uint8 key data = parsed uint8 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "uint16".
                let uint16 key data = parsed uint16 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "uint32".
                let uint32 key data = parsed uint32 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "uint64".
                let uint64 key data = parsed uint64 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "char".
                let char key data = parsed char key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "byte".
                let byte key data = parsed byte key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "sbyte".
                let sbyte key data = parsed sbyte key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "decimal".
                let decimal key data = parsed decimal key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "float32".
                let float32 key data = parsed float32 key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "float".
                let float key data = parsed float key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "single".
                let single key data = parsed single key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "double".
                let double key data = parsed double key data
                /// Query the instrument with a given key and value, then parse the result into
                /// the primitive .NET type "bool".
                let bool key data = parsed bool.Parse key data

            /// Functions for querying SCPI instruments by key only, e.g., ":SYSTEM:ERROR?".
            module Key =
                /// Query the instrument with a given key, then return the raw ASCII
                /// string read from the machine.
                let raw key instrument = String.Query.key key |> querier instrument

                /// Query the isntrument with a given key, then parse the result from
                /// a UTF-8 string using the passed parser.
                let parsed<'Out> (parser : string -> 'Out) key instrument = raw key instrument |> Async.map (utf8 >> parser)

                // We can't use complete partial application here because ScpiInstrument
                // can be generalised, and so the compiler cannot tell if it is safe to totally
                // generalise the value into closure creation.
                //
                // See: https://blogs.msdn.microsoft.com/mulambda/2010/05/01/finer-points-of-f-value-restriction/
                // for more details.

                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "string".
                let string key = parsed string key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "int8".
                let int8 key = parsed int8 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "int16".
                let int16 key = parsed int16 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "int32".
                let int32 key = parsed int32 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "int64".
                let int64 key = parsed int64 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "uint8".
                let uint8 key = parsed uint8 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "uint16".
                let uint16 key = parsed uint16 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "uint32".
                let uint32 key = parsed uint32 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "uint64".
                let uint64 key = parsed uint64 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "char".
                let char key = parsed char key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "byte".
                let byte key = parsed byte key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "sbyte".
                let sbyte key = parsed sbyte key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "decimal".
                let decimal key = parsed decimal key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "float32".
                let float32 key = parsed float32 key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "float".
                let float key = parsed float key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "single".
                let single key = parsed single key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "double".
                let double key = parsed double key
                /// Query the instrument with a given key, then parse the result into
                /// the primitive .NET type "bool".
                let bool key = parsed bool.Parse key

            /// Get the identity of an instrument.
            ///
            /// Throws SCPI.InvalidResponseException if the response does not match the SCPI spec.
            let identity instrument =
                let parser (str : string) =
                    let  parts = str.Split ([| ',' |], 4) |> Array.map trim
                    try
                        { Manufacturer = parts.[0]
                          Model        = parts.[1]
                          Serial       = parts.[2]
                          Version      = parts.[3] }
                    with _ -> raise (InvalidResponseException str)
                Key.parsed parser Key.identify instrument
