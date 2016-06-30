namespace Endorphin.Core

open LanguagePrimitives
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Endorphin types and functions for dealing with time-based events.
module internal Time =
    /// Time interval with unit of measure.
    [<CustomEquality; CustomComparison>]
    type Interval =
        | Interval_fs of interval : int<fs>
        | Interval_ps of interval : int<ps>
        | Interval_ns of interval : int<ns>
        | Interval_us of inverval : int<us>
        | Interval_ms of interval : int<ms>
        | Interval_s  of interval : int<s>
        with

        /// Convert the F# DU case into an int64<fs>.
        member internal this.in_fs =
            let to_fs m x = (int64 x) * (int64 m) * 1L<fs>
            match this with
            | Interval_fs interval -> interval * 1</fs> |> to_fs 1e0<fs>
            | Interval_ps interval -> interval * 1</ps> |> to_fs 1e3<fs>
            | Interval_ns interval -> interval * 1</ns> |> to_fs 1e6<fs>
            | Interval_us interval -> interval * 1</us> |> to_fs 1e9<fs>
            | Interval_ms interval -> interval * 1</ms> |> to_fs 1e12<fs>
            | Interval_s  interval -> interval * 1</s>  |> to_fs 1e15<fs>
        override x.GetHashCode() = hash x.in_fs
        override x.Equals(other) =
            match other with | :? Interval as y -> x.in_fs = y.in_fs | _ -> false
        interface System.IComparable<Interval> with
            member x.CompareTo y = compare x.in_fs y.in_fs
        interface System.IComparable with
            member x.CompareTo other =
                match other with | :? Interval as y -> compare x.in_fs y.in_fs | _ -> -1
        /// Convert an Interval to an int64<fs>.
        member this.asLong_fs  = this.in_fs
        /// Convert an Interval to an int64<ps>.
        member this.asLong_ps  = this.in_fs / 1000L<fs/ps>
        /// Convert an Interval to an int64<ns>.
        member this.asLong_ns  = this.in_fs / 1000000L<fs/ns>
        /// Convert an Interval to an int64<us>.
        member this.asLong_us  = this.in_fs / (int64 1.e9 * 1L<fs/us>)
        /// Convert an Interval to an int64<ms>.
        member this.asLong_ms  = this.in_fs / (int64 1.e12 * 1L<fs/ms>)
        /// Convert an Interval to an int64<s>.
        member this.asLong_s   = this.in_fs / (int64 1.e15 * 1L<fs/s>)

        /// Convert an Interval to an int<fs>.
        member this.asInt_fs   = (int this.asLong_fs) * 1<fs>
        /// Convert an Interval to an int<ps>.
        member this.asInt_ps   = (int this.asLong_ps) * 1<ps>
        /// Convert an Interval to an int<ns>.
        member this.asInt_ns   = (int this.asLong_ns) * 1<ns>
        /// Convert an Interval to an int<us>.
        member this.asInt_us   = (int this.asLong_us) * 1<us>
        /// Convert an Interval to an int<ms>.
        member this.asInt_ms   = (int this.asLong_ms) * 1<ms>
        /// Convert an Interval to an int<s>.
        member this.asInt_s    = (int this.asLong_s)  * 1<s>

        /// Convert an Interval to a float<fs>.
        member this.asFloat_fs = float this.in_fs
        /// Convert an Interval to a float<ps>.
        member this.asFloat_ps = float this.in_fs / 1.e3  * 1.<ps>
        /// Convert an Interval to a float<ns>.
        member this.asFloat_ns = float this.in_fs / 1.e6  * 1.<ns>
        /// Convert an Interval to a float<us>.
        member this.asFloat_us = float this.in_fs / 1.e9  * 1.<us>
        /// Convert an Interval to a float<ms>.
        member this.asFloat_ms = float this.in_fs / 1.e12 * 1.<ms>
        /// Convert an Interval to a float<s>.
        member this.asFloat_s  = float this.in_fs / 1.e15 * 1.<s>

        /// Creates an integer time interval in femtoseconds.
        static member from_fs (i : int<fs>) = Interval_fs i
        /// Creates an integer time interval in picoseconds.
        static member from_ps (i : int<ps>) = Interval_ps i

        /// Creates an integer time interval in nanoseconds.
        static member from_ns (i : int<ns>)   = Interval_ns i
        /// Create a time interval in femtoseconds, using a floating-point value in nanoseconds.
        static member fromFloat_ns (i : float<ns>) = 1.e6 * float i |> int |> (*) 1<fs> |> Interval_fs

        /// Creates an integer time interval in microseconds.
        static member from_us (i : int<us>)   = Interval_us i
        /// Create a time interval in picoseconds, using a floating-point value in microseconds.
        static member fromFloat_us (i : float<us>) = 1.e6 * float i |> int |> (*) 1<ps> |> Interval_ps

        /// Creates an integer time interval in milliseconds.
        static member from_ms (i : int<ms>) = Interval_ms i
        /// Create a time interval in nanoseconds, using a floating-point value in milliseconds.
        static member fromFloat_ms (i : float<ms>) = 1.e6 * float i |> int |> (*) 1<ns> |> Interval_ns

        /// Creates an integer time interval in seconds.
        static member from_s (i : int<s>) = Interval_s
        /// Create a time interval in microseconds, using a floating-point value in seconds.
        static member fromFloat_s (i : float<s>) = 1.e6 * float i |> int |> (*) 1<us> |> Interval_us

        override interval.ToString() =
            match interval with
            | Interval_fs i -> sprintf "%d fs" i
            | Interval_ps i -> sprintf "%d ps" i
            | Interval_ns i -> sprintf "%d ns" i
            | Interval_us i -> sprintf "%d us" i
            | Interval_ms i -> sprintf "%d ms" i
            | Interval_s  i -> sprintf "%d s"  i

    /// Frequency with unit of measure.
    [<CustomEquality; CustomComparison>]
    type Frequency =
        | Frequency_Hz  of frequency : int<Hz>
        | Frequency_kHz of frequency : int<kHz>
        | Frequency_MHz of frequency : int<MHz>
        | Frequency_GHz of frequency : int<GHz>
        with

        /// Convert the F# DU case into an int64<Hz>.
        member internal this.in_Hz =
            let to_Hz m x = (int64 x) * (int64 m) * 1L<Hz>
            match this with
            | Frequency_Hz  frequency -> frequency * 1</Hz>  |> to_Hz 1e0<Hz>
            | Frequency_kHz frequency -> frequency * 1</kHz> |> to_Hz 1e3<Hz>
            | Frequency_MHz frequency -> frequency * 1</MHz> |> to_Hz 1e6<Hz>
            | Frequency_GHz frequency -> frequency * 1</GHz> |> to_Hz 1e9<Hz>
        override x.GetHashCode() = hash x.in_Hz
        override x.Equals(other) =
            match other with | :? Frequency as y -> x.in_Hz = y.in_Hz | _ -> false
        interface System.IComparable<Frequency> with
            member x.CompareTo y = compare x.in_Hz y.in_Hz
        interface System.IComparable with
            member x.CompareTo other =
                match other with | :? Frequency as y -> compare x.in_Hz y.in_Hz | _ -> -1
        /// Convert a Frequency to an int64<Hz>.
        member this.asLong_Hz  = this.in_Hz
        /// Convert a Frequency to an int64<kHz>.
        member this.asLong_kHz = this.in_Hz / 1000L<Hz/kHz>
        /// Convert a Frequency to an int64<MHz>.
        member this.asLong_MHz = this.in_Hz / 1000000L<Hz/MHz>
        /// Convert a Frequency to an int64<GHz>.
        member this.asLong_GHz = this.in_Hz / (int64 1.e9 * 1L<Hz/GHz>)

        /// Convert a Frequency to an int<Hz>.
        member this.asInt_Hz  = (int this.asLong_Hz)  * 1<Hz>
        /// Convert a Frequency to an int<kHz>.
        member this.asInt_kHz = (int this.asLong_kHz) * 1<kHz>
        /// Convert a Frequency to an int<MHz>.
        member this.asInt_MHz = (int this.asLong_MHz) * 1<MHz>
        /// Convert a Frequency to an int<GHz>.
        member this.asInt_GHz = (int this.asLong_GHz) * 1<GHz>

        /// Convert a Frequency to a float<Hz>.
        member this.asFloat_Hz = float this.in_Hz  * 1e-0<Hz>
        /// Convert a Frequency to a float<kHz>.
        member this.asFloat_kHz = float this.in_Hz * 1e-3<kHz>
        /// Convert a Frequency to a float<MHz>.
        member this.asFloat_MHz = float this.in_Hz * 1e-6<MHz>
        /// Convert a Frequency to a float<GHz>.
        member this.asFloat_GHz = float this.in_Hz * 1e-9<GHz>

        /// Creates an integer frequency in hertz.
        static member from_Hz (i : int<Hz>) = Frequency_Hz i
        /// Creates an integer frequency in kilohertz.
        static member from_kHz (i : int<kHz>) = Frequency_kHz i

        /// Creates an integer frequency in megahertz.
        static member from_MHz (i : int<MHz>)   = Frequency_MHz i
        /// Create a frequency in hertz, using a floating-point value in megaseconds.
        static member fromFloat_MHz (i : float<MHz>) = 1e6 * float i |> int |> (*) 1<Hz> |> Frequency_Hz

        /// Creates an integer frequency in gigahertz.
        static member from_GHz (i : int<GHz>)   = Frequency_GHz i
        /// Create a frequency in kilohertz, using a floating-point value in gigaseconds.
        static member fromFloat_GHz (i : float<GHz>) = 1e6 * float i |> int |> (*) 1<kHz> |> Frequency_kHz

        override interval.ToString() =
            match interval with
            | Frequency_Hz i -> sprintf "%d Hz" i
            | Frequency_kHz i -> sprintf "%d kHz" i
            | Frequency_MHz i -> sprintf "%d MHz" i
            | Frequency_GHz i -> sprintf "%d GHz" i
