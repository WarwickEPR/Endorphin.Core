// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Units =

    [<AutoOpen>]
    module Time =

        /// Femtoseconds.
        [<Measure>] type fs

        /// Picoseconds.
        [<Measure>] type ps

        /// Nanoseconds.
        [<Measure>] type ns

        /// Microseconds.
        [<Measure>] type us

        /// Milliseconds.
        [<Measure>] type ms

        let secondsPerMillisecond =  1.0e-3<s/ms>
        let secondsPerMicrosecond =  1.0e-6<s/us>
        let secondsPerNanosecond  =  1.0e-9<s/ns>
        let secondsPerPicosecond  = 1.0e-12<s/ps>
        let secondsPerFemtosecond = 1.0e-15<s/fs>
        
        let millisecondsPerSecond      =   1.0e3<ms/s>
        let millisecondsPerMicrosecond =  1.0e-3<ms/us>
        let millisecondsPerNanosecond  =  1.0e-6<ms/ns>
        let millisecondsPerPicosecond  =  1.0e-9<ms/ps>
        let millisecondsPerFemtosecond = 1.0e-12<ms/fs>

        let microsecondsPerSecond      =  1.0e6<us/s>
        let microsecondsPerMillisecond =  1.0e3<us/ms>
        let microsecondsPerNanosecond  = 1.0e-3<us/ns>
        let microsecondsPerPicosecond  = 1.0e-6<us/ps>
        let microsecondsPerFemtosecond = 1.0e-9<us/fs>

        let nanosecondsPerSecond       =  1.0e9<ns/s>
        let nanosecondsPerMillisecond  =  1.0e6<ns/ms>
        let nanosecondsPerMicrosecond  =  1.0e3<ns/us>
        let nanosecondsPerPicosecond   = 1.0e-3<ns/ps>
        let nanosecondsPerFemtosecond  = 1.0e-6<ns/fs>

        let picosecondsPerSecond       = 1.0e12<ps/s>
        let picosecondsPerMillisecond  =  1.0e9<ps/ms>
        let picosecondsPerMicrosecond  =  1.0e6<ps/us>
        let picosecondsPerNanosecond   =  1.0e3<ps/ns>
        let picosecondsPerFemtosecond  = 1.0e-3<ps/fs>
        
        let femtosecondsPerSecond      = 1.0e15<fs/s>
        let femtosecondsPerMillisecond = 1.0e12<fs/ms>
        let femtosecondsPerMicrosecond =  1.0e9<fs/us>
        let femtosecondsPerNanosecond  =  1.0e6<fs/ns>
        let femtosecondsPerPicosecond  =  1.0e3<fs/ps>

        module Seconds =
            let toMilliseconds (t : float<s>) = t * millisecondsPerSecond
            let toMicroseconds (t : float<s>) = t * microsecondsPerSecond
            let toNanoseconds  (t : float<s>) = t * nanosecondsPerSecond
            let toPicoseconds  (t : float<s>) = t * picosecondsPerSecond
            let toFemtoseconds (t : float<s>) = t * femtosecondsPerSecond

        module Milliseconds =
            let toSeconds      (t : float<ms>) = t * secondsPerMillisecond
            let toMicroseconds (t : float<ms>) = t * microsecondsPerMillisecond
            let toNanoseconds  (t : float<ms>) = t * nanosecondsPerMillisecond
            let toPicoseconds  (t : float<ms>) = t * picosecondsPerMillisecond
            let toFemtoseconds (t : float<ms>) = t * femtosecondsPerMillisecond

        module Microseconds =
            let toSeconds      (t : float<us>) = t * secondsPerMicrosecond
            let toMilliseconds (t : float<us>) = t * millisecondsPerMicrosecond
            let toNanoseconds  (t : float<us>) = t * nanosecondsPerMicrosecond
            let toPicoseconds  (t : float<us>) = t * picosecondsPerMicrosecond
            let toFemtoseconds (t : float<us>) = t * femtosecondsPerMicrosecond

        module Nanoseconds =
            let toSeconds      (t : float<ns>) = t * secondsPerNanosecond
            let toMilliseconds (t : float<ns>) = t * millisecondsPerNanosecond
            let toMicroseconds (t : float<ns>) = t * microsecondsPerNanosecond
            let toPicoseconds  (t : float<ns>) = t * picosecondsPerNanosecond
            let toFemtoseconds (t : float<ns>) = t * femtosecondsPerNanosecond

        module Picoseconds =
            let toSeconds      (t : float<ps>) = t * secondsPerPicosecond
            let toMilliseconds (t : float<ps>) = t * millisecondsPerPicosecond
            let toMicroseconds (t : float<ps>) = t * microsecondsPerPicosecond
            let toNanoseconds  (t : float<ps>) = t * nanosecondsPerPicosecond
            let toFemtoseconds (t : float<ps>) = t * femtosecondsPerPicosecond
            
        module Femtoseconds =
            let toSeconds      (t : float<fs>) = t * secondsPerFemtosecond
            let toMilliseconds (t : float<fs>) = t * millisecondsPerFemtosecond
            let toMicroseconds (t : float<fs>) = t * microsecondsPerFemtosecond
            let toNanoseconds  (t : float<fs>) = t * nanosecondsPerFemtosecond
            let toPicoseconds  (t : float<fs>) = t * picosecondsPerFemtosecond

    /// Binary bits.
    [<Measure>] type bit
    
    /// Number of digital steps.
    [<Measure>] type step

    /// Decimal places.
    [<Measure>] type dp

    /// Significant figures.
    [<Measure>] type sf

    /// dBm i.e. decibels with respect to 1 mW of power
    [<Measure>] type dBm

    /// dB decibels
    [<Measure>] type dB

    /// Hz (cycles per second)
    [<Measure>] type Hz = 1/s

    /// kHz (cycles per millisecond)
    [<Measure>] type kHz = 1/ms

    /// MHz (cycles per microsecond)
    [<Measure>] type MHz = 1/us

    /// GHz (cycles per nanosecond)
    [<Measure>] type GHz = 1/ns

    /// Percent
    [<Measure>] type pct
    let fractionToPercentage fraction   = fraction * 100.0<pct>
    let percentageToFraction percentage = percentage / 100.0<pct>

    /// Degrees
    [<Measure>] type deg

    /// Radians
    [<Measure>] type rad

    let radiansToDegrees angle = angle * (180.0<deg/rad> / System.Math.PI)
    let degreesToRadians angle = angle * (System.Math.PI / 180.0<deg/rad>)

   
    [<AutoOpen>]
    module Voltage = 
        [<Measure>] type uV
        [<Measure>] type mV

        let voltsPerMilliVolt      = 1.0e-3<V/mV>
        let voltsPerMicroVolt      = 1.0e-6<V/uV>

        let millivoltsPerVolt      = 1.0e3<mV/V>
        let millivoltsPerMicroVolt = 1.0e-3<mV/uV>

        let microvoltsPerVolt      = 1.0e6<uV/V>
        let microvoltsPerMilliVolt = 1.0e3<uV/mV>

        module Volts = 
            let toMillivolts (v : float<V>) = v * millivoltsPerVolt 
            let toMicrovolts (v : float<V>) = v * microvoltsPerVolt

        module Millivolts = 
            let toVolts      (v : float<mV>) = v * voltsPerMilliVolt 
            let toMicrovolts (v : float<mV>) = v * microvoltsPerMilliVolt
                                             
        module Microvolts =                  
            let toVolts      (v : float<uV>) = v * voltsPerMicroVolt
            let toMillivolts (v : float<uV>) = v * millivoltsPerMicroVolt