module Units

// Current, amps
[<Measure>] type A
let amps current = current * 1.0<A>

// Voltage, volts
[<Measure>] type V
let volts voltage = voltage * 1.0<V>

// Time units
[<Measure>] type s // seconds
[<Measure>] type ms // milliseconds
[<Measure>] type us // microseconds
[<Measure>] type ns // nanoseconds
[<Measure>] type fs // femtoseconds

// Time initialisation functions
let seconds t = t * 1.0<s>
let milliseconds t = t * 1.0<ms>
let microseconds t = t * 1.0<us>
let nanoseconds t = t * 1.0<ns>
let femtoseconds t = t * 1.0<fs> 
let perSecond f = f * 1.0<1/s>

// Time conversion contsants
let millisecondsPerSecond = 1.0e3<ms/s>
let microsecondsPerSecond = 1.0e6<s/us>
let nanosecondsPerSecond = 1.0e9<s/ns>
let femtosecondsPerSecond = 1.0e15<s/fs>
let microsecondsPerMillisecond = 1.0e3<us/ms>
let nanosecondsPerMillisecond = 1.0e6<ns/ms>
let femtosecondsPerMillisecond = 1.0e12<fs/ms>
let nanosecondsPerMicrosecond = 1.0e3<ns/us>
let femtosecondsPerMicrosecond = 1.0e9<fs/us>
let femtosecondsPerNanosecond = 1.0e6<fs/ns>

// Composite unit initialisation functions
let ampsPerSecond r = (amps >> perSecond) r