namespace Endorphin.Instrument.PicoScope5000

type SignalGeneratorWaveType =
    | Sine = 0
    | Square = 1
    | Triangle = 2
    | RampUp = 3
    | RampDown = 4
    | Sinc = 5
    | Gaussian = 6
    | HalfSine = 7
    | DcVoltage = 8
    | MAX_WAVE_TYPES = 9

type SignalGeneratorSweepType = 
    | Up = 0
    | Down = 1
    | UpDown = 2
    | DownUp = 3

type SignalGeneratorExtras =
    | None = 0
    | WhiteNoise = 1
    | PseudoRandomBitStream = 2

type SignalGeneratorTriggerType =
    | Rising = 0
    | Falling = 1
    | GateHigh = 2
    | GateLow = 3

type SignalGeneratorTriggerSource =
    | None = 0
    | Scope = 1
    | Auxiliary = 2
    | External = 3
    | Software = 4

type SignalGeneratorIndexMode =
    | Single = 0
    | Dual = 1
    | Quad = 2
