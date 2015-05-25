namespace Endorphin.Instrument.PicoScope5000

// enumerations in this file are present for completeness but not used in any of the PicoScope API
// features implemented so far

[<AutoOpen>]
module SignalGenerator = 
    type SignalGeneratorWaveTypeEnum =
        | Sine      = 0
        | Square    = 1
        | Triangle  = 2
        | RampUp    = 3
        | RampDown  = 4
        | Sinc      = 5
        | Gaussian  = 6
        | HalfSine  = 7
        | DcVoltage = 8
    
    type SignalGeneratorSweepTypeEnum = 
        | Up     = 0
        | Down   = 1
        | UpDown = 2
        | DownUp = 3
    
    type SignalGeneratorExtrasEnum =
        | None                  = 0
        | WhiteNoise            = 1
        | PseudoRandomBitStream = 2
    
    type SignalGeneratorTriggerTypeEnum =
        | Rising   = 0
        | Falling  = 1
        | GateHigh = 2
        | GateLow  = 3
    
    type SignalGeneratorTriggerSourceEnum =
        | None      = 0
        | Scope     = 1
        | Auxiliary = 2
        | External  = 3
        | Software  = 4
    
    type SignalGeneratorIndexModeEnum =
        | Single = 0
        | Dual   = 1
        | Quad   = 2