namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module Parsing = 
       
    /// Takes first value from a tuple. 
    let private first = function
        | (x, y, z) -> x     
    
    /// Takes second value from a tuple    
    let private second = function 
        | (x, y, z) -> y
    
    /// Takes thirs value from a tuple
    let private third = function 
        | (x, y, z) -> z
   
    let optiontoValue = function
        | Some (x:float32, y:float32, z:float32) -> (x, y, z)   
        | None -> failwithf "Cannot have coordinate None."
    
    /// Converts a three element tuple into an array
    let tupletoArray (tuple:float32*float32*float32) =
        let x = float32(first tuple)
        let y = float32(second tuple)
        let z = float32(third tuple)
        let array = [|x; y; z|]
        array

    /// Converts type byte option into type byte.
    /// Used on exponent field in record type Encoder. 
    let internal byteOptionConvert = function 
        | Some byte -> byte
        | None -> 0uy 

    /// Converts type channel into bytes. 
    let channelByte = function
        | Channel0 -> 0uy 
        | Channel1 -> 1uy 
        | Channel2 -> 2uy
     
    /// Converts channel bytes into type channel.  
    let parseChannel = function
        | 0uy -> Channel0
        | 1uy -> Channel1
        | 2uy -> Channel2
        | xuy         -> failwithf "Not a valid channel: %A" xuy

    /// Converts type Loop to type boolean.
    let loopBoolean = function
        | OpenLoop  -> false
        | ClosedLoop -> true 

    /// Converts typr boolean to type Loop.
    let parseLoop = function
        | false -> OpenLoop
        | true  -> ClosedLoop

    /// Converts type switch to type boolean.
    let switchBoolean = function
        | On  -> true
        | Off -> false
    
    /// Converts type boolean to type switch. 
    let parseSwitch = function 
        | true  -> On
        | false -> Off

    /// Converts type ActuatorPosition into type expected by the NativeApi functions (ActuatorCoordinate).
    let actuatorPositionMap = function
        | PositionX -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X
        | PositionY -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y
        | PositionZ -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z
        | PositionNone -> Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.None

    /// Converts type expected by the NativeApi functions (ActuatorCoordinate) into type AcuatorPosition.
    /// Includes fields Phi and Theta, can be returned from NativeApi functions. 
    let parseActuatorPosition= function
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.X     -> PositionX
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Y     -> PositionY
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Z     -> PositionZ
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Phi   -> failwithf "Phi positioning not avalible."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.Theta -> failwithf "Theta positioning not avalible."
        | Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate.None  -> PositionNone
        | _ -> failwithf "Not a valid coordinate axis."

    /// Converts type mode into type expected by NativeApi functions (EncoderMode).
    let modeMap = function
        | Normal               ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal               
        | IntervalWithAcceleration ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration
        | Interval             ->  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval     

    /// Converts type expected by NativeApi functions (EncoderMode) to type Mode. 
    let parseMode = function 
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Normal                   -> Normal     
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.IntervalWithAcceleration -> IntervalWithAcceleration         
        |  Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode.Interval                 -> Interval            
        |  str -> failwithf "Not a valid mode: %A" str

    /// Maps type StopBits onto dll type StopBitsKind.
    let stopBitsMap = function
        | One          -> Piezojena.Protocols.SerialStopBitsKind.One          
        | OnePointFive -> Piezojena.Protocols.SerialStopBitsKind.OnePointFive
        | Two          -> Piezojena.Protocols.SerialStopBitsKind.Two
    
    /// Maps type StopBitsKind onto type Stop Bits. 
    let parseStopBits = function
        | Piezojena.Protocols.SerialStopBitsKind.One          -> One
        | Piezojena.Protocols.SerialStopBitsKind.OnePointFive -> OnePointFive
        | Piezojena.Protocols.SerialStopBitsKind.Two          -> Two
        | _ -> failwithf "Not a valib stop bit."

    /// Maps type Parity onto dll type SerialParity.
    let parityMap = function
        | ParityNone  -> Piezojena.Protocols.SerialParity.None
        | ParityOdd   -> Piezojena.Protocols.SerialParity.Odd
        | ParityEven  -> Piezojena.Protocols.SerialParity.Even
        | ParityMark  -> Piezojena.Protocols.SerialParity.Mark
        | ParitySpace -> Piezojena.Protocols.SerialParity.Space

    /// Maps type SerialParity onto type Parity.
    let parseParity = function
        | Piezojena.Protocols.SerialParity.None  -> ParityNone
        | Piezojena.Protocols.SerialParity.Odd   -> ParityOdd
        | Piezojena.Protocols.SerialParity.Even  -> ParityEven
        | Piezojena.Protocols.SerialParity.Mark  -> ParityMark
        | Piezojena.Protocols.SerialParity.Space -> ParitySpace
        | _ -> failwithf "Not a valid parity."

    /// Maps type SerialFlowControl onto dll type SerialFlowControls. 
    let flowControlMap = function 
        | FlowControlNone   -> Piezojena.Protocols.SerialFlowControls.None
        | FlowControlDtrDsr  -> Piezojena.Protocols.SerialFlowControls.DtrDsr
        | FlowControlRtsCts  -> Piezojena.Protocols.SerialFlowControls.RtsCts
        | FlowControlXOnXOff -> Piezojena.Protocols.SerialFlowControls.XOnXOff

    /// Maps type SerialFlowControls onto type SerialFlowControl. 
    let parseFlowControl = function 
        | Piezojena.Protocols.SerialFlowControls.None     -> FlowControlNone
        | Piezojena.Protocols.SerialFlowControls.DtrDsr  -> FlowControlDtrDsr
        | Piezojena.Protocols.SerialFlowControls.RtsCts  -> FlowControlRtsCts
        | Piezojena.Protocols.SerialFlowControls.XOnXOff -> FlowControlXOnXOff
        | _ -> failwithf "Not a valid flow control."