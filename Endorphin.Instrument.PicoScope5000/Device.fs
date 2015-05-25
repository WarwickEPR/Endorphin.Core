namespace Endorphin.Instrument.PicoScope5000

open System.Runtime.CompilerServices
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

[<AutoOpen>]
module Device =
    type PicoScope5000 = PicoScope5000 of handle : int16
    
    type ToggleState = Enabled | Disabled

    let internal parseToggleState value = if value <> 0s then Enabled else Disabled
    
    type PowerSource = MainsPower | UsbPower

    let internal tryParsePowerSource =
        function
        | StatusCode.PowerSupplyConnected    -> Some MainsPower
        | StatusCode.PowerSupplyNotConnected -> Some UsbPower
        | _                                  -> None

    let internal (|PowerSource|) =
        function
        | MainsPower -> StatusCode.PowerSupplyConnected
        | UsbPower   -> StatusCode.PowerSupplyNotConnected

    let internal (|PowerSourceStatus|_|) =
        function
        | StatusCode.PowerSupplyConnected    -> Some MainsPower
        | StatusCode.PowerSupplyNotConnected -> Some UsbPower
        | _                                  -> None

    /// Enumeration representing the available device information which can be requested from the PicoScope driver.
    type internal DeviceInfoEnum = 
        | DriverVersion           = 0
        | UsbVersion              = 1
        | HardwareVersion         = 2
        | ModelNumber             = 3
        | SerialNumber            = 4
        | CalibrationDate         = 5
        | KernelVersion           = 6
        | DigitalHardwareVersion  = 7
        | AnalogueHardwareVersion = 8
        | FirmwareVersion1        = 9
        | FirmwareVersion2        = 10
    
    type DeviceInfo =
        | DriverVersion           
        | UsbVersion              
        | HardwareVersion         
        | ModelNumber             
        | SerialNumber            
        | CalibrationDate         
        | KernelVersion           
        | DigitalHardwareVersion  
        | AnalogueHardwareVersion 
        | FirmwareVersion1        
        | FirmwareVersion2        

    let internal parseDeviceInfo =
        function
        | DeviceInfoEnum.DriverVersion           -> DriverVersion           
        | DeviceInfoEnum.UsbVersion              -> UsbVersion
        | DeviceInfoEnum.HardwareVersion         -> HardwareVersion         
        | DeviceInfoEnum.ModelNumber             -> ModelNumber             
        | DeviceInfoEnum.SerialNumber            -> SerialNumber            
        | DeviceInfoEnum.CalibrationDate         -> CalibrationDate         
        | DeviceInfoEnum.KernelVersion           -> KernelVersion           
        | DeviceInfoEnum.DigitalHardwareVersion  -> DigitalHardwareVersion  
        | DeviceInfoEnum.AnalogueHardwareVersion -> AnalogueHardwareVersion 
        | DeviceInfoEnum.FirmwareVersion1        -> FirmwareVersion1        
        | DeviceInfoEnum.FirmwareVersion2        -> FirmwareVersion2        
        | enum                                   -> failwithf "Unexpected device info enum value: %A." enum

    let internal (|DeviceInfo|) =
        function
        | DriverVersion           -> DeviceInfoEnum.DriverVersion          
        | UsbVersion              -> DeviceInfoEnum.UsbVersion             
        | HardwareVersion         -> DeviceInfoEnum.HardwareVersion        
        | ModelNumber             -> DeviceInfoEnum.ModelNumber            
        | SerialNumber            -> DeviceInfoEnum.SerialNumber           
        | CalibrationDate         -> DeviceInfoEnum.CalibrationDate        
        | KernelVersion           -> DeviceInfoEnum.KernelVersion          
        | DigitalHardwareVersion  -> DeviceInfoEnum.DigitalHardwareVersion 
        | AnalogueHardwareVersion -> DeviceInfoEnum.AnalogueHardwareVersion
        | FirmwareVersion1        -> DeviceInfoEnum.FirmwareVersion1       
        | FirmwareVersion2        -> DeviceInfoEnum.FirmwareVersion2   
            
    /// Represents the possible states which can be set to the front panel LED of the PicoScope 5000 device.
    type LedFlash =
        | LedOff
        | LedRepeat of counts : int16
        | LedIndefiniteRepeat

    let internal (|LedFlash|) =
        function
        | LedOff              -> LedFlash 0s
        | LedRepeat n         -> LedFlash n
        | LedIndefiniteRepeat -> LedFlash -1s

    /// Enumeration representing the vertical resolution of a PicoScope 5000 series device.
    type internal ResolutionEnum =
        | _8bit  = 0
        | _12bit = 1
        | _14bit = 2
        | _15bit = 3
        | _16bit = 4

    type Resolution =
        | Resolution_8bit
        | Resolution_12bit
        | Resolution_14bit
        | Resolution_15bit
        | Resolution_16bit

    let internal parseResolution =
        function
        | ResolutionEnum._8bit  -> Resolution_8bit
        | ResolutionEnum._12bit -> Resolution_12bit
        | ResolutionEnum._14bit -> Resolution_14bit
        | ResolutionEnum._15bit -> Resolution_15bit
        | ResolutionEnum._16bit -> Resolution_16bit
        | enum                  -> failwithf "Unexpected resolution enum value: %A." enum

    let internal (|Resolution|) = 
        function
        | Resolution_8bit  -> ResolutionEnum._8bit
        | Resolution_12bit -> ResolutionEnum._12bit
        | Resolution_14bit -> ResolutionEnum._14bit
        | Resolution_15bit -> ResolutionEnum._15bit
        | Resolution_16bit -> ResolutionEnum._16bit

    let fastestTimebaseForResolution =
        function
        | Resolution_8bit  -> 0u
        | Resolution_12bit -> 1u
        | Resolution_14bit
        | Resolution_15bit -> 3u
        | Resolution_16bit -> 4u

    /// Returns the maximum number of input channels supported by the PicoScope 5000 hardware for this vertical
    /// resolution.
    let maximumNumberOfCChannelsForResolution =
        function
        | Resolution_8bit
        | Resolution_12bit 
        | Resolution_14bit -> 4
        | Resolution_15bit -> 2
        | Resolution_16bit -> 1

    /// Returns the fastest streaming sample interval supported by the PicoScope 5000 hardware for this vertical
    /// resolution.
    let fastestStreamingInterval resolution channelCount =
        match (resolution, channelCount) with
        | (Resolution_12bit, 4)
        | (Resolution_12bit, 3)
        | (Resolution_14bit, 4)
        | (Resolution_14bit, 3) -> 256.0<ns>
        | (Resolution_8bit,  4) 
        | (Resolution_8bit,  3) 
        | (Resolution_12bit, 2) 
        | (Resolution_14bit, 2) 
        | (Resolution_15bit, 2) -> 128.0<ns>
        | (Resolution_8bit,  3)
        | (Resolution_12bit, 1)
        | (Resolution_14bit, 1)
        | (Resolution_15bit, 1)
        | (Resolution_16bit, 2) -> 64.0<ns>
        | (Resolution_8bit,  1) -> 32.0<ns>
        | (resolution, channelCount) when channelCount > maximumNumberOfCChannelsForResolution resolution ->
            failwithf "Exceeded maximum number of channels for resolution: %A." (resolution, channelCount)
        | (_, channelCount) ->
            failwithf "Number of channels must be a non-zero, positive integer: %d." channelCount

    /// Returns the number of bits for this resolution as an integer value.
    let resolutionValueInBits =
        function
        | Resolution_8bit  -> 8<bits>
        | Resolution_12bit -> 12<bits>
        | Resolution_14bit -> 14<bits>
        | Resolution_15bit -> 15<bits>
        | Resolution_16bit -> 16<bits>

    /// Returns a bit mask which only selects the significant number of top bits for the given resolution.
    let bitMaskForResolution resolution =
        // First left shift 0000 0000 0000 0001 by (16 minus the device resolution, e.g. 12).
        // Then we have 0000 0000 0001 0000 and we subtract 1 from that. This gives 
        // 0000 0000 0000 1111. Then negating all bits means that the highest order bits
        // correspond to the device resolution are 1s and the lowest order bits are 0s.
        ~~~ ((1s <<< (int (16<bits> - resolutionValueInBits resolution))) - 1s)