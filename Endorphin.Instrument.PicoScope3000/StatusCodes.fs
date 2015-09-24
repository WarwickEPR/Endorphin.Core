namespace Endorphin.Instrument.PicoScope3000

module internal StatusCodes =
    /// PicoScope 3000 series status codes indicating device status after all API calls.
    /// Must always be of type int.
    type StatusCode = 
        | Ok                                   = 0x0000
        | MaximumUnitsOpened                   = 0x0001
        | MemoryFailure                        = 0x0002
        | NotFound                             = 0x0003
        | FirmwareFailure                      = 0x0004
        | OpenOperationInProgress              = 0x0005
        | OperationFailed                      = 0x0006
        | NotResponding                        = 0x0007
        | ConfigFailure                        = 0x0008
        | KernelDriverTooOld                   = 0x0009
        | EepromCorrupt                        = 0x000A
        | OperatingSystemNotSupported          = 0x000B
        | InvalidHandle                        = 0x000C
        | InvalidParameter                     = 0x000D
        | InvalidTimebase                      = 0x000E
        | InvalidVoltageRange                  = 0x000F
        | InvalidChannel                       = 0x0010
        | InvalidTriggerChannel                = 0x0011
        | InvalidConditionChannel              = 0x0012
        | NoSignalGenerator                    = 0x0013
        | StreamingFailed                      = 0x0014
        | BlockModeFailed                      = 0x0015
        | NullParameter                        = 0x0016
//      | EtsModeSet                           = 0x0017  -- Not supported on PicoScope 3000
        | DataNotAvailable                     = 0x0018
        | StringBufferTooSmall                 = 0x0019
        | EtsNotSupported                      = 0x001A
        | AutoTriggerTimeTooShort              = 0x001B
        | BufferStall                          = 0x001C
        | TooManySamples                       = 0x001D
        | TooManySegments                      = 0x001E
        | PulseWidthQualifier                  = 0x001F
        | Delay                                = 0x0020
        | SourceDetails                        = 0x0021
        | Conditions                           = 0x0022
        | UserCallback                         = 0x0023
        | DeviceSampling                       = 0x0024
        | NoSamplesAvailable                   = 0x0025
        | SegmentOutOfRange                    = 0x0026
        | Busy                                 = 0x0027
        | StartIndexInvalid                    = 0x0028
        | InvalidInfo                          = 0x0029
        | InfoUnavailable                      = 0x002A
        | InvalidSampleInterval                = 0x002B
        | TriggerError                         = 0x002C
        | Memory                               = 0x002D
        | SigGenParameter                      = 0x002E
        | ShotsSweepsWarning                   = 0x002F
//      | SigGenTriggerSource                  = 0x0030 -- Not supported on PicoScope 3000
//      | AuxOutputConflict                    = 0x0031 -- Not supported on PicoScope 3000
//      | AuxOutputEtsConflict                 = 0x0032 -- Not supported on PicoScope 3000
        | WarningExtThresholdConflict          = 0x0033
//      | WarningAuxOutputConflict             = 0x0034 -- Not supported on PicoScope 3000
        | SigGenOutputOverVoltage              = 0x0035
        | DelayNull                            = 0x0036
        | InvalidBuffer                        = 0x0037
        | SigGenOffsetVoltage                  = 0x0038
        | SigGenPeakToPeak                     = 0x0039
        | Cancelled                            = 0x003A
        | SegmentNotUsed                       = 0x003B
        | InvalidCall                          = 0x003C
//      | GetValuesInterrupted                 = 0x003D -- Not supported on PicoScope 3000
        | NotUsed                              = 0x003F
        | InvalidSampleRatio                   = 0x0040
        | InvalidState                         = 0x0041
        | NotEnoughSegments                    = 0x0042
        | DriverFunction                       = 0x0043
        | Reserved                             = 0x0044
        | InvalidCoupling                      = 0x0045
        | BuffersNotSet                        = 0x0046
        | RatioModeNotSupported                = 0x0047
//      | RapidModeDoesNotSupportAggregation   = 0x0048 -- Not supported on PicoScope 3000
        | InvalidTriggerProperty               = 0x0049
        | InterfaceNotConnected                = 0x004A
//      | ResistanceAndProbeNotAllowed         = 0x004B -- Not supported on PicoScope 3000
//      | PowerFailed                          = 0x004C -- Not supported on PicoScope 3000
        | SigGenWaveformSetupFailed            = 0x004D
        | FpgaFailure                          = 0x004E
        | PowerManager                         = 0x004F
        | InvalidAnalogueOffset                = 0x0050
        | PllLockFailed                        = 0x0051
        | AnalogBoard                          = 0x0052
        | ConfigFailureAwg                     = 0x0053
        | InitialiseFpga                       = 0x0054
        | ExternalFrequencyInvalid             = 0x0056
        | ClockChangeError                     = 0x0057
        | TriggerAndExternalClockClash         = 0x0058
        | PwqAndExternalClockClash             = 0x0059
        | UnableToOpenScalingFile              = 0x005A
        | MemoryClockFrequency                 = 0x005B
        | I2cNotResponding                     = 0x005C
        | NoCapturesAvailable                  = 0x005D
        | NotUsedInThisCaptureMode             = 0x005E
        | GetDataActive                        = 0x0103
        | IpNetworked                          = 0x0104
        | InvalidIpAddress                     = 0x0105
        | IpSocketFailed                       = 0x0106
        | IpSocketTimeout                      = 0x0107
        | SettingsFailed                       = 0x0108
        | NetworkFailed                        = 0x0109
        | Ws2DllNotLoaded                      = 0x010A
        | InvalidIpPort                        = 0x010B
        | CouplingNotSupported                 = 0x010C
        | BandwidthNotSupported                = 0x010D
        | InvalidBandwidth                     = 0x010E
        | AwgNotSupported                      = 0x010F
        | EtsNotRunning                        = 0x0110
        | SigGenWhiteNoiseNotSupported         = 0x0111
        | SigGenWaveTypeNotSupported           = 0x0112
        | InvalidDigitalPort                   = 0x0113
        | InvalidDigitalChannel                = 0x0114
        | InvalidDigitalTriggerDirection       = 0x0115
        | SigGenPrbsNotSupported               = 0x0116
        | EtsNotAvailableWithLogicChannels     = 0x0117
        | WarningRepeatValue                   = 0x0118
        | PowerSupplyConnected                 = 0x0119
        | PowerSupplyNotConnected              = 0x011A
        | PowerSupplyRequestInvalid            = 0x011B
        | PowerSupplyUnderVoltage              = 0x011C
        | CapturingData                        = 0x011D
        | Usb3DeviceNotUsb3Port                = 0x011E
//      | NotSupportedByThisDevice             = 0x011F -- Not supported on PicoScope 3000
//      | InvalidDeviceResolution              = 0x0120 -- Not supported on PicoScope 3000
//      | InvalidNumberOfChannelsForResolution = 0x0121 -- Not supported on PicoScope 3000
//      | ChannelDisabledDueToUsbPower         = 0x0122 -- Not supported on PicoScope 3000

    /// Converts a status code to a string message describing it.
    let statusMessage = function
        | StatusCode.Ok -> "OK"
        | StatusCode.MaximumUnitsOpened -> "An attempt has been made to open more than `PS3000A_MAX_UNITS`"
        | StatusCode.MemoryFailure -> "Not enough memory could be allocated on the host machine"
        | StatusCode.NotFound -> "No PicoScope could be found"
        | StatusCode.FirmwareFailure -> "Unable to download firmware"
        | StatusCode.OpenOperationInProgress -> "Open operation in progress"
        | StatusCode.OperationFailed -> "Operation failed"
        | StatusCode.NotResponding -> "The PicoScope is not responding to commands from the PC"
        | StatusCode.ConfigFailure -> "The configuration information in the PicoScope has become corrupt or is missing"
        | StatusCode.KernelDriverTooOld -> "The picopp.sys file is too old to be used with the device driver"
        | StatusCode.EepromCorrupt -> "The EEPROM has become corrupt, so the device will use a default setting"
        | StatusCode.OperatingSystemNotSupported -> "The operating system on the PC is not supported by this driver"
        | StatusCode.InvalidHandle -> "There is no device with the handle value passed"
        | StatusCode.InvalidParameter -> "A parameter value is not valid"
        | StatusCode.InvalidTimebase -> "The timebase is not supported or is invalid"
        | StatusCode.InvalidVoltageRange -> "The voltage range is not supported or is invalid"
        | StatusCode.InvalidChannel -> "The channel number is not valid on this device or no channels have been set"
        | StatusCode.InvalidTriggerChannel -> "The channel set for a trigger is not available on this device"
        | StatusCode.InvalidConditionChannel -> "The channel set for a condition is not available on this device"
        | StatusCode.NoSignalGenerator -> "The device does not have a signal generator"
        | StatusCode.StreamingFailed -> "Streaming has failed to start or has stopped without user request"
        | StatusCode.BlockModeFailed -> "Block failed to start ->a parameter may have been set wrongly"
        | StatusCode.NullParameter -> "A parameter that was required is NULL"
        | StatusCode.DataNotAvailable -> "No data is available from a run block call"
        | StatusCode.StringBufferTooSmall -> "The buffer passed for the information was too small"
        | StatusCode.EtsNotSupported -> "ETS is not supported on this device"
        | StatusCode.AutoTriggerTimeTooShort -> "The auto trigger time is less than the time it will take to collect the pre-trigger data"
        | StatusCode.BufferStall -> "The collection of data has stalled as unread data would be overwritten"
        | StatusCode.TooManySamples -> "Number of samples requested is more than available in the current memory segment"
        | StatusCode.TooManySegments -> "Not possible to create number of segments requested"
        | StatusCode.PulseWidthQualifier -> "A null pointer has been passed in the trigger function or one of the parameters is out of range"
        | StatusCode.Delay -> "One or more of the hold-off parameters are out of range"
        | StatusCode.SourceDetails -> "One or more of the source details are incorrect"
        | StatusCode.Conditions -> "One or more of the conditions are incorrect"
        | StatusCode.UserCallback -> "The driver's thread is currently in the `ps3000a...Ready` callback function and therefore the action cannot be carried out"
        | StatusCode.DeviceSampling -> "An attempt is being made to get stored data while streaming. Either stop streaming by calling `ps3000aStop`, or use `ps3000aGetStreamingLatestValues`"
        | StatusCode.NoSamplesAvailable -> "No samples available because a run has not been completed"
        | StatusCode.SegmentOutOfRange -> "The memory index is out of range"
        | StatusCode.Busy -> "Data cannot be returned yet"
        | StatusCode.StartIndexInvalid -> "The start time to get stored data is out of range"
        | StatusCode.InvalidInfo -> "The information number requested is not a valid number"
        | StatusCode.InfoUnavailable -> "The handle is invalid so no information is available about the device. Only PICO_DRIVER_VERSION is available"
        | StatusCode.InvalidSampleInterval -> "The sample interval selected for streaming is out of range"
        | StatusCode.TriggerError -> "Trigger error"
        | StatusCode.Memory -> "Driver cannot allocate memory"
        | StatusCode.SigGenParameter -> "Incorrect parameter passed to the signal generator"
        | StatusCode.ShotsSweepsWarning -> "Conflict between the `shots` and `sweeps` parameters sent to the signal generator"
        | StatusCode.WarningExtThresholdConflict -> "Attempt to set different EXT input thresholds set for signal generator and oscilloscope trace"
        | StatusCode.SigGenOutputOverVoltage -> "The combined peak to peak voltage and the analog offset voltage exceed the allowable voltage the signal generator can produce"
        | StatusCode.DelayNull -> "NULL pointer passed as delay parameter"
        | StatusCode.InvalidBuffer -> "The buffers for overview data have not been set while streaming"
        | StatusCode.SigGenOffsetVoltage -> "The analog offset voltage is out of range"
        | StatusCode.SigGenPeakToPeak -> "The analog peak to peak voltage is out of range"
        | StatusCode.Cancelled -> "A block collection has been cancelled"
        | StatusCode.SegmentNotUsed -> "The segment index is not currently being used"
        | StatusCode.InvalidCall -> "The wrong `GetValues` function has been called for the collection mode in use"
        | StatusCode.NotUsed -> "The function is not available"
        | StatusCode.InvalidSampleRatio -> "The aggregation ratio requested is out of range"
        | StatusCode.InvalidState -> "Device is in an invalid state"
        | StatusCode.NotEnoughSegments -> "The number of segments allocated is fewer than the number of captures requested"
        | StatusCode.DriverFunction -> "You called a driver function while another driver function was still being processed"
        | StatusCode.InvalidCoupling -> "An invalid coupling type was specified in `ps3000aSetChannel`"
        | StatusCode.BuffersNotSet -> "An attempt was made to get data before a data buffer was defined"
        | StatusCode.RatioModeNotSupported -> "The selected downsampling mode (used for data reduction) is not allowed"
        | StatusCode.InvalidTriggerProperty -> "An invalid parameter was passed to `ps3000aSetTriggerChannelProperties`"
        | StatusCode.InterfaceNotConnected -> "The driver was unable to contact the oscilloscope"
        | StatusCode.SigGenWaveformSetupFailed -> "A problem occurred in `ps3000aSetSigGenBuiltIn` or `ps3000aSetSigGenArbitrary`"
        | StatusCode.FpgaFailure -> "FPGA not successfully set up"
        | StatusCode.PowerManager -> "Power manager error"
        | StatusCode.InvalidAnalogueOffset -> "impossible analogue offset value was specified in `ps3000aSetChannel`"
        | StatusCode.PllLockFailed -> "Unable to configure the PicoScope"
        | StatusCode.AnalogBoard -> "The oscilloscope's analog board is not detected, or is not connected to the digital board"
        | StatusCode.ConfigFailureAwg -> "Unable to configure the signal generator"
        | StatusCode.InitialiseFpga -> "The FPGA cannot be initialized, so unit cannot be opened"
        | StatusCode.ExternalFrequencyInvalid -> "The frequency for the external clock is not within +/-5% of the stated value"
        | StatusCode.ClockChangeError -> "The FPGA could not lock the clock signal"
        | StatusCode.TriggerAndExternalClockClash -> "You are trying to configure the AUX input as both a trigger and a reference clock"
        | StatusCode.PwqAndExternalClockClash -> "You are trying to congfigure the AUX input as both a pulse width qualifier and a reference clock"
        | StatusCode.UnableToOpenScalingFile -> "The scaling file set can not be opened"
        | StatusCode.MemoryClockFrequency -> "The frequency of the memory is reporting incorrectly"
        | StatusCode.I2cNotResponding -> "The I2C that is being actioned is not responding to requests"
        | StatusCode.NoCapturesAvailable -> "There are no captures available and therefore no data can be returned"
        | StatusCode.NotUsedInThisCaptureMode -> "The capture mode the device is currently running in does not support the current request"
        | StatusCode.IpNetworked -> "The device is currently connected via the IP Network socket and thus the call made is not supported"
        | StatusCode.InvalidIpAddress -> "An IP address that is not correct has been passed to the driver"
        | StatusCode.IpSocketFailed -> "The IP socket has failed"
        | StatusCode.IpSocketTimeout -> "The IP socket has timed out"
        | StatusCode.SettingsFailed -> "The settings requested have failed to be set"
        | StatusCode.NetworkFailed -> "The network connection has failed"
        | StatusCode.Ws2DllNotLoaded -> "Unable to load the WS2 dll"
        | StatusCode.InvalidIpPort -> "The IP port is invalid"
        | StatusCode.CouplingNotSupported -> "The type of coupling requested is not supported on the opened device"
        | StatusCode.BandwidthNotSupported -> "Bandwidth limit is not supported on the opened device"
        | StatusCode.InvalidBandwidth -> "The value requested for the bandwidth limit is out of range"
        | StatusCode.AwgNotSupported -> "The arbitrary waveform generator is not supported by the opened device"
        | StatusCode.EtsNotRunning -> "Data has been requested with ETS mode set but run block has not been called, or stop has been called"
        | StatusCode.SigGenWhiteNoiseNotSupported -> "White noise is not supported on the opened device"
        | StatusCode.SigGenWaveTypeNotSupported -> "The wave type requested is not supported by the opened device"
        | StatusCode.InvalidDigitalPort -> "A port number that does not evaluate to either `PS3000A_DIGITAL_PORT0` or `PS3000A_DIGITAL_PORT1`, the ports that are supported"
        | StatusCode.InvalidDigitalChannel -> "The digital channel is not in the range `PS3000A_DIGITAL_CHANNEL0` to `PS3000A_DIGITAL_CHANNEL15`, the digital channels that are supported"
        | StatusCode.InvalidDigitalTriggerDirection -> "The digital trigger direction is not a valid trigger direction and should be equal in value to one of the `PS3000A_DIGITAL_DIRECTION` enumerations"
        | StatusCode.SigGenPrbsNotSupported -> "Siggen does not generate pseudo-random bit stream"
        | StatusCode.EtsNotAvailableWithLogicChannels -> "When a digital port is enabled, ETS sample mode is not available for use"
        | StatusCode.PowerSupplyConnected -> "The DC power supply is connected"
        | StatusCode.PowerSupplyNotConnected -> "The DC power supply isn’t connected"
        | StatusCode.PowerSupplyRequestInvalid -> "Incorrect power mode passed for current power source"
        | StatusCode.PowerSupplyUnderVoltage -> "The supply voltage from the USB source is too low"
        | StatusCode.CapturingData -> "The oscilloscope is in the process of capturing data"
        | StatusCode.Usb3DeviceNotUsb3Port -> "A Pico USB 3.0 device has been connected to a non-USB 3.0 port"
//      | StatusCode.NotSupportedByThisDevice -> "A function has been called that is not supported by the current device variant"
//      | StatusCode.InvalidDeviceResolution -> "The device resolution is invalid (out of range)"
//      | StatusCode.InvalidNumberOfChannelsForResolution -> "The number of channels which can be enabled is limited in 15 and 16-bit modes"
//      | StatusCode.ChannelDisabledDueToUsbPower -> "USB Power not sufficient to power all channels"
        | status -> sprintf "Failed with unexpected StatusCode value: %A" status