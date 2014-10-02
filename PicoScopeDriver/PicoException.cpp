#pragma once

#include "Stdafx.h"

using namespace PicoScopeDriver;

String^ PicoException::BuildMessageForStatus(PicoStatus status) {
	// status messages as defined in the PS5000A programmer manual.
	switch (status) {
	case PicoStatus::MaximumUnitsOpened:
		return gcnew String("An attempt has been made to open more than PS5000A_MAX_UNITS.");
	case PicoStatus::MemoryFailure:
		return gcnew String("Not enough memory could be allocated on the host machine.");
	case PicoStatus::NotFound:
		return gcnew String("No PicoScope could be found.");
	case PicoStatus::FirmwareFailure:
		return gcnew String("Unable to download firmware.");
	case PicoStatus::OpenOperationInProgress:
		return gcnew String("Open operation in progress.");
	case PicoStatus::OperationFailed:
		return gcnew String("Operation failed.");
	case PicoStatus::NotResponding:
		return gcnew String("The PicoScope is not responding to commands from the PC.");
	case PicoStatus::ConfigFailure:
		return gcnew String("The configuration information in the PicoScope has become corrupt or is missing.");
	case PicoStatus::KernelDriverTooOld:
		return gcnew String("The picopp.sys file is too old to be used with the device driver.");
	case PicoStatus::EepromCorrupt:
		return gcnew String("The EEPROM has become corrupt, so the device will use a default setting.");
	case PicoStatus::OperatingSystemNotSupported:
		return gcnew String("The operating system on the PC is not supported by this driver.");
	case PicoStatus::InvalidHandle:
		return gcnew String("There is no device with the handle value passed.");
	case PicoStatus::InvalidParameter:
		return gcnew String("A parameter value is not valid.");
	case PicoStatus::InvalidTimebase:
		return gcnew String("The timebase is not supported or is invalid.");
	case PicoStatus::InvalidVoltageRange:
		return gcnew String("The voltage range is not supported or is invalid.");
	case PicoStatus::InvalidChannel:
		return gcnew String("The channel number is not valid on this device or no channels have been set.");
	case PicoStatus::InvalidTriggerChannel:
		return gcnew String("The channel set for a trigger is not available on this device.");
	case PicoStatus::InvalidConditionChannel:
		return gcnew String("The channel set for a condition is not available on this device.");
	case PicoStatus::NoSignalGenerator:
		return gcnew String("The device does not have a signal generator.");
	case PicoStatus::StreamingFailed:
		return gcnew String("Streaming has failed to start or has stopped without user request.");
	case PicoStatus::BlockModeFailed:
		return gcnew String("Block failed to start: a parameter may have been set wrongly.");
	case PicoStatus::NullParameter:
		return gcnew String("A parameter that was required is NULL.");
	case PicoStatus::DataNotAvailable:
		return gcnew String("No data is available from a run block call.");
	case PicoStatus::StringBufferTooSmall:
		return gcnew String("The buffer passed for the information was too small.");
	case PicoStatus::EtsNotSupported:
		return gcnew String("ETS is not supported on this device.");
	case PicoStatus::AutoTriggerTimeTooShort:
		return gcnew String("The auto trigger time is less than the time it will take to collect the pre-trigger data.");
	case PicoStatus::BufferStall:
		return gcnew String("The collection of data has stalled as unread data would be overwritten.");
	case PicoStatus::TooManySamples:
		return gcnew String("Number of samples requested is more than available in the current memory segment.");
	case PicoStatus::TooManySegments:
		return gcnew String("Not possible to create number of segments requested.");
	case PicoStatus::PulseWidthQualifier:
		return gcnew String("A null pointer has been passed in the trigger function or one of the parameters is out of range.");
	case PicoStatus::Delay:
		return gcnew String("One or more of the hold-off parameters are out of range.");
	case PicoStatus::SourceDetails:
		return gcnew String("One or more of the source details are incorrect.");
	case PicoStatus::Conditions:
		return gcnew String("One or more of the conditions are incorrect.");
	case PicoStatus::UserCallback:
		return gcnew String("The driver's thread is currently in the ps5000a...Ready callback function and therefore the action cannot be carried out.");
	case PicoStatus::DeviceSampling:
		return gcnew String("An attempt is being made to get stored data while streaming. Either stop streaming by calling ps5000aStop, or use ps5000aGetStreamingLatestValues.");
	case PicoStatus::NoSamplesAvailable:
		return gcnew String("No samples available because a run has not been completed.");
	case PicoStatus::SegmentOutOfRange:
		return gcnew String("The memory index is out of range.");
	case PicoStatus::Busy:
		return gcnew String("Data cannot be returned yet.");
	case PicoStatus::StartIndexInvalid:
		return gcnew String("The start time to get stored data is out of range.");
	case PicoStatus::InvalidInfo:
		return gcnew String("The information number requested is not a valid number.");
	case PicoStatus::InfoUnavailable:
		return gcnew String("The handle is invalid so no information is available about the device. Only PICO_DRIVER_VERSION is available.");
	case PicoStatus::InvalidSampleInterval:
		return gcnew String("The sample interval selected for streaming is out of range.");
	case PicoStatus::TriggerError:
		return gcnew String("Trigger error.");
	case PicoStatus::Memory:
		return gcnew String("Driver cannot allocate memory.");
	case PicoStatus::SigGenOutputOverVoltage:
		return gcnew String("The combined peak to peak voltage and the analog offset voltage exceed the allowable voltage the signal generator can produce.");
	case PicoStatus::DelayNull:
		return gcnew String("NULL pointer passed as delay parameter.");
	case PicoStatus::InvalidBuffer:
		return gcnew String("The buffers for overview data have not been set while streaming.");
	case PicoStatus::SigGenOffsetVoltage:
		return gcnew String("The analog offset voltage is out of range.");
	case PicoStatus::SigGenPeakToPeak:
		return gcnew String("The analog peak to peak voltage is out of range.");
	case PicoStatus::Cancelled:
		return gcnew String("A block collection has been cancelled.");
	case PicoStatus::SegmentNotUsed:
		return gcnew String("The segment index is not currently being used.");
	case PicoStatus::InvalidCall:
		return gcnew String("The wrong GetValues function has been called for the collection mode in use.");
	case PicoStatus::NotUsed:
		return gcnew String("The function is not available.");
	case PicoStatus::InvalidSampleRatio:
		return gcnew String("The aggregation ratio requested is out of range.");
	case PicoStatus::InvalidState:
		return gcnew String("Device is in an invalid state.");
	case PicoStatus::NotEnoughSegments:
		return gcnew String("The number of segments allocated is fewer than the number of captures requested.");
	case PicoStatus::DriverFunction:
		return gcnew String("You called a driver function while another driver function was still being processed.");
	case PicoStatus::InvalidCoupling:
		return gcnew String("An invalid coupling type was specified in ps5000aSetChannel.");
	case PicoStatus::BuffersNotSet:
		return gcnew String("An attempt was made to get data before a data buffer was defined.");
	case PicoStatus::RatioModeNotSupported:
		return gcnew String("The selected downsampling mode (used for data reduction) is not allowed.");
	case PicoStatus::InvalidTriggerProperty:
		return gcnew String("An invalid parameter was passed to ps5000aSetTriggerChannelProperties.");
	case PicoStatus::InterfaceNotConnected:
		return gcnew String("The driver was unable to contact the oscilloscope.");
	case PicoStatus::SigGenWaveformSetupFailed:
		return gcnew String("A problem occurred in ps5000aSetSigGenBuiltIn or ps5000aSetSigGenArbitrary.");
	case PicoStatus::FpgaFailure:
		return gcnew String("FPGA not successfully set up.");
	case PicoStatus::PowerManager:
		return gcnew String("Power manager error.");
	case PicoStatus::InvalidAnalogueOffset:
		return gcnew String("impossible analogue offset value was specified in ps5000aSetChannel.");
	case PicoStatus::PllLockFailed:
		return gcnew String("Unable to configure the PicoScope.");
	case PicoStatus::AnalogBoard:
		return gcnew String("The oscilloscope's analog board is not detected, or is not connected to the digital board.");
	case PicoStatus::ConfigFailureAwg:
		return gcnew String("Unable to configure the signal generator.");
	case PicoStatus::InitialiseFpga:
		return gcnew String("The FPGA cannot be initialized, so unit cannot be opened.");
	case PicoStatus::ExternalFrequencyInvalid:
		return gcnew String("The frequency for the external clock is not within +/-5% of the stated value.");
	case PicoStatus::ClockChangeError:
		return gcnew String("The FPGA could not lock the clock signal.");
	case PicoStatus::TriggerAndExternalClockClash:
		return gcnew String("You are trying to configure the AUX input as both a trigger and a reference clock.");
	case PicoStatus::PwqAndExternalClockClash:
		return gcnew String("You are trying to congfigure the AUX input as both a pulse width qualifier and a reference clock.");
	case PicoStatus::UnableToOpenScalingFile:
		return gcnew String("The scaling file set can not be opened.");
	case PicoStatus::MemoryClockFrequency:
		return gcnew String("The frequency of the memory is reporting incorrectly.");
	case PicoStatus::I2CNotResponding:
		return gcnew String("The I2C that is being actioned is not responding to requests.");
	case PicoStatus::NoCapturesAvailable:
		return gcnew String("There are no captures available and therefore no data can be returned.");
	case PicoStatus::NotUsedInThisCaptureMode:
		return gcnew String("The capture mode the device is currently running in does not support the current request.");
	case PicoStatus::IpNetworked:
		return gcnew String("The device is currently connected via the IP Network socket and thus the call made is not supported.");
	case PicoStatus::InvalidIpAddress:
		return gcnew String("An IP address that is not correct has been passed to the driver.");
	case PicoStatus::IpSocketFailed:
		return gcnew String("The IP socket has failed.");
	case PicoStatus::IpSocketTimeout:
		return gcnew String("The IP socket has timed out.");
	case PicoStatus::SettingsFailed:
		return gcnew String("The settings requested have failed to be set.");
	case PicoStatus::NetworkFailed:
		return gcnew String("The network connection has failed.");
	case PicoStatus::Ws2DllNotLoaded:
		return gcnew String("Unable to load the WS2 dll.");
	case PicoStatus::InvalidIpPort:
		return gcnew String("The IP port is invalid.");
	case PicoStatus::CouplingNotSupported:
		return gcnew String("The type of coupling requested is not supported on the opened device.");
	case PicoStatus::BandwidthNotSupported:
		return gcnew String("Bandwidth limit is not supported on the opened device.");
	case PicoStatus::InvalidBandwidth:
		return gcnew String("The value requested for the bandwidth limit is out of range.");
	case PicoStatus::AwgNotSupported:
		return gcnew String("The arbitrary waveform generator is not supported by the opened device.");
	case PicoStatus::EtsNotRunning:
		return gcnew String("Data has been requested with ETS mode set but run block has not been called, or stop has been called.");
	case PicoStatus::SigGenWhiteNoiseNotSupported:
		return gcnew String("White noise is not supported on the opened device.");
	case PicoStatus::SigGenWaveTypeNotSupported:
		return gcnew String("The wave type requested is not supported by the opened device.");
	case PicoStatus::EtsNotAvailableWithLogicChannels:
		return gcnew String("When a digital port is enabled, ETS sample mode is not available for use.");
	case PicoStatus::PowerSupplyConnected:
		return gcnew String("The DC power supply is connected.");
	case PicoStatus::PowerSupplyNotConnected:
		return gcnew String("The DC power supply isn’t connected.");
	case PicoStatus::PowerSupplyRequestInvalid:
		return gcnew String("Incorrect power mode passed for current power source.");
	case PicoStatus::PowerSupplyUnderVoltage:
		return gcnew String("The supply voltage from the USB source is too low.");
	case PicoStatus::CapturingData:
		return gcnew String("The device is currently busy capturing data.");
	case PicoStatus::Usb3DeviceNotUsb3Port:
		return gcnew String("A Pico USB 3.0 device has been connected to a non-USB 3.0 port.");
	case PicoStatus::NotSupportedByThisDevice:
		return gcnew String("A function has been called that is not supported by the current device variant.");
	case PicoStatus::InvalidDeviceResolution:
		return gcnew String("The device resolution is invalid (out of range).");
	case PicoStatus::InvalidNumberOfChannelsForResolution:
		return gcnew String("The number of channels which can be enabled is limited in 15 and 16-bit modes.");
	case PicoStatus::ChannelDisabledDueToUsbPower:
		return gcnew String("USB Power not sufficient to power all channels.");
	default:
		return String::Format("Failed with unexpected PicoStatus value: {0}.", status);
	}
}