#include "Stdafx.h"

#include "PicoScopeDriver.h"
#include "ps5000aApi.h"
#include <string>

using namespace PicoScopeDriver;
using namespace Runtime::InteropServices;

void PicoScope5000::CheckStatus(PicoStatus status) {
	if (status != PicoStatus::Ok && status != PicoStatus::PowerSupplyNotConnected) {
		throw gcnew PicoException(status);
	}
}

bool PicoScope5000::CheckChannelSettingsForResolution(Resolution resolution) {
	return GetEnabledChannelCount() <= GetMaximumChannelsForResolution(resolution);
}

void PicoScope5000::SetDefaultChannelSettings(Resolution resolution) {
	int enabledCount = GetMaximumChannelsForResolution(resolution);
	for (int i = (int)Channel::A; i <= (int)Channel::D; i++) {
		auto channel = (Channel)i;
		if (i <= enabledCount)	{ _channelSettings[channel] = ChannelSettings::DefaultEnabled(); }
		else					{ _channelSettings[channel] = ChannelSettings::DefaultDisabled(); }

		ps5000aSetChannel(_handle, (PS5000A_CHANNEL)i, _channelSettings[channel]->enabled, 
			(PS5000A_COUPLING)_channelSettings[channel]->coupling, (PS5000A_RANGE)_channelSettings[channel]->range, 
			_channelSettings[channel]->analogueOffset);
	}
}

String^ PicoScope5000::GetUnitInfoValue(short handle, PicoInfo info) {
	auto buffer = std::string(32, ' ');
	short requiredSize;
	auto bufferCstr = const_cast<int8_t*> (reinterpret_cast<const int8_t*> (buffer.c_str()));
	
	auto status = (PicoStatus)ps5000aGetUnitInfo(handle, bufferCstr, 32, &requiredSize, (PICO_INFO)info);
	CheckStatus(status);
	if (requiredSize > 32) {
		throw gcnew Exception("Call to ps5000aGetUnitInfo returned a result which exceeds the buffer length.");
	}

	return String(buffer.c_str()).TrimEnd(String(" ").ToCharArray());
}

PicoScope5000::PicoScope5000(Resolution resolution) {
	pin_ptr<short> pinnedHandle = &_handle;
	auto status = (PicoStatus)ps5000aOpenUnit(pinnedHandle, (int8_t*)IntPtr::Zero.ToPointer(), (PS5000A_DEVICE_RESOLUTION)resolution);
	CheckStatus(status);
	
	SetDefaultChannelSettings(resolution);
}

PicoScope5000::PicoScope5000(String^ serial, Resolution resolution) {
	pin_ptr<short> pinnedHandle = &_handle;
	PicoStatus status;

	if (serial == nullptr) {
		throw gcnew Exception("Called PicoScope5000.OpenUnit with null serial string. If you would like to open the first found PicoScope, use the overload with no serial parameter.");
	}
	else {
		auto unmanagedSerial = Marshal::StringToHGlobalAnsi(serial);
		status = (PicoStatus) ps5000aOpenUnit(pinnedHandle, (int8_t*)unmanagedSerial.ToPointer(), (PS5000A_DEVICE_RESOLUTION)resolution);
		Marshal::FreeHGlobal(unmanagedSerial);
	}
	CheckStatus(status);

	SetDefaultChannelSettings(resolution);
}

PicoScope5000::~PicoScope5000() {
	auto status = (PicoStatus)ps5000aCloseUnit(_handle);
	CheckStatus(status);
}

Dictionary<PicoInfo, String^>^ PicoScope5000::GetUnitInfo() {
	auto results = gcnew Dictionary<PicoInfo, String^>();
	for (int infoValue = (int)(PicoInfo::DriverVersion); infoValue != (int)(PicoInfo::MacAddress); infoValue++) {
		auto info = (PicoInfo)infoValue;
		results->Add(info, GetUnitInfoValue(_handle, info));
	}
	return results;
}

array<String^>^ PicoScope5000::GetConnectedUnitSerials() {
	short count;
	short length = 32;
	auto buffer = std::string(' ', length);
	auto bufferCstr = const_cast<int8_t*> (reinterpret_cast<const int8_t*> (buffer.c_str()));
	
	auto status = (PicoStatus) ps5000aEnumerateUnits(&count, bufferCstr, &length);
	CheckStatus(status);

	auto results = gcnew String(buffer.c_str(), 0, length);
	return results->Split(String(",").ToCharArray());
}

bool PicoScope5000::GetUnitIsMainsPowered() {
	auto status = (PicoStatus)ps5000aCurrentPowerSource(_handle);
	switch (status) {
	case PicoStatus::PowerSupplyConnected:
		return true;
	case PicoStatus::PowerSupplyNotConnected:
		return false;
	default:
		throw gcnew PicoException(status);
	}
}

void PicoScope5000::SetUnitIsMainsPowered(bool mainsPowered) {
	auto powerState = mainsPowered ? PicoStatus::PowerSupplyConnected : PicoStatus::PowerSupplyNotConnected;
	auto status = (PicoStatus) ps5000aChangePowerSource(_handle, (PICO_STATUS)powerState);
	CheckStatus(status);
}

Resolution PicoScope5000::GetDeviceResolution() {
	Resolution resolution;
	auto status = (PicoStatus)ps5000aGetDeviceResolution(_handle, (PS5000A_DEVICE_RESOLUTION*)&resolution);
	CheckStatus(status);
	return resolution;
}

void PicoScope5000::SetDeviceResolution(Resolution resolution) {
	if (GetEnabledChannelCount() > GetMaximumChannelsForResolution(resolution)) {
		throw gcnew Exception("Enabled channel count exceeds maximum allowed channels for requested resolution.");
	}
	auto status = (PicoStatus)ps5000aSetDeviceResolution(_handle, (PS5000A_DEVICE_RESOLUTION)resolution);
	CheckStatus(status);
}

void PicoScope5000::GetAnalogueOffsetLimits(Range range, Coupling coupling, [Out] float% maxVoltage, [Out] float% minVoltage) {
	pin_ptr<float> pinnedMaxVoltage = &maxVoltage;
	pin_ptr<float> pinnedMinVoltage = &minVoltage;
	auto status = (PicoStatus) ps5000aGetAnalogueOffset(_handle, (PS5000A_RANGE)range, (PS5000A_COUPLING)coupling, pinnedMaxVoltage, pinnedMinVoltage);
	CheckStatus(status);
}

array<Range>^ PicoScope5000::GetAvailableChannelRanges(Channel channel) {
	int ranges[12], rangeCount = 12;
	auto status = (PicoStatus)ps5000aGetChannelInformation(_handle, PS5000A_CI_RANGES, 0, ranges, &rangeCount, (int32_t) channel);
	CheckStatus(status);

	auto results = gcnew array<Range>(rangeCount);
	for (int i = 0; i < rangeCount; i++) {
		results[i] = (Range) ranges[i];
	}
	return results;
}

unsigned int PicoScope5000::GetMaximumDownsampleRatio
	(unsigned long numberOfUnaggregatedSegments, RatioMode ratioMode, unsigned long segmentIndex) {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetMaxDownSampleRatio(_handle, numberOfUnaggregatedSegments, &result, (PS5000A_RATIO_MODE)ratioMode, segmentIndex);
	CheckStatus(status);
	return result;
}

unsigned int PicoScope5000::GetMaximumSegments() {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetMaxSegments(_handle, &result);
	CheckStatus(status);
	return result;
}

unsigned int PicoScope5000::GetNumberOfCaptures() {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetNoOfCaptures(_handle, &result);
	CheckStatus(status);
	return result;
}

unsigned int PicoScope5000::GetNumberOfProcessedCaptures() {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetNoOfProcessedCaptures(_handle, &result);
	CheckStatus(status);
	return result;
}

void PicoScope5000::FlashLed(short count) {
	auto status = (PicoStatus)ps5000aFlashLed(_handle, count);
	CheckStatus(status);
}

void PicoScope5000::Ping() {
	auto status = (PicoStatus)ps5000aPingUnit(_handle);
	CheckStatus(status);
}

short PicoScope5000::GetMinimumAdcValue() {
	short value;
	auto status = (PicoStatus) ps5000aMinimumValue(_handle, &value);
	CheckStatus(status);
	return value;
}

short PicoScope5000::GetMaximumAdcValue() {
	short value;
	auto status = (PicoStatus)ps5000aMaximumValue(_handle, &value);
	CheckStatus(status);
	return value;
}

float PicoScope5000::GetTimebaseIntervalInNanoseconds(unsigned int timebase, unsigned int segmentIndex, [Out] int% maximumNumberOfSamples) {
	float interval;
	pin_ptr<int> maximumSamplesPtr = &maximumNumberOfSamples;
	auto status = (PicoStatus) ps5000aGetTimebase2(_handle, timebase, 0, &interval, maximumSamplesPtr, segmentIndex);
	CheckStatus(status);
	return interval;
}

unsigned int PicoScope5000::GetMinimumSampleIntervalTimebase() {
	switch (GetDeviceResolution()) {
	case Resolution::_8bit: return 0;
	case Resolution::_12bit: return 1;
	case Resolution::_14bit:
	case Resolution::_15bit: return 3;
	case Resolution::_16bit: return 4;
	}
}

unsigned int PicoScope5000::GetMinimumStreamingSampleIntervalTimeBase() {
	auto channelCount = GetEnabledChannelCount();
	switch (GetDeviceResolution()) {
	case Resolution::_8bit:
		switch (GetEnabledChannelCount()) {
		case 4: 
		case 3: return 18;	// 7.8125 MS / s(128 ns per sample) when three or four channels are active => 18
		case 2: return 10;	// 15.625 MS / s(64 ns per sample) when two channels are active => 10
		case 1: return 6;	// 31.25 MS / s(32 ns per sample) when one channel is active => 6
		}
	case Resolution::_12bit:
		switch (GetEnabledChannelCount()) {
		case 4:
		case 3: return 19;	// 3.906 MS / s(256 ns per sample) when three or four channels are active => 19
		case 2: return 11;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 11
		case 1: return 7;	// 15.625 MS / s(64 ns per sample) when one channel is active => 7
		}
	case Resolution::_14bit:
		switch (GetEnabledChannelCount()) {
		case 4:
		case 3: return 34;	// 3.906 MS / s(256 ns per sample) when three or four channels are active => 34
		case 2: return 18;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 18
		case 1: return 10;	// 15.625 MS / s(64 ns per sample) when one channel is active => 10
		}
	case Resolution::_15bit:
		switch (GetEnabledChannelCount()) {
		case 4:
		case 3: throw gcnew System::Exception("Unexpected number of channels enabled for current resolution.");
		case 2: return 18;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 18
		case 1: return 10;	// 15.625 MS / s(64 ns per sample) when one channel is active => 10
		}
	case Resolution::_16bit:
		switch (GetEnabledChannelCount()) {
		case 4:
		case 3: 
		case 2: throw gcnew System::Exception("Unexpected number of channels enabled for current resolution.");
		case 1: return 7;	// 15.625 MS / s(64 ns per sample) when one channel is active => 7
		}
	}
}

void PicoScope5000::SetChannel(Channel channel, bool enabled, Coupling coupling, Range range, float analogueOffset) {
	_channelSettings[channel]->enabled = enabled;
	_channelSettings[channel]->coupling = coupling;
	_channelSettings[channel]->range = range;
	_channelSettings[channel]->analogueOffset = analogueOffset;
	ps5000aSetChannel(_handle, (PS5000A_CHANNEL)channel, _channelSettings[channel]->enabled,
		(PS5000A_COUPLING)_channelSettings[channel]->coupling, (PS5000A_RANGE)_channelSettings[channel]->range,
		_channelSettings[channel]->analogueOffset);
}

void PicoScope5000::SetBandwidth(Channel channel, BandwidthLimit bandwidth) {
	auto status = (PicoStatus)ps5000aSetBandwidthFilter(_handle, (PS5000A_CHANNEL)channel, (PS5000A_BANDWIDTH_LIMITER)bandwidth);
	CheckStatus(status);
}

void PicoScope5000::DisableChannel(Channel channel) {
	_channelSettings[channel]->enabled = false;
	ps5000aSetChannel(_handle, (PS5000A_CHANNEL)channel, _channelSettings[channel]->enabled,
		(PS5000A_COUPLING)_channelSettings[channel]->coupling, (PS5000A_RANGE)_channelSettings[channel]->range,
		_channelSettings[channel]->analogueOffset);
}

int PicoScope5000::GetMaximumChannelsForResolution(Resolution resolution) {
	switch (resolution) {
	case Resolution::_16bit: return 1;
	case Resolution::_15bit: return 2;
	default: return 4;
	}
}

int PicoScope5000::GetEnabledChannelCount() {
	int enabledCount = 0;
	for each(KeyValuePair<Channel, ChannelSettings^>^ setting in _channelSettings) {
		if (setting->Value->enabled) {
			++enabledCount;
		}
	}
	return enabledCount;
}

void PicoScope5000::DisableTrigger() {
	auto status = (PicoStatus) ps5000aSetSimpleTrigger(_handle, 0, (PS5000A_CHANNEL)Channel::A, 0, 
		(PS5000A_THRESHOLD_DIRECTION)ThresholdDirection::None, 0, 0);
	CheckStatus(status);
}

void PicoScope5000::SetAutoTrigger(short delayInMilliseconds) {
	auto status = (PicoStatus) ps5000aSetSimpleTrigger(_handle, 0, (PS5000A_CHANNEL)Channel::A, 0,
		(PS5000A_THRESHOLD_DIRECTION)ThresholdDirection::None, 0, delayInMilliseconds);
	CheckStatus(status);
}

void PicoScope5000::SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay) {
	SetSimpleTrigger(source, threshold, direction, delay, 0);
}

void PicoScope5000::SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay, short autoDelayInMilliseconds) {
	auto status = (PicoStatus) ps5000aSetSimpleTrigger(_handle, 1, (PS5000A_CHANNEL)source, threshold,
		(PS5000A_THRESHOLD_DIRECTION)direction, delay, autoDelayInMilliseconds);
	CheckStatus(status);
}

void PicoScope5000::SetTriggerDelay(unsigned long delaySamples) {
	auto status = (PicoStatus) ps5000aSetTriggerDelay(_handle, delaySamples);
	CheckStatus(status);
}

bool PicoScope5000::IsTriggerEnabled() {
	short trigEnabled, pwqEnabled;
	auto status = (PicoStatus)ps5000aIsTriggerOrPulseWidthQualifierEnabled(_handle, &trigEnabled, &pwqEnabled);
	return (trigEnabled != 0);
}

bool PicoScope5000::IsPulseWidthQualifierEnabled() {
	short trigEnabled, pwqEnabled;
	auto status = (PicoStatus)ps5000aIsTriggerOrPulseWidthQualifierEnabled(_handle, &trigEnabled, &pwqEnabled);
	return (pwqEnabled != 0);
}

void PicoScope5000::IsTriggerOrPulseWidthQualifierEnabled([Out] bool% triggerEnabled, [Out] bool% pulseWidthQualifierEnabled) {
	short trigEnabled, pwqEnabled;
	auto status = (PicoStatus)ps5000aIsTriggerOrPulseWidthQualifierEnabled(_handle, &trigEnabled, &pwqEnabled);
	triggerEnabled = (trigEnabled != 0);
	pulseWidthQualifierEnabled = (pwqEnabled != 0);
}

StreamingAcquisition^ PicoScope5000::RunStreaming(StreamData^ dataCallback, StreamFinished^ finishedCallback) {
	return gcnew StreamingAcquisition(_handle, dataCallback, finishedCallback);
}
