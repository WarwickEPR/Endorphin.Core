#include "Stdafx.h"

#include "PicoScopeDriver.h"
#include "ps5000aApi.h"
#include <string>

using namespace PicoScopeDriver;
using namespace Runtime::InteropServices;

// Private members

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

// Internal members

void PicoScope5000::CheckStatus(PicoStatus status) {
	if (status != PicoStatus::Ok && status != PicoStatus::PowerSupplyNotConnected) {
		throw gcnew PicoException(status);
	}
}

// Static members

array<String^>^ PicoScope5000::GetConnectedUnitSerials() {
	short count;
	short length = 32;
	char buffer[32];
	auto bufferCstr = const_cast<int8_t*> (reinterpret_cast<const int8_t*> (buffer));

	auto status = (PicoStatus)ps5000aEnumerateUnits(&count, bufferCstr, &length);
	CheckStatus(status);

	auto results = gcnew String(buffer, 0, length);
	return results->Split(String(",").ToCharArray());
}

float PicoScope5000::RangeInVolts(Range range) {
	switch (range) {
	case Range::_10mV: return 0.0010;
	case Range::_20mV: return 0.020;
	case Range::_50mV: return 0.050;
	case Range::_100mV: return 0.100;
	case Range::_200mV: return 0.200;
	case Range::_500mV: return 0.500;
	case Range::_1V: return 1.0;
	case Range::_2V: return 2.0;
	case Range::_5V: return 5.0;
	case Range::_10V: return 10.0;
	case Range::_20V: return 20.0;
	case Range::_50V: return 50.0;
	default: throw gcnew Exception("Unexpected range.");
	}
}

// Constructors / destructor

PicoScope5000::PicoScope5000(Resolution resolution) {
	pin_ptr<short> pinnedHandle = &_handle;
	auto status = (PicoStatus)ps5000aOpenUnit(pinnedHandle, NULL, (PS5000A_DEVICE_RESOLUTION)resolution);
	CheckStatus(status);
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
}

PicoScope5000::~PicoScope5000() {
	auto status = (PicoStatus)ps5000aCloseUnit(_handle);
	CheckStatus(status);
}

// Device information

Dictionary<PicoInfo, String^>^ PicoScope5000::GetUnitInfo() {
	auto results = gcnew Dictionary<PicoInfo, String^>();
	for (int infoValue = (int)(PicoInfo::DriverVersion); infoValue != (int)(PicoInfo::MacAddress); infoValue++) {
		auto info = (PicoInfo)infoValue;
		results->Add(info, GetUnitInfoValue(_handle, info));
	}
	return results;
}

// Device status

bool PicoScope5000::UnitIsMainsPowered::get() {
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

void PicoScope5000::UnitIsMainsPowered::set(bool mainsPowered) {
	auto powerState = mainsPowered ? PicoStatus::PowerSupplyConnected : PicoStatus::PowerSupplyNotConnected;
	auto status = (PicoStatus) ps5000aChangePowerSource(_handle, (PICO_STATUS)powerState);
	CheckStatus(status);
}

void PicoScope5000::FlashLed(short count) {
	auto status = (PicoStatus)ps5000aFlashLed(_handle, count);
	CheckStatus(status);
}

void PicoScope5000::Ping() {
	auto status = (PicoStatus)ps5000aPingUnit(_handle);
	CheckStatus(status);
}

// Timebases

float PicoScope5000::GetTimebaseIntervalInNanoseconds(unsigned int timebase, unsigned int segmentIndex, [Out] int% maximumNumberOfSamples) {
	float interval;
	pin_ptr<int> maximumSamplesPtr = &maximumNumberOfSamples;
	auto status = (PicoStatus)ps5000aGetTimebase2(_handle, timebase, 0, &interval, maximumSamplesPtr, segmentIndex);
	CheckStatus(status);
	return interval;
}

unsigned int PicoScope5000::GetFastestTimebase(Resolution resolution) {
	switch (resolution) {
	case Resolution::_8bit: return 0;
	case Resolution::_12bit: return 1;
	case Resolution::_14bit:
	case Resolution::_15bit: return 3;
	case Resolution::_16bit: return 4;
	}
	throw gcnew Exception("Unexpected device resolution.");
}

unsigned int PicoScope5000::GetFastestStreamingIntervalInNanoseconds(Resolution resolution, int channelCount) {
	switch (resolution) {
	case Resolution::_8bit:
		switch (channelCount) {
		case 4:
		case 3: return 128;	// 7.8125 MS / s(128 ns per sample) when three or four channels are active => 18
		case 2: return 64;	// 15.625 MS / s(64 ns per sample) when two channels are active => 10
		case 1: return 32;	// 31.25 MS / s(32 ns per sample) when one channel is active => 6
		}
	case Resolution::_12bit:
		switch (channelCount) {
		case 4:
		case 3: return 256;	// 3.906 MS / s(256 ns per sample) when three or four channels are active => 19
		case 2: return 128;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 11
		case 1: return 64;	// 15.625 MS / s(64 ns per sample) when one channel is active => 7
		}
	case Resolution::_14bit:
		switch (channelCount) {
		case 4:
		case 3: return 256;	// 3.906 MS / s(256 ns per sample) when three or four channels are active => 34
		case 2: return 128;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 18
		case 1: return 64;	// 15.625 MS / s(64 ns per sample) when one channel is active => 10
		}
	case Resolution::_15bit:
		switch (channelCount) {
		case 4:
		case 3: throw gcnew System::Exception("Unexpected number of channels for resolution.");
		case 2: return 128;	// 7.8125 MS / s(128 ns per sample) when two channels are active => 18
		case 1: return 64;	// 15.625 MS / s(64 ns per sample) when one channel is active => 10
		}
	case Resolution::_16bit:
		switch (channelCount) {
		case 4:
		case 3:
		case 2: throw gcnew System::Exception("Unexpected number of channels for resolution.");
		case 1: return 64;	// 15.625 MS / s(64 ns per sample) when one channel is active => 7
		}
	}
	throw gcnew Exception("Unexpected resolution or enabled channel count.");
}

Resolution PicoScope5000::DeviceResolution::get() { 
	Resolution resolution;
	auto status = (PicoStatus)ps5000aGetDeviceResolution(_handle, (PS5000A_DEVICE_RESOLUTION*)&resolution);
	CheckStatus(status);
	return resolution;
}

// Channel setup

void PicoScope5000::DeviceResolution::set(Resolution resolution) {
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

void PicoScope5000::SetChannel(Channel channel, bool enabled, Coupling coupling, Range range, float analogueOffset) {
	ps5000aSetChannel(_handle, (PS5000A_CHANNEL)channel, enabled, (PS5000A_COUPLING)coupling, (PS5000A_RANGE)range, analogueOffset);
}

void PicoScope5000::SetBandwidth(Channel channel, BandwidthLimit bandwidth) {
	auto status = (PicoStatus)ps5000aSetBandwidthFilter(_handle, (PS5000A_CHANNEL)channel, (PS5000A_BANDWIDTH_LIMITER)bandwidth);
	CheckStatus(status);
}

void PicoScope5000::DisableChannel(Channel channel) {
	ps5000aSetChannel(_handle, (PS5000A_CHANNEL)channel, 0, (PS5000A_COUPLING)Coupling::DC, (PS5000A_RANGE)Range::_5V, 0.0);
}

int PicoScope5000::GetMaximumNumberOfChannels(Resolution resolution) {
	switch (resolution) {
	case Resolution::_16bit: return 1;
	case Resolution::_15bit: return 2;
	default: return 4;
	}
}

// Trigger setup 

void PicoScope5000::DisableTrigger() {
	auto status = (PicoStatus)ps5000aSetSimpleTrigger(_handle, 0, (PS5000A_CHANNEL)Channel::A, 0,
		(PS5000A_THRESHOLD_DIRECTION)ThresholdDirection::None, 0, 0);
	CheckStatus(status);
}

void PicoScope5000::SetAutoTrigger(short delayInMilliseconds) {
	auto status = (PicoStatus)ps5000aSetSimpleTrigger(_handle, 0, (PS5000A_CHANNEL)Channel::A, 0,
		(PS5000A_THRESHOLD_DIRECTION)ThresholdDirection::None, 0, delayInMilliseconds);
	CheckStatus(status);
}

void PicoScope5000::SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned int delay) {
	SetSimpleTrigger(source, threshold, direction, delay, 0);
}

void PicoScope5000::SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned int delay, short autoDelayInMilliseconds) {
	auto status = (PicoStatus)ps5000aSetSimpleTrigger(_handle, 1, (PS5000A_CHANNEL)source, threshold,
		(PS5000A_THRESHOLD_DIRECTION)direction, delay, autoDelayInMilliseconds);
	CheckStatus(status);
}

void PicoScope5000::SetTriggerDelay(unsigned int delaySamples) {
	auto status = (PicoStatus)ps5000aSetTriggerDelay(_handle, delaySamples);
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

// Buffer setup and reaodut

unsigned int PicoScope5000::CurrentSegmentIndex::get() {
	return _currentSegmentIndex;
}

void PicoScope5000::CurrentSegmentIndex::set(unsigned int segmentIndex) {
	_currentSegmentIndex = segmentIndex;
}

int PicoScope5000::SetNumberOfSegmentsAndGetSamplesPerSegment(unsigned int numberOfSegments) {
	int result;
	auto status = (PicoStatus)ps5000aMemorySegments(_handle, numberOfSegments, &result);
	CheckStatus(status);
	return result;
}

unsigned int PicoScope5000::GetMaximumNumberOfSegments() {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetMaxSegments(_handle, &result);
	CheckStatus(status);
	return result;
}

short PicoScope5000::GetMinimumAdcCountsForCurrentResolution() {
	short value;
	auto status = (PicoStatus)ps5000aMinimumValue(_handle, &value);
	CheckStatus(status);
	return value;
}

short PicoScope5000::GetMaximumAdcCountsForCurrentResolution() {
	short value;
	auto status = (PicoStatus)ps5000aMaximumValue(_handle, &value);
	CheckStatus(status);
	return value;
}

unsigned int PicoScope5000::GetMaximumDownsamplingRatio(unsigned int numberOfUnaggregatedSamples, Downsampling ratioMode, unsigned int segmentIndex) {
	unsigned int result;
	auto status = (PicoStatus)ps5000aGetMaxDownSampleRatio(_handle, numberOfUnaggregatedSamples, &result, (PS5000A_RATIO_MODE)ratioMode, segmentIndex);
	CheckStatus(status);
	return result;
}

IInt16BufferHandle^ PicoScope5000::CreateUnmanagedBuffer(Channel channel, Downsampling downsampling, int bufferLength, unsigned int segmentIndex) {
	auto bufferHandle = gcnew UnmanagedInt16BufferHandle(bufferLength);
	auto status = (PicoStatus)ps5000aSetDataBuffer(_handle, (PS5000A_CHANNEL)channel, bufferHandle->BufferPointer(), bufferHandle->BufferSize, segmentIndex, 
		(PS5000A_RATIO_MODE)downsampling);
	CheckStatus(status);
	return bufferHandle;
}

IInt16BufferHandle^ PicoScope5000::CreatePinnedBuffer(Channel channel, Downsampling downsampling, array<Int16>^ buffer, unsigned int segmentIndex) {
	auto bufferHandle = gcnew PinnedInt16BufferHandle(buffer);
	auto status = (PicoStatus)ps5000aSetDataBuffer(_handle, (PS5000A_CHANNEL)channel, bufferHandle->BufferPointer(), bufferHandle->BufferSize, segmentIndex,
		(PS5000A_RATIO_MODE)downsampling);
	CheckStatus(status);
	return bufferHandle;
}

Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ PicoScope5000::CreateUnmanagedBuffers(Channel channel, int bufferLength, unsigned int segmentIndex) {
	auto maxBufferHandle = gcnew UnmanagedInt16BufferHandle(bufferLength);
	auto minBufferHandle = gcnew UnmanagedInt16BufferHandle(bufferLength);
	auto status = (PicoStatus)ps5000aSetDataBuffers(_handle, (PS5000A_CHANNEL)channel, maxBufferHandle->BufferPointer(), minBufferHandle->BufferPointer(),
		bufferLength, segmentIndex, (PS5000A_RATIO_MODE)Downsampling::Aggregate);
	CheckStatus(status);
	return gcnew Tuple<IInt16BufferHandle^, IInt16BufferHandle^>(maxBufferHandle, minBufferHandle);
}

Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ PicoScope5000::CreatePinnedBuffers(Channel channel, array<Int16>^ bufferMax,
	array<Int16>^ bufferMin, unsigned int segmentIndex) {

	auto maxBufferHandle = gcnew PinnedInt16BufferHandle(bufferMax);
	auto minBufferHandle = gcnew PinnedInt16BufferHandle(bufferMin);
	auto status = (PicoStatus)ps5000aSetDataBuffers(_handle, (PS5000A_CHANNEL)channel, maxBufferHandle->BufferPointer(), minBufferHandle->BufferPointer(),
		maxBufferHandle->BufferSize, segmentIndex, (PS5000A_RATIO_MODE)Downsampling::Aggregate);
	CheckStatus(status);
	return gcnew Tuple<IInt16BufferHandle^, IInt16BufferHandle^>(maxBufferHandle, minBufferHandle);
}

// Streaming acquisition

StreamingAcquisition^ PicoScope5000::RunStreaming(unsigned int% timeInterval, TimeUnit timeUnit, unsigned int maxPreTriggerSamples, 
	unsigned int maxPostTriggerSamples, bool autoStop, Downsampling downsamplingModes, unsigned int downsamplingRatio, unsigned int bufferSize,
	StreamDataReady^ dataCallback, StreamFinished^ finishedCallback) {
	return gcnew StreamingAcquisition(_handle, timeInterval, timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop, downsamplingModes,
		downsamplingRatio, bufferSize, dataCallback, finishedCallback);
}
