// PicoScopeDriver.h

#pragma once

#include "Stdafx.h"
#include "StreamingAcquisition.h"
#include "IInt16BufferHandle.h"

using namespace System;
using namespace System::Collections::Concurrent;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;

namespace PicoScopeDriver {
	public ref class PicoScope5000
	{
	private:
		short _handle;
		unsigned int _currentSegmentIndex = 0;

		static String^ GetUnitInfoValue(short handle, PicoInfo info);

	internal:
		static void CheckStatus(PicoStatus status);

	public:
		// Static members

		static array<String^>^ GetConnectedUnitSerials();

		// Constructors / destructor

		PicoScope5000() : PicoScope5000(Resolution::_8bit) {};
		PicoScope5000(Resolution resolution);
		PicoScope5000(String^ serial) : PicoScope5000(serial, Resolution::_8bit) {};
		PicoScope5000(String^ serial, Resolution resolution);
		~PicoScope5000();

		// Device information

		String^ GetUnitDriverVersion()				{ return GetUnitInfoValue(_handle, PicoInfo::DriverVersion); }
		String^ GetUnitUsbVersion()					{ return GetUnitInfoValue(_handle, PicoInfo::UsbVersion); }
		String^ GetUnitHardwareVersion()			{ return GetUnitInfoValue(_handle, PicoInfo::HardwareVersion); }
		String^ GetUnitVariantInfo()				{ return GetUnitInfoValue(_handle, PicoInfo::VariantInfo); }
		String^ GetUnitSerial()						{ return GetUnitInfoValue(_handle, PicoInfo::BatchAndSerial); }
		String^ GetUnitCalibrationDate()			{ return GetUnitInfoValue(_handle, PicoInfo::CalibrationDate); }
		String^ GetUnitKernelVersion()				{ return GetUnitInfoValue(_handle, PicoInfo::KernelVersion); }
		String^ GetUnitDigitalHardwareVersion()		{ return GetUnitInfoValue(_handle, PicoInfo::DigitalHardwareVersion); }
		String^ GetUnitAnalogueHardwareVersion()	{ return GetUnitInfoValue(_handle, PicoInfo::AnalogueHardwareVersion); }
		String^ GetUnitFirmwareVersion1()			{ return GetUnitInfoValue(_handle, PicoInfo::FirmwareVersion1); }
		String^ GetUnitFirmwareVersion2()			{ return GetUnitInfoValue(_handle, PicoInfo::FirmwareVersion2); }
		String^ GetUnitMacAddress()					{ return GetUnitInfoValue(_handle, PicoInfo::MacAddress); }
		Dictionary<PicoInfo, String^>^ GetUnitInfo();

		// Device status

		property bool UnitIsMainsPowered { bool get(); void set(bool mainsPower); }

		void FlashLed(short count);
		void Ping();

		// Timebases

		float GetTimebaseIntervalInNanoseconds(unsigned int timebase, unsigned int segmentIndex, [Out] int% maximumNumberOfSamples);
		unsigned int GetFastestTimebase(Resolution resolution);
		unsigned int GetFastestStreamingIntervalInNanoseconds(Resolution resolution, int channelCount);

		// Channel setup

		property Resolution DeviceResolution { Resolution get(); void set(Resolution resolution); }

		void GetAnalogueOffsetLimits(Range range, Coupling coupling, [Out] float% maxVoltage, [Out] float% minVoltage);
		array<Range>^ GetAvailableChannelRanges(Channel channel);

		void SetChannel(Channel channel, bool enabled, Coupling coupling, Range range, float analogueOffset);
		void SetBandwidth(Channel channel, BandwidthLimit bandwidth);
		void DisableChannel(Channel channel);
		int GetMaximumChannelsForResolution(Resolution resolution);

		// Trigger setup

		void DisableTrigger();
		void SetAutoTrigger(short delayInMilliseconds);
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay);
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay, short autoDelayInMilliseconds);
		void SetTriggerDelay(unsigned long delaySamples);
		bool IsTriggerEnabled();
		bool IsPulseWidthQualifierEnabled();
		void IsTriggerOrPulseWidthQualifierEnabled([Out] bool% triggerEnabled, [Out] bool% pulseWidthQualifierEnabled);

		// Buffer setup and reaodut
		
		property unsigned int CurrentSegmentIndex { unsigned int get(); void set(unsigned int); }
		int SetNumberOfSegmentsAndGetSamplesPerSegment(unsigned int numberOfSegments);
		unsigned int GetMaximumNumberOfSegments();

		short GetMinimumAdcValueForCurrentResolution();
		short GetMaximumAdcValueForCurrentResolution();

		unsigned int GetMaximumDownsamplingRatio(unsigned long numberOfUnaggregatedSamples, Downsampling downsampling, unsigned long segmentIndex);
		unsigned int GetMaximumDownsamplingRatio(unsigned long numberOfUnaggregatedSamples, Downsampling downsampling)
		{ return GetMaximumDownsamplingRatio(numberOfUnaggregatedSamples, downsampling, CurrentSegmentIndex); }

		IInt16BufferHandle^ CreateUnmanagedBuffer(Channel channel, Downsampling downsampling, int bufferLength, unsigned int segmentIndex);
		IInt16BufferHandle^ CreateUnmanagedBuffer(Channel channel, Downsampling downsampling, int bufferLength)
		{ return CreateUnmanagedBuffer(channel, downsampling, bufferLength, CurrentSegmentIndex); }

		IInt16BufferHandle^ CreatePinnedBuffer(Channel channel, Downsampling downsampling, array<Int16>^ buffer, unsigned int segmentIndex);
		IInt16BufferHandle^ CreatePinnedBuffer(Channel channel, Downsampling downsampling, array<Int16>^ buffer)
		{ return CreatePinnedBuffer(channel, downsampling, buffer, CurrentSegmentIndex); }

		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreateUnmanagedBuffers(Channel channel, Downsampling downsampling, int bufferLength,
			unsigned int segmentIndex);
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreateUnmanagedBuffers(Channel channel, Downsampling downsampling, int bufferLength)
		{ return CreateUnmanagedBuffers(channel, downsampling, bufferLength, CurrentSegmentIndex); }

		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreatePinnedBuffers(Channel channel, Downsampling downsampling, array<Int16>^ bufferMax,
			array<Int16>^ bufferMin, unsigned int segmentIndex);
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreatePinnedBuffers(Channel channel, Downsampling downsampling, array<Int16>^ bufferMax,
			array<Int16>^ bufferMin) { return CreatePinnedBuffers(channel, downsampling, bufferMax, bufferMin, CurrentSegmentIndex); }

		// Streaming acquisition

		StreamingAcquisition^ RunStreaming(unsigned int% timeInterval, TimeUnit timeUnit, unsigned int maxPreTriggerSamples,
			unsigned int maxPostTriggerSamples, bool autoStop, Downsampling downsamplingModes, unsigned int downsamplingRatio, unsigned int bufferSize, 
			StreamDataReady^ dataCallback, StreamFinished^ finishedCallback);
	};
}