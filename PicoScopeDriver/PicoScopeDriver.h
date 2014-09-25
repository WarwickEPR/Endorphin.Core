// PicoScopeDriver.h

#pragma once

#include "Stdafx.h"
#include "StreamingAcquisition.h"

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
		short _minimumAdcValue;
		short _maximumAdcValue;
		Dictionary<Channel, ChannelSettings^>^ _channelSettings = gcnew Dictionary<Channel, ChannelSettings^>();
		
		static String^ GetUnitInfoValue(short handle, PicoInfo info);
		bool CheckChannelSettingsForResolution(Resolution resolution);
		void SetDefaultChannelSettings(Resolution resolution);
	internal:
		static void CheckStatus(PicoStatus status);

	public:
		static array<String^>^ GetConnectedUnitSerials();

		PicoScope5000() : PicoScope5000(Resolution::_8bit) {};
		PicoScope5000(Resolution resolution);
		PicoScope5000(String^ serial) : PicoScope5000(serial, Resolution::_8bit) {};
		PicoScope5000(String^ serial, Resolution resolution);
		~PicoScope5000();

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

		bool GetUnitIsMainsPowered();
		void SetUnitIsMainsPowered(bool mainsPower);

		Resolution GetDeviceResolution();
		void SetDeviceResolution(Resolution resolution);

		void GetAnalogueOffsetLimits(Range range, Coupling coupling, [Out] float% maxVoltage, [Out] float% minVoltage);
		array<Range>^ GetAvailableChannelRanges(Channel channel);
		unsigned int GetMaximumDownsampleRatio(unsigned long numberOfUnaggregatedSegments, RatioMode ratioMode, unsigned long segmentIndex);
		unsigned int GetMaximumSegments();
		unsigned int GetNumberOfCaptures();
		unsigned int GetNumberOfProcessedCaptures();

		void FlashLed(short count);
		void Ping();

		short GetMinimumAdcValue();
		short GetMaximumAdcValue();

		float GetTimebaseIntervalInNanoseconds(unsigned int timebase, unsigned int segmentIndex, [Out] int% maximumNumberOfSamples);
		unsigned int GetMinimumSampleIntervalTimebase();
		unsigned int GetMinimumStreamingSampleIntervalTimeBase();

		void SetChannel(Channel channel, bool enabled, Coupling coupling, Range range, float analogueOffset);
		void SetBandwidth(Channel channel, BandwidthLimit bandwidth);
		void DisableChannel(Channel channel);
		int GetEnabledChannelCount();
		int GetMaximumChannelsForResolution(Resolution resolution);

		void DisableTrigger();
		void SetAutoTrigger(short delayInMilliseconds);
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay);
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay, short autoDelayInMilliseconds);
		void SetTriggerDelay(unsigned long delaySamples);
		bool IsTriggerEnabled();
		bool IsPulseWidthQualifierEnabled();
		void IsTriggerOrPulseWidthQualifierEnabled([Out] bool% triggerEnabled, [Out] bool% pulseWidthQualifierEnabled);

		StreamingAcquisition^ RunStreaming(StreamData^ dataCallback, StreamFinished^ finishedCallback);
		// BlockAcquisition^ RunBlockAcquisition(...)
		// BlockAcquisition^ RunRapidBlockAcquisition(...)
	};
}