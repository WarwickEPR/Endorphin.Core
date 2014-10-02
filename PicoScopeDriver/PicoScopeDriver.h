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

	/// <summary>
	/// This class enables communication with a connected PicoScope 5000 series oscilloscope. Each class
	/// instance represents a single physical device. The class implements <see cref="System.IDisposable" />
	/// and with the device is closed once the Dispose method is called.
	/// </summary>
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

		/// <summary>
		/// This function returns an array of device serial numbers for the connected PicoScope 5000 units.
		/// </summary>
		static array<String^>^ GetConnectedUnitSerials();

		/// <summary>
		/// Converts a <see cref="PicoScopeDriver.Range" /> enumeration to a floating point value in millivolts.
		/// </summary>
		static float RangeInMillivolts(Range range);

		// Constructors / destructor

		/// <summary>
		/// Creates a new PicoScope5000 object with the default (8-bit) vertial resolution, opening a
		/// connection to the first found device.
		/// </summary>
		PicoScope5000() : PicoScope5000(Resolution::_8bit) {};

		/// <summary>
		/// Creates a new PicoScope5000 object with the specified vertial resolution, opening a connection
		/// to the first found device.
		/// </summary>
		/// <param name="resolution">The required vertial resolution when the device is opened.</param>
		PicoScope5000(Resolution resolution);

		/// <summary>
		/// Creates a new PicoScope5000 object with the default (8-bit) vertial resolution, opening a
		/// connection to the device with the specified serial string.
		/// </summary>
		/// <param name="serial">The serial number of the required device.</param>
		PicoScope5000(String^ serial) : PicoScope5000(serial, Resolution::_8bit) {};

		/// <summary>
		/// Creates a new PicoScope5000 object with the specified vertial resolution, opening a connection
		/// to the device with the specified serial string.
		/// </summary>
		/// <param name="serial">The serial number of the required device.</param>
		/// <param name="resolution">The required vertial resolution when the device is opened.</param>
		PicoScope5000(String^ serial, Resolution resolution);

		/// <summary>
		/// Closes communication with the device.
		/// </summary>
		~PicoScope5000();

		// Device information

		/// <summary>
		/// Gets the driver version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitDriverVersion()				{ return GetUnitInfoValue(_handle, PicoInfo::DriverVersion); }

		/// <summary>
		/// Gets the USB version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitUsbVersion()					{ return GetUnitInfoValue(_handle, PicoInfo::UsbVersion); }
		
		/// <summary>
		/// Gets the device hardware version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitHardwareVersion()			{ return GetUnitInfoValue(_handle, PicoInfo::HardwareVersion); }
		
		/// <summary>
		/// Gets the device variant string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitVariantInfo()				{ return GetUnitInfoValue(_handle, PicoInfo::VariantInfo); }
		
		/// <summary>
		/// Gets the serial number string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitSerial()						{ return GetUnitInfoValue(_handle, PicoInfo::BatchAndSerial); }
		
		/// <summary>
		/// Gets the carlibration date string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitCalibrationDate()			{ return GetUnitInfoValue(_handle, PicoInfo::CalibrationDate); }

		/// <summary>
		/// Gets the device kernel version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitKernelVersion()				{ return GetUnitInfoValue(_handle, PicoInfo::KernelVersion); }

		/// <summary>
		/// Gets the device digital hardware version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitDigitalHardwareVersion()		{ return GetUnitInfoValue(_handle, PicoInfo::DigitalHardwareVersion); }
		
		/// <summary>
		/// Gets the device analogue hardware version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitAnalogueHardwareVersion()	{ return GetUnitInfoValue(_handle, PicoInfo::AnalogueHardwareVersion); }
		
		/// <summary>
		/// Gets the first device firmware version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitFirmwareVersion1()			{ return GetUnitInfoValue(_handle, PicoInfo::FirmwareVersion1); }
		
		/// <summary>
		/// Gets the second device firmware version string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitFirmwareVersion2()			{ return GetUnitInfoValue(_handle, PicoInfo::FirmwareVersion2); }
		
		/// <summary>
		/// Gets the MAC address string for the connected PicoScope 5000.
		/// </summary>
		String^ GetUnitMacAddress()					{ return GetUnitInfoValue(_handle, PicoInfo::MacAddress); }
		
		/// <summary>
		/// Gets dictionary containing all information strings about the connected PicoScope 5000.
		/// </summary>
		Dictionary<PicoInfo, String^>^ GetUnitInfo();

		// Device status

		/// <summary>
		/// Gets or sets whether the unit is mains-powered.
		/// </summary>
		property bool UnitIsMainsPowered { bool get(); void set(bool mainsPower); }

		/// <summary>
		/// Causes the device LED to flash.
		/// </summary>
		/// <param name="count">
		/// If count &gt; 0 then the LED flashes the specified number of times.
		/// If count = 0 then the LED stops flashing.
		/// If count &lt; 0 then the LED flashes indefinitely (until stopped).
		/// </param>
		void FlashLed(short count);

		/// <summary>
		/// Pings the device to confirm that it is still connected via USB.
		/// </summary>
		void Ping();

		// Timebases
		
		/// <summary>
		/// Gets the time interval between samples in nanoseconds, corresponding to the specified timebase.
		/// Note that the output depends on the current device resolution. The maximum number of samples
		/// which can be stored in the current device memory segment is returned via an out parameter. Note
		/// that this storage is shared across all recorded channels so the number needs to be divided by
		/// the number of enabled channels.
		/// </summary>
		/// <param name="timebase">The timebase.</param>
		/// <param name="maximumNumberOfSamlpes">The maximum number of samples which can be stored in the
		/// current memory segment. This number is shared across all acquired channels so divide by the
		/// number of enabled channels to get the number of samples per channel.</param>
		float GetTimebaseIntervalInNanoseconds(unsigned int timebase, [Out] int% maximumNumberOfSamples)
		{ return GetTimebaseIntervalInNanoseconds(timebase, CurrentSegmentIndex, maximumNumberOfSamples); }

		/// <summary>
		/// Gets the time interval between samples in nanoseconds, corresponding to the specified timebase.
		/// Note that the output depends on the current device resolution. The maximum number of samples
		/// which can be stored in the specified device memory segment is returned via an out parameter. Note
		/// that this storage is shared across all recorded channels so the number needs to be divided by
		/// the number of enabled channels.
		/// </summary>
		/// <param name="timebase">The timebase.</param>
		/// <param name="segmentIndex">The index of the memory segment to be used for storage.</param>
		/// <param name="maximumNumberOfSamlpes">The maximum number of samples which can be stored in the
		/// specified memory segment. This number is shared across all acquired channels so divide by the
		/// number of enabled channels to get the number of samples per channel.</param>
		float GetTimebaseIntervalInNanoseconds(unsigned int timebase, unsigned int segmentIndex, [Out] int% maximumNumberOfSamples);
		
		/// <summary>
		/// Gets the fastest available timebase for the current device resolution.
		/// </summary>
		unsigned int GetFastestTimebase() { return GetFastestTimebase(DeviceResolution); }
		
		/// <summary>
		/// Gets the fastest available timebase for the specified device resolution.
		/// </summary>
		/// <param name="resolution">The device resolution.</summary>
		unsigned int GetFastestTimebase(Resolution resolution);

		/// <summary>
		/// Gets the fastest streaming sample interval available in nanoseconds, for a specified number of 
		/// channels at the current device resolution.
		/// </summary>
		/// <param name="channelCount">The number of channels to be acquired.</param>
		unsigned int GetFastestStreamingIntervalInNanoseconds(int channelCount)
		{ return GetFastestStreamingIntervalInNanoseconds(DeviceResolution, channelCount); }

		/// <summary>
		/// Gets the fastest streaming sample interval available in nanoseconds, for a specified number of
		/// channels and device resolution.
		/// <summary>
		/// <param name="resolution">The device resolution.</param>
		/// <param name="channelCount">The number of channels to be acquired.</param>
		unsigned int GetFastestStreamingIntervalInNanoseconds(Resolution resolution, int channelCount);

		// Channel setup

		/// <summary>
		/// Gets or sets the vertial resolution of the device.
		/// </summary>
		property Resolution DeviceResolution { Resolution get(); void set(Resolution resolution); }

		/// <summary>
		/// Gets the analogue offset limits for a specified voltage range and channel coupling (AC/DC).
		/// </summary>
		/// <param name="range">The voltage range of the channel.</param>
		/// <param name="coupling">The coupling (AC/DC) of the channel.</param>
		/// <param name="maxVoltage">Out parameter giving the maximum analogue offset voltage in volts.</param>
		/// <param name="minVoltage">Out parameter giving the minimum analogue offset voltage in volts.</param>
		void GetAnalogueOffsetLimits(Range range, Coupling coupling, [Out] float% maxVoltage, [Out] float% minVoltage);

		/// <summary>
		/// Gets the available channel voltage ranges for a specified device channel.
		/// </summary>
		/// <param name="channel">The device channel.</param>
		array<Range>^ GetAvailableChannelRanges(Channel channel);

		/// <summary>
		/// Sets parameters for a specified channel.
		/// </summary>
		/// <param name="channel">The channel to which the parameters will be set.</param>
		/// <param name="enabled">A boolean indicating whether the channel should be enabled.</param>
		/// <param name="range">The voltage range to set to for channel.</param>
		/// <param name="analogueOffset">The analogue offset of the channel in volts.</param>
		void SetChannel(Channel channel, bool enabled, Coupling coupling, Range range, float analogueOffset);

		/// <summary>
		/// Sets channel bandwidth for a specified channel.
		/// </summary>
		/// <param name="channel">The channel for which to set the bandwidth limit.</param>
		/// <param name="bandwidth">The required bandwidth limit.</param>
		void SetBandwidth(Channel channel, BandwidthLimit bandwidth);

		/// <summary>
		/// Disables a channel.
		/// </summary>
		/// <param name="channel">The channel to disable.</param>
		void DisableChannel(Channel channel);

		/// <summary>
		/// Gets the maximum number of channels which can be used with the current device resolution.
		/// </summary>
		int GetMaximumNumberOfChannels() { return GetMaximumNumberOfChannels(DeviceResolution); }

		/// <summary>
		/// Gets the maximum number of channels which can be used with a specified resolution.
		/// </summary>
		/// <param name="resolution">The required device resolution.</param>
		int GetMaximumNumberOfChannels(Resolution resolution);

		// Trigger setup

		/// <summary>
		/// Disables triggering.
		/// </summary>
		void DisableTrigger();

		/// <summary>
		/// Sets the device to trigger automatically after a specified delay.
		/// </summary>
		/// <param name="delayInMilliseconds">The trigger delay in milliseconds.</param>
		void SetAutoTrigger(short delayInMilliseconds);

		/// <summary>
		/// Sets a simple threshold trigger on a particular channel without any automatic triggering.
		/// </summary>
		/// <param name="source">The trigger channel.</param>
		/// <param name="threshold">The ADC count at which trigger will fire.</param>
		/// <param name="direction">The direction in which the signal must vary to cause the trigger to fire.</param>
		/// <param name="delay">The time between the trigger occuring and the first sample. This is given in units of
		/// the current timebase interval.</param>
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay);

		/// <summary>
		/// Sets a simple threshold trigger on a particular channel.
		/// </summary>
		/// <param name="source">The trigger channel.</param>
		/// <param name="threshold">The ADC count at which trigger will fire.</param>
		/// <param name="direction">The direction in which the signal must vary to cause the trigger to fire.</param>
		/// <param name="delay">The time between the trigger occuring and the first sample. This is given in units of
		/// the current timebase interval.</param>
		/// <param name="autoDelayInMilliseconds">The number of milliseconds before the trigger is automatically
		/// fired if the threshold is not crossed. The device will wait indefinitely if this is set to zero.</param>
		void SetSimpleTrigger(Channel source, short threshold, ThresholdDirection direction, unsigned long delay, short autoDelayInMilliseconds);
		
		/// <summary>
		/// Sets the delay between a trigger event occuring and the first sample being acquired.
		/// </summary>
		/// <param name="delay">The time between the trigger occuring and the first sample. This is given in units of
		/// the current timebase interval.</param>
		void SetTriggerDelay(unsigned long delay);

		/// <summary>
		/// Checks if triggering is enabled.
		/// </summary>
		bool IsTriggerEnabled();

		/// <summary>
		/// Checks if a pulse-width qualifier is enabled. This is an advanced trigger type. See device documentation.
		/// </summary>
		bool IsPulseWidthQualifierEnabled();
		
		/// <summary>
		/// Checks whether any trigger or pulse-width qualifier is enabled.
		/// </summary>
		/// <param name="triggerEnabled">Out parameter indicating whether any threshold triggers are enabled.</param>
		/// <param name="pulseWidthQualifierEnabled">Out parameter indicating whether any pulse-width qualifier
		/// triggers are enabled.</param>
		void IsTriggerOrPulseWidthQualifierEnabled([Out] bool% triggerEnabled, [Out] bool% pulseWidthQualifierEnabled);

		// Buffer setup and reaodut
		
		/// <summary>
		/// Gets or sets the current device memory segment index to be used for acquisition.
		/// </summary>
		property unsigned int CurrentSegmentIndex { unsigned int get(); void set(unsigned int); }
		
		/// <summary>
		/// Sets the number of memory segments on the device and returns the number of samples which can be stored
		/// in each segment. These samples are shared across all acquired channels so divide by the number of enabled
		/// channels to get number of samples per channel.
		/// </summary>
		/// <param name="numberOfSegments">The number of memory segments into which to divide the device memory</param>
		int SetNumberOfSegmentsAndGetSamplesPerSegment(unsigned int numberOfSegments);

		/// <summary>
		/// Gets the maximum number of segments into which the device memory can be divided.
		/// </summary>
		unsigned int GetMaximumNumberOfSegments();

		/// <summary>
		/// Gets the minimum number of ADC counts for samples at the current device resolution.
		/// </summary>
		short GetMinimumAdcCountsForCurrentResolution();
		
		/// <summary>
		/// Gets the maximum number of ADC counts for samples at the current device resolution.
		/// </summary>
		short GetMaximumAdcCountsForCurrentResolution();

		/// <summary>
		/// Gets the maximum downsampling ratio which can be achieved in hardware.
		/// </summary>
		/// <param name="numberOfUnaggregatedSamples">The number of samples to be reduced, prior to downsampling.</param>
		/// <param name="downsampling">The type of downsampling to be used.</param>
		/// <param name="segmentIndex">The index of the device memory segment to be downsampled.</param>
		unsigned int GetMaximumDownsamplingRatio(unsigned long numberOfUnaggregatedSamples, Downsampling downsampling, unsigned long segmentIndex);

		/// <summary>
		/// Gets the maximum downsampling ratio which can be achieved in hardware for the current device memory segment.
		/// </summary>
		/// <param name="numberOfUnaggregatedSamples">The number of samples to be reduced, prior to downsampling.</param>
		/// <param name="downsampling">The type of downsampling to be used.</param>
		unsigned int GetMaximumDownsamplingRatio(unsigned long numberOfUnaggregatedSamples, Downsampling downsampling)
		{ return GetMaximumDownsamplingRatio(numberOfUnaggregatedSamples, downsampling, CurrentSegmentIndex); }

		/// <summary>
		/// Creates a buffer on the unmanaged heap for the specified channel and device memory segment with specified size
		/// and downsampling. An <see cref="PicoScopeDriver.IInt16BufferHandle" /> is returned which implements the
		/// <see cref="System.IDisposable" /> interface. The user code needs to call the Dispose method on this buffer
		/// handle to ensure that the allocated memory is released or a memory leak will occur. The preferred way to do this
		/// is to place the buffer object in a "using block". An unmanaged buffer is preferred to a pinned buffer for streaming 
		/// acquisitions which can continue of long periods of time as it is allocated on the unmanaged heap and will not 
		/// limit the garbage collector in defragmenting the managed heap for its lifespan.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		/// <param name="segmentIndex">The index of the device memory segment to be transferred into the buffer.</param>
		IInt16BufferHandle^ CreateUnmanagedBuffer(Channel channel, Downsampling downsampling, int bufferLength, unsigned int segmentIndex);

		/// <summary>
		/// Creates a buffer on the unmanaged heap for the specified channel and the current device memory segment with 
		/// specified size and downsampling. An <see cref="PicoScopeDriver.IInt16BufferHandle" /> is returned which implements 
		/// the <see cref="System.IDisposable" /> interface. The user code needs to call the Dispose method on this buffer
		/// handle to ensure that the allocated memory is released or a memory leak will occur. The preferred way to do this
		/// is to place the buffer object in a "using block". An unmanaged buffer is preferred to a pinned buffer for streaming 
		/// acquisitions which can continue of long periods of time as it is allocated on the unmanaged heap and will not 
		/// limit the garbage collector in defragmenting the managed heap for its lifespan.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		IInt16BufferHandle^ CreateUnmanagedBuffer(Channel channel, Downsampling downsampling, int bufferLength)
		{ return CreateUnmanagedBuffer(channel, downsampling, bufferLength, CurrentSegmentIndex); }

		/// <summary>
		/// Creates a pinned buffer using a provided managed array for the specified channel and device memory segment with
		/// specifed downsampling. An <see cref="PicoScopeDriver.IInt16BufferHandle" /> is returned which implements the
		/// <see cref="System.IDisposable" /> interface. The user code needs to call the Dispose method on this buffer
		/// handle to ensure that the garbage collector can release the memory or a memory leak will occur. The preferred way
		/// to do this is to place the buffer object in a "using block". A pinned buffer is preferred to an unmanaged buffer
		/// for block acquisitions which have a relatively short duration compared to streaming acquisitions. Though the
		/// pinning will prevent the garbage collector from defragmenting a large part of the managed heap memory during
		/// until the handle is disposed, this way, the data is read straight into managed memory and does not have to be
		/// copied across from the unmanaged heap to be used in the rest of the application.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		/// <param name="segmentIndex">The index of the device memory segment to be transferred into the buffer.</param>
		IInt16BufferHandle^ CreatePinnedBuffer(Channel channel, Downsampling downsampling, array<Int16>^ buffer, unsigned int segmentIndex);

		/// <summary>
		/// Creates a pinned buffer using a provided managed array for the specified channel and the current device memory 
		/// segment with specifed downsampling. An <see cref="PicoScopeDriver.IInt16BufferHandle" /> is returned which 
		/// implements the <see cref="System.IDisposable" /> interface. The user code needs to call the Dispose method on this 
		/// buffer handle to ensure that the garbage collector can release the memory or a memory leak will occur. The 
		/// preferred way to do this is to place the buffer object in a "using block". A pinned buffer is preferred to an 
		/// unmanaged buffer for block acquisitions which have a relatively short duration compared to streaming acquisitions.
		/// Though the pinning will prevent the garbage collector from defragmenting a large part of the managed heap memory 
		/// during until the handle is disposed, this way, the data is read straight into managed memory and does not have to 
		/// be copied across from the unmanaged heap to be used in the rest of the application.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		IInt16BufferHandle^ CreatePinnedBuffer(Channel channel, Downsampling downsampling, array<Int16>^ buffer)
		{ return CreatePinnedBuffer(channel, downsampling, buffer, CurrentSegmentIndex); }

		/// <summary>
		/// Creates a pair of buffers on the unmanaged heap for the specified channel and device memory segment with specified 
		/// size, for aggregate downsampling. The A <see cref="System.Tuple" /> of two <see cref="PicoScopeDriver.IInt16BufferHandle"/> 
		/// objects is returned which implement the <see cref="System.IDisposable"/> interface. The user code needs to call the Dispose
		/// method on both of these buffer handles to ensure that the allocated memory is released or a memory leak will occur. 
		/// The preferred way to do this is to place the buffer objects in a "using block". An unmanaged buffer is preferred to 
		/// a pinned buffer for streaming acquisitions which can continue of long periods of time as it is allocated on the 
		/// unmanaged heap and will not limit the garbage collector in defragmenting the managed heap for its lifespan.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		/// <param name="segmentIndex">The index of the device memory segment to be transferred into the buffer.</param>
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreateUnmanagedBuffers(Channel channel, int bufferLength, unsigned int segmentIndex);

		/// <summary>
		/// Creates a pair of buffers on the unmanaged heap for the specified channel and the current device memory segment with 
		/// specified size for aggregate downsampling. A <see cref="System.Tuple" /> of two 
		/// <see cref="PicoScopeDriver.IInt16BufferHandle"/> objects is returned which implement the <see cref="System.IDisposable"/>
		/// interface. The user code needs to call the Dispose method on both of these buffer handles to ensure that the allocated 
		/// memory is released or a memory leak will occur.  The preferred way to do this is to place the buffer objects in a "using
		/// block". An unmanaged buffer is preferred to a pinned buffer for streaming acquisitions which can continue of long periods
		/// of time as it is allocated on the unmanaged heap and will not limit the garbage collector in defragmenting the managed heap 
		/// for its lifespan.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="downsampling">The type of downsampling to be used in transferring the data.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreateUnmanagedBuffers(Channel channel, int bufferLength)
		{ return CreateUnmanagedBuffers(channel, bufferLength, CurrentSegmentIndex); }

		/// <summary>
		/// Creates a pair of pinned buffers the specified channel and device memory segment with specified size for aggregate 
		/// downsampling. A <see cref="System.Tuple" /> of two <see cref="PicoScopeDriver.IInt16BufferHandle"/> is returned which 
		/// implement the <see cref="System.IDisposable"/> interface. The user code needs to call the Dispose method on both of
		/// these buffer handles to ensure that the allocated memory is released or a memory leak will occur. The preferred way 
		/// to do this is to place the buffer objects in a "using block". A pinned buffer is preferred to an unmanaged buffer 
		/// for block acquisitions which have a relatively short duration compared to streaming acquisitions. Though the pinning 
		/// will prevent the garbage collector from defragmenting a large part of the managed heap memory during until the handle 
		/// is disposed, this way, the data is read straight into managed memory and does not have to be copied across from the 
		/// unmanaged heap to be used in the rest of the application.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		/// <param name="segmentIndex">The index of the device memory segment to be transferred into the buffer.</param>
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreatePinnedBuffers(Channel channel, array<Int16>^ bufferMax,
			array<Int16>^ bufferMin, unsigned int segmentIndex);

		/// <summary>
		/// Creates a pair of pinned buffers the specified channel and the current device memory segment with specified size for 
		/// aggregate  downsampling. A <see cref="System.Tuple" /> of two <see cref="PicoScopeDriver.IInt16BufferHandle"/> is 
		/// returned which implement the <see cref="System.IDisposable"/> interface. The user code needs to call the Dispose method 
		/// on both of these buffer handles to ensure that the allocated memory is released or a memory leak will occur. The 
		/// preferred way to do this is to place the buffer objects in a "using block". A pinned buffer is preferred to an unmanaged
		/// buffer for block acquisitions which have a relatively short duration compared to streaming acquisitions. Though the
		/// pinning will prevent the garbage collector from defragmenting a large part of the managed heap memory during until the
		/// handle  is disposed, this way, the data is read straight into managed memory and does not have to be copied across from
		/// the unmanaged heap to be used in the rest of the application.
		/// </summary>
		/// <param name="channel">The device channel which will be transferred from memory into the buffer.</param>
		/// <param name="bufferLength">The length of the buffer.</param>
		Tuple<IInt16BufferHandle^, IInt16BufferHandle^>^ CreatePinnedBuffers(Channel channel, array<Int16>^ bufferMax,
			array<Int16>^ bufferMin) { return CreatePinnedBuffers(channel, bufferMax, bufferMin, CurrentSegmentIndex); }

		// Streaming acquisition

		/// <summary>
		/// Begins a streaming acquisition with the specified sample rate and pre- and post-trigger samples. The user can
		/// specify callback functions which are called when new data has been written to the previously created buffers and
		/// when the streaming acquisition stops. A downsampling ratio can be specified for the streaming data. The method
		/// returns a <see cref="PicoScopeDriver.StreamingAcquisition" /> object which can be used to stop the stream and 
		/// create new views on the data once it has finished. Note that the zero-index memory segment is always used for
		/// streaming acquisition.
		/// </summary>
		/// <param name="timeInterval">A reference parameter which represents the requested sample interval on entry. On exit,
		/// this is the actual sample interval which will be produced by the device. This is to ensure that the sample rate is
		/// is an integer fraction of the device clock rate.</param>
		/// <param name="timeUnit">The time unit for the sample interval.</param>
		/// <param name="maxPreTriggerSamples">The maximum number of pre-trigger samples to be recorded.</param>
		/// <param name="maxPostTriggerSamples">The maximum number of post-trigger samples to be recorded.</param>
		/// <param name="autoStop">A boolean indicating whether the device should stop recording automatically when the
		/// maximum number of (pre- and post-trigger) samples has been recorded.</param>
		/// <param name="downsamplingModes">The downsampling modes to be used during the streaming, combined together with
		/// a logical "OR" operation.</param>
		/// <param name="downsamplingRatio">The downsampling ratio to be applied to the continuously streamed data as it is
		/// transferred into the data buffers.</param>
		/// <param name="bufferSize">The size of the data buffers.</param>
		/// <param name="dataCallback">A callback delegate which will be called when new data has been written by the driver
		/// into the buffer. It is important to keep the processing in this callback to a minimum as it will otherwise hold up
		/// the driver thread and cause the acquisition to stop.</param>
		/// <param name="finishedCallback">A callback delegate which will be called once the stream has been stopped, either
		/// manually or automatically. It is then safe to create new views on the data which is still in the device memory</param>
		StreamingAcquisition^ RunStreaming(unsigned int% timeInterval, TimeUnit timeUnit, unsigned int maxPreTriggerSamples,
			unsigned int maxPostTriggerSamples, bool autoStop, Downsampling downsamplingModes, unsigned int downsamplingRatio,
			unsigned int bufferSize, StreamDataReady^ dataCallback, StreamFinished^ finishedCallback);
	};
}