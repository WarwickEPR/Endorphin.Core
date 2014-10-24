#pragma once
#include "ps5000aApi.h"
#include "PicoScopeDriver.h"

using namespace System;
using namespace System::Collections::Concurrent;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;

namespace PicoScopeDriver {

	/// <summary>
	/// Delegate representing a callback function which will be called when new data is written by the driver into the data buffers.
	/// It is important to keep processing to a minimum in the callback as it will otherwise hold up the driver thread and cause
	/// acquisition to fail.
	/// </summary>
	public delegate void StreamDataReady(unsigned int startIndex, int numberOfSamples, List<Channel>^ overflows, short triggeredAt);
	
	/// <summary>
	/// Delegate representing a callback funcition which will be called when the stream is stopped either manually or automatically.
	/// At this stage, it is safe to create new data buffers and request new views of the data stored in the device memory.
	/// </summary>
	public delegate void StreamFinished(bool didAutoStop);

	/// <summary>
	/// Delegate representing a callback function which will be called by the underlying C driver when new data is written to the
	/// buffer. This is used internally by the C++ CLI managed wrapper.
	/// </summary>
	private delegate void PicoScopeStreamReady(short handle, int numberOfSamples, unsigned int startIndex, short overflow,
		unsigned int triggerAt, short triggered, short autoStop, void* state);

	/// <summary>
	/// This class represents a streaming acquisition which is immediately started when an instance is created. The object invokes
	/// the provided callback delegates to notify the user code when new data is written to the data buffers and when the stream
	/// is stopped manaually or automatically.
	/// </summary>
	public ref class StreamingAcquisition {
	private:
		ref struct ViewCallbackData {
			GCHandle^ PicoScopeCallbackHandle;
			GCHandle^ ViewCallbackDataHandle;
			StreamDataReady^ DataCallback;
		};

		short _handle;
		StreamDataReady^ _dataCallback;
		StreamFinished^ _finishedCallback;
		Timer^ _timer;

		PicoScopeStreamReady^ _picoScopeManagedCallback;
		ps5000aStreamingReady _picoScopeCallback;

		bool _stopStreaming = false;
		bool _didAutoStop = false;
		bool _hasStopped = false;;
		Object^ _streamingLock = gcnew Object();

		void GetValues(Object^ state);

	internal:
		StreamingAcquisition(short handle, unsigned int% sampleInterval, TimeUnit timeUnit, unsigned int maxPreTriggerSamples, unsigned int maxPostTriggerSamples,
			bool autoStop, Downsampling downsamplingModes, unsigned int downsamplingRatio, unsigned int bufferSize, StreamDataReady^ dataCallback,
			StreamFinished^ finishedCallback);
		void PicoScopeStreamCallback(short handle, int numberOfSamples, unsigned int startIndex, short overflow,
			unsigned int triggerAt, short triggered, short autoStop, void* state);
		void PicoScopeViewCallback(short handle, int numberOfSamples, unsigned int startIndex, short overflow,
			unsigned int triggerAt, short triggered, short autoStop, void* state);

	public:
		/// <summary>
		/// Causes the streaming acquisition to stop at the next timer tick (performed every 100ms), before further stream data callbacks
		/// are requested from the driver.
		/// </summary>
		void Stop();

		/// <summary>
		/// Reads a new data view from the samples stored in the device memory into the previously created data buffers (using the
		/// <see cref="PicoScopeDriver.PicoScope5000"> object) after streaming has stopped. If this method is called before the
		/// <see cref="PicoScopeDriver.StreamFinished"> callback fires, an exception will be thrown.
		/// </summary>
		/// <param name="numberOfSamples">The number of samples to read into the data buffer.</param>
		/// <param name="startIndex">The number of sample intervals from the start of the data buffer to the start point of the
		/// data collection.</param>		
		/// <param name="downsamplingModes">The downsampling modes to be used during the streaming, combined together with
		/// a logical "OR" operation.</param>
		/// <param name="downsamplingRatio">The downsampling ratio to be applied to the continuously streamed data as it is
		/// transferred into the data buffers.</param>
		/// <param name="dataCallback">A callback delegate which will be called once the data has been read into the buffer.</param>
		void ViewData(unsigned int numberOfSamples, unsigned int startIndex, Downsampling downsamplingModes, unsigned int downsamplingRatio,
			StreamDataReady^ dataCallback);
	};
}