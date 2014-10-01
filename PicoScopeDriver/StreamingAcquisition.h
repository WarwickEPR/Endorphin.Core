#pragma once
#include "PicoScopeDriver.h"

using namespace System;
using namespace System::Collections::Concurrent;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;
using namespace System::Threading;
using namespace System::Reactive;

namespace PicoScopeDriver {

	public delegate void StreamDataReady(unsigned int startIndex, int numberOfSamples, List<Channel>^ overflows, short triggeredAt);
	public delegate void StreamFinished(bool didAutoStop);
	private delegate void PicoScopeStreamReady(short handle, int numberOfSamples, unsigned int startIndex, short overflow,
		unsigned int triggerAt, short triggered, short autoStop, void* state);


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

		GCHandle^ _picoScopeCallbackHandle;
		void* _picoScopeCallback;

		bool _stopStreaming = false;
		bool _didAutoStop = false;
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
		void Stop();
		void ViewData(unsigned int numberOfSamples, unsigned int startIndex, Downsampling downsamplingModes, unsigned int downsamplingRatio,
			StreamDataReady^ dataCallback);
	};
}