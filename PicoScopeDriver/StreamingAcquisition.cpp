#include "Stdafx.h"
#include "PicoScopeDriver.h"
#include "ps5000aApi.h"
#include <string>

#pragma managed

using namespace PicoScopeDriver;
using namespace Runtime::InteropServices;

StreamingAcquisition::StreamingAcquisition(short handle, unsigned int% sampleInterval, TimeUnit timeUnit, unsigned int maxPreTriggerSamples, unsigned int maxPostTriggerSamples,
	bool autoStop, Downsampling downsamplingModes, unsigned int downsamplingRatio, unsigned int bufferSize, StreamDataReady^ dataCallback, StreamFinished^ finishedCallback) :
	_handle(handle), _dataCallback(dataCallback), _finishedCallback(finishedCallback) {
	
	_picoScopeManagedCallback = gcnew PicoScopeStreamReady(this, &StreamingAcquisition::PicoScopeStreamCallback);
	auto fp = Marshal::GetFunctionPointerForDelegate(_picoScopeManagedCallback);
	_picoScopeCallback = reinterpret_cast<ps5000aStreamingReady>(fp.ToPointer());

	pin_ptr<unsigned int> sampleIntervalPtr = &sampleInterval;
	auto status = (PicoStatus) ps5000aRunStreaming(_handle, sampleIntervalPtr, (PS5000A_TIME_UNITS)timeUnit, maxPreTriggerSamples, maxPostTriggerSamples, autoStop ? 1 : 0,
		downsamplingRatio, (PS5000A_RATIO_MODE)downsamplingModes, bufferSize);
	PicoScope5000::CheckStatus(status);

	_timer = gcnew Timer(gcnew TimerCallback(this, &StreamingAcquisition::GetValues), nullptr, TimeSpan::FromMilliseconds(100.0), Timeout::InfiniteTimeSpan);
}

void StreamingAcquisition::GetValues(Object^ state) {
	Monitor::Enter(_streamingLock);
	bool stopStreaming = _stopStreaming;
	Monitor::Exit(_streamingLock);
	
	if (!stopStreaming) {
		auto status = (PicoStatus)ps5000aGetStreamingLatestValues(_handle, _picoScopeCallback, (void*)IntPtr::Zero);
		PicoScope5000::CheckStatus(status);
		_timer->Change(TimeSpan::FromMilliseconds(100.0), Timeout::InfiniteTimeSpan);
	}
	else {
		auto status = (PicoStatus)ps5000aStop(_handle);
		PicoScope5000::CheckStatus(status);

		Monitor::Enter(_streamingLock);
		_hasStopped = true;
		Monitor::Exit(_streamingLock);

		if (_finishedCallback != nullptr) {
			_finishedCallback->Invoke(_didAutoStop);
		}
	}
}


void StreamingAcquisition::PicoScopeStreamCallback(short handle, int numberOfSamples, unsigned int startIndex, 
	short overflow, unsigned int triggerAt, short triggered, short autoStop, void* state) {
	Console::WriteLine("Your mum!");
	if (numberOfSamples > 0) {
		if (_dataCallback != nullptr) {
			auto overflows = gcnew List<Channel>();
			short overflowBit = 1;

			for (int i = (int)Channel::A; i <= (int)Channel::D; i++)
			{
				if (overflow & overflowBit) {
					overflows->Add((Channel)i);
				}
				overflowBit = overflowBit << 1;
			}

			_dataCallback->Invoke(startIndex, numberOfSamples, overflows, triggered != 0 ? triggerAt : -1);
			_timer->Change(TimeSpan::FromMilliseconds(100.0), Timeout::InfiniteTimeSpan);
		}

		if (autoStop != 0) {
			Monitor::Enter(_streamingLock);
			_didAutoStop = true;
			_stopStreaming = true;
			Monitor::Exit(_streamingLock);
		}
	}
}

void StreamingAcquisition::PicoScopeViewCallback(short handle, int numberOfSamples, unsigned int startIndex, short overflow,
	unsigned int triggerAt, short triggered, short autoStop, void* state) {

	auto data = (ViewCallbackData^) Marshal::PtrToStructure(IntPtr(state), ViewCallbackData::typeid);
	data->PicoScopeCallbackHandle->Free();
	data->ViewCallbackDataHandle->Free();

	auto overflows = gcnew List<Channel>();
	short overflowBit = 1;

	for (int i = (int)Channel::A; i <= (int)Channel::D; i++)
	{
		if (overflow & overflowBit) {
			overflows->Add((Channel)i);
		}
		overflowBit = overflowBit << 1;
	}

	data->DataCallback->Invoke(startIndex, numberOfSamples, overflows, triggered != 0 ? triggerAt : -1);
}

void StreamingAcquisition::Stop() {
	Monitor::Enter(_streamingLock);
	_stopStreaming = true;
	Monitor::Exit(_streamingLock);
}

void StreamingAcquisition::ViewData(unsigned int numberOfSamples, unsigned int startIndex, Downsampling downsamplingModes, 
	unsigned int downsamplingRatio, StreamDataReady^ dataCallback) {
	Monitor::Enter(_streamingLock);
	if (!_hasStopped) {
		throw gcnew Exception("Attempted to create view stream data before the stream has stopped.");
	}
	Monitor::Exit(_streamingLock);

	auto callback = gcnew PicoScopeStreamReady(this, &StreamingAcquisition::PicoScopeViewCallback);
	auto picoScopeCallback = Marshal::GetFunctionPointerForDelegate(callback).ToPointer();
	auto picoScopeCallbackHandle = GCHandle::Alloc(callback);

	auto state = gcnew ViewCallbackData();
	auto stateHandle = GCHandle::Alloc(state, GCHandleType::Pinned);
	state->PicoScopeCallbackHandle = picoScopeCallbackHandle;
	state->DataCallback = dataCallback;
	state->ViewCallbackDataHandle = stateHandle;

	auto status = (PicoStatus)ps5000aGetValuesAsync(_handle, startIndex, numberOfSamples, downsamplingRatio,
		(PS5000A_RATIO_MODE)downsamplingModes, 0, picoScopeCallback, GCHandle::ToIntPtr(stateHandle).ToPointer());
}