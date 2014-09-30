#pragma once

using namespace System;

namespace PicoScopeDriver {
	public enum class Channel : int {
		A,
		B,
		C,
		D,
		External,
		Aux,
		None,
	};

	public enum class Range : int {
		_10mV,
		_20mV,
		_50mV,
		_100mV,
		_200mV,
		_500mV,
		_1V,
		_2V,
		_5V,
		_10V,
		_20V,
		_50V,
	};

	float RangeInMillivolts(Range range) {
		switch (range) {
		case Range::_10mV: return 10.0;
		case Range::_20mV: return 20.0;
		case Range::_50mV: return 50.0;
		case Range::_100mV: return 100.0;
		case Range::_200mV: return 200.0;
		case Range::_500mV: return 500.0;
		case Range::_1V: return 1000.0;
		case Range::_2V: return 2000.0;
		case Range::_5V: return 5000.0;
		case Range::_10V: return 1.0e4;
		case Range::_20V: return 2.0e4;
		case Range::_50V: return 5.0e4;
		}
		throw gcnew Exception("Unexpected range.");
	}

	public enum class Resolution : int {
		_8bit,
		_12bit,
		_14bit,
		_15bit,
		_16bit,
	};

	public enum class TimeUnit : int {
		Femtoseconds,
		Picoseconds,
		Nanoseconds,
		Microseconds,
		Milliseconds,
		Seconds,
	};

	public enum class SignalGeneratorWaveform : int {
		Sine,
		Square,
		Triangle,
		RampUp,
		RampDown,
		Sinc,
		Gaussian,
		HalfSine,
		DcVoltage,
		MAX_WAVE_TYPES
	};

	public enum class SignalGeneratorSweep : int {
		Up,
		Down,
		UpDown,
		DownUp
	};

	public enum class SignalGeneratorTrigger : int {
		Rising,
		Falling,
		GateHigh,
		GateLow
	};

	public enum class SignalGeneratorTriggerSource : int {
		None,
		ScopeTrigger,
		AuxIn,
		ExtIn,
		SoftwareTrigger
	};

	public enum class ThresholdDirection : int {
		// Values for level threshold mode
		//
		Above,
		Below,
		Rising,
		Falling,
		RisingOrFalling,

		// Values for window threshold mode
		//
		// Inside = Above,
		// Outside = Below,
		// Enter = Rising,
		// Exit = Falling,
		// EnterOrExit = RisingOrFalling,

		None = Rising,
	};

	public enum class PulseWidthType : int {
		None,
		LessThan,
		GreaterThan,
		InRange,
		OutOfRange
	};

	public enum class Coupling : int {
		DC,
		AC
	};

	public enum class Downsampling : int {
		None = 0,
		Aggregate = 1,
		Decimated = 2,
		Averaged = 4,
		Distribution = 8
	};

	public ref struct ChannelView {
		Channel channel;
		Downsampling mode;
	};

	public enum class BandwidthLimit : int {
		Full = 0,
		_20MHz = 1
	};
}	
