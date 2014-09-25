#pragma once

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

	public enum class DownSamplingMode : int {
		None,
		Aggregate
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

	public enum class RatioMode : int {
		None = 0,
		Aggregate = 1,
		Decimate = 2,
		Average = 4,
		Distribution = 8
	};

	public enum class BandwidthLimit : int {
		Full = 0,
		_20MHz = 1
	};

	public ref struct ChannelSettings {
		bool enabled = true;
		Coupling coupling = Coupling::DC;
		Range range = Range::_5V;
		float analogueOffset = 0.0;

		static ChannelSettings^ DefaultEnabled() {
			return gcnew ChannelSettings();
		}

		static ChannelSettings^ DefaultDisabled() {
			auto channelSettings = gcnew ChannelSettings();
			channelSettings->enabled = false;
			return channelSettings;
		}
	};
}	
