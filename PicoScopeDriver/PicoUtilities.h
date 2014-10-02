#pragma once

using namespace System;

namespace PicoScopeDriver {

	/// <summary>
	/// An enumeration representing a PicoScope 5000 device channel.
	/// </summary>
	public enum class Channel : int {
		A,
		B,
		C,
		D,
		External,
		Aux,
		None,
	};

	/// <summary>
	/// An enumeration representing the input voltage range of a PicoScope 5000 device channel.
	/// </summary>
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

	/// <summary>
	/// An enumeration representing the vertial ADC resolution of a PicoScope 5000 device.
	/// </summary>
	public enum class Resolution : int {
		_8bit,
		_12bit,
		_14bit,
		_15bit,
		_16bit,
	};
	
	/// <sumary>
	/// An enumeration representing units of time which can be used when providing a sample interval value.
	/// </summary>
	public enum class TimeUnit : int {
		Femtoseconds,
		Picoseconds,
		Nanoseconds,
		Microseconds,
		Milliseconds,
		Seconds,
	};
	
	/// <summary>
	/// An enumeration representing the signal available signal generator waveforms on a PicoScope 5000 device.
	/// </summary>
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

	/// <summary>
	/// An enumeration representing the available signal generator sweep directions on a PicoScope 5000 device.
	/// </summary>
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

	/// <summary>
	/// An enumeration representing the signal direction as it crosses the trigger threshold in order to cause
	/// the device trigger to fire.
	/// </summary>
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

	/// <summary>
	/// An enumeration representing the coupling of a PicoScope 5000 device channel. With AC mode, signals are a
	/// capacitively coupled to filter out frequencies below approximately 1 Hz.
	/// </summary>
	public enum class Coupling : int {
		DC,
		AC
	};

	/// <summary>
	/// An enumeration representing the hardware downsampling to be applied to data stored in PicoScope 5000 device
	/// before it is read into data buffers. If multiple downsampling modes will be applied to the same data in a
	/// given acquisition, then the downsampling modes need to be combined together using a logical "OR" operation.
	/// </summary>
	public enum class Downsampling : int {
		None = 0,
		Aggregate = 1,
		Decimated = 2,
		Averaged = 4,
		Distribution = 8
	};

	/// <summary>
	/// An enumeration representing the bandwidth limit of a PicoScope 5000 device channel.
	/// </summary>
	public enum class BandwidthLimit : int {
		Full = 0,
		_20MHz = 1
	};
}	
