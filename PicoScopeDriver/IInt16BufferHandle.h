using namespace System;
using namespace System::Collections::Generic;

/// <summary>
/// This interface represents a 16-bit integer buffer handle which can be used to read data from the
/// buffer and copy it into managed memory. Note that the interface also implements the <see cref=
/// "System.IDisposable" /> interface. The Dispose method needs to be called as soon as the buffer
/// is no longer needed to prevent memory leaks.
/// </summary>
public interface class IInt16BufferHandle : IDisposable {

	/// <summary>
	/// Gets the i-th buffer element where i is a zero-based index.
	/// </summary>
	property short default[int] { short get(int i); }

	/// <summary>
	/// Gets the size (maximum number of samples) of the data buffer.
	/// </summary>
	property int BufferSize { int get(); }

	/// <summary>
	/// Copies a block of data from the buffer into managed memory.
	/// </summary>
	/// <param name="destination">The managed array into which to copy the data.</param>
	/// <param name="destinationStartIndex">The element index of the destination array at which to
	/// begin copying data.</param>
	/// <param name="sourceStartIndex">The sample index of the buffer into from which to begin copying
	/// data.</param>
	/// <param name="length">The number of samples to copy from the buffer to the destination array.
	/// </param>
	void Copy(array<short>^ destination, int destinationStartIndex, int sourceStartIndex, int length);
};

public ref class UnmanagedInt16BufferHandle : IInt16BufferHandle {
private:
	short* _buffer;
	int _bufferSize;

internal:
	UnmanagedInt16BufferHandle(int size) : _bufferSize(size) {
		_buffer = new short[size];
	}

	short* BufferPointer() {
		return _buffer;
	}

public:
	~UnmanagedInt16BufferHandle() {
		delete _buffer;
	}

	virtual property short default[int] {
		short get(int i) {
			if (i < 0 || i >= _bufferSize) {
				throw gcnew IndexOutOfRangeException();
			}

			return _buffer[i];
		}
	}

	virtual property int BufferSize {
		int get() {
			return _bufferSize;
		}
	}

	virtual void Copy(array<short>^ destination, int destinationStartIndex, int sourceStartIndex, int length) {
		if (sourceStartIndex < 0 || length < 0 || sourceStartIndex + length > _bufferSize) {
			throw gcnew IndexOutOfRangeException();
		}

		Marshal::Copy(IntPtr((void*)(_buffer + sourceStartIndex * sizeof(short))), destination, destinationStartIndex, length);
	}
};

public ref class PinnedInt16BufferHandle : IInt16BufferHandle {
private:
	array<short>^ _buffer;
	GCHandle _handle;

internal:
	PinnedInt16BufferHandle(array<short>^ buffer) : _buffer(buffer) {
		GC::Collect();
		_handle = GCHandle::Alloc(_buffer, GCHandleType::Pinned);
	}

	short* BufferPointer() {
		return (short*)GCHandle::ToIntPtr(_handle).ToPointer();
	}

public:
	~PinnedInt16BufferHandle() {
		_handle.Free();
	}

	virtual property short default[int] {
		short get(int i) {
			return _buffer[i];
		}
	}

	virtual property int BufferSize {
		int get() {
			return _buffer->Length;
		}
	}

	virtual void Copy(array<short>^ destination, int destinationStartIndex, int sourceStartIndex, int length) {
		Array::Copy(_buffer, sourceStartIndex, destination, destinationStartIndex, length);
	}
};
