using namespace System;
using namespace System::Collections::Generic;

public interface class IInt16BufferHandle : IDisposable {
	property short default[int] { short get(int i); }
	property int BufferSize { int get(); }
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
