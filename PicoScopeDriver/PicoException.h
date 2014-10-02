#pragma once

#include "Stdafx.h"

using namespace System;

namespace PicoScopeDriver {

	public ref class PicoException : public Exception {
	private:
		PicoStatus _status;
		static String^ BuildMessageForStatus(PicoStatus status);

	internal:
		PicoException(PicoStatus status)
			: Exception(BuildMessageForStatus(status)) {
			_status = status;
		}

	public:
		/// <summary>
		/// Gets the <see cref="PicoScopeDriver.PicoStatus" /> which caused the exception.
		/// </summary>
		property PicoStatus Status {
			PicoStatus get() { return _status; }
		}
	};

}