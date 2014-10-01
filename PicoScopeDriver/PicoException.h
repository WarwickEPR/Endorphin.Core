#pragma once

#include "Stdafx.h"

using namespace System;

namespace PicoScopeDriver {

	public ref class PicoException : public Exception {
	private:
		PicoStatus _status;
		static String^ buildMessageForStatus(PicoStatus status);

	public:
		PicoException(PicoStatus status) 
			: Exception(buildMessageForStatus(status)) {
			_status = status;
		}
		
		property PicoStatus Status {
			PicoStatus get() { return _status; }
		}
	};

}