#pragma once

#include "Stdafx.h"

using namespace System;

namespace PicoScopeDriver {

	public ref class PicoException : public Exception {
	private:
		PicoStatus m_status;
		static String^ buildMessageForStatus(PicoStatus status);

	public:
		PicoException(PicoStatus status) 
			: Exception(buildMessageForStatus(status)) {
			m_status = status;
		}
		
		property PicoStatus Status {
			PicoStatus get() { return m_status; }
		}
	};

}