#### 0.3.7 - 2016-09-08
* Add bytes to SCPI.format. Add helpers for instruments to log errors

#### 0.3.6 - 2016-09-07
* Add logging to the SCPI functions.

#### 0.3.5 - 2016-08-10
* Add SCPI.format for conditionally accessing the IScpiFormatable interface.

#### 0.3.4 - 2016-08-10
* Add SCPI.Checked.Query.errors for initialisation checks.

#### 0.3.3 - 2016-08-10
* Make the SCPI.\*.verbatim functions consistent in calling convention with the
  rest of the library.
* Add checked SCPI IO functions, where the error queue is checked after each
  command, and an exception raised if errors are found.

#### 0.3.2 - 2016-08-09
* Add IScpiFormatable interface so as not to clobber obj.ToString () on
  user-defined types.  Primitive types may still be passed, and their
  obj.ToString () methods will be called.

#### 0.3.1 - 2016-08-08
* Add type annotations to SCPI generic functions for better type safety.

#### 0.3.0 - 2016-08-08
* Add SCPI library to Endorphin.Core
* Add SCPI interface to VISA instrument agents
* Remove obsoleted VISA async IO functions.

#### 0.2.0 - 2016-08-03
* Obsolete VISA async IO functions due to a memory bug potentially in the
  NI-VISA implementation.

#### 0.1.2 - 2016-07-01
* Make Endorphin.Core.NationalInstruments use the correct version of Units.fs

#### 0.1.1 - 2016-07-01
* Make units visible outside the assembly!

#### 0.1.0=beta2 - 2016-07-01
* Move Units.fs into the main package to prevent naming conflicts.

#### 0.1.0-beta1 - 2016-06-28
* Initial open-source release of Endorphin.Core
