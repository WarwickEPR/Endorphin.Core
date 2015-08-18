namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open System.Text
open System.Runtime.InteropServices

module NativeApi = 

    [<Literal>]
    let dllName = "Piezojena.Protocols.Nv40Multi.dll" 
    
    [<DllImport(dllName, EntryPoint = "GetCommandError")>]
    /// Retrieves error string for command. 
    extern void GetCommandError(StringBuilder)

    [<DllImport(dllName, EntryPoint = "GetIdentification")>]
    /// Retrieves the Piezojena's identification string.
    extern void GetIdentification(StringBuilder)
    
    [<DllImport(dllName, EntryPoint = "GetSerialNumber")>]
    /// Retrieves the Piezojena's serial number.
    extern void GetSerialNumber(int& serial)
    
    [<DllImport(dllName, EntryPoint = "GetVersion")>]
    /// Retrieves the Piezojena's version along with the time the version was retrieved.
    extern void GetVersion([<Out>] int& major, [<Out>] int& minor, [<Out>] int& build, [<Out>] System.DateTime& time )

    [<DllImport(dllName, EntryPoint = "Initialize")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern void Initialise()
    
    [<DllImport(dllName, EntryPoint = "OnUpdateError")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern void UpdateError (byte channel, Piezojena.Protocols.Nv40Multi.Nv40MultiErrors)

    [<DllImport(dllName, EntryPoint = "GetTemperature")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern void GetTemperature([<Out>] float& temperature)
    
    [<DllImport(dllName, EntryPoint = "SetSoftStart")>]
    /// Sets soft start on or off. s
    extern void SetSoftStart(bool softstart)
    
    [<DllImport(dllName, EntryPoint = "SetClosedLoopControlled")>]
    /// If true then the channel set to closed loop control. Channel number is in the form of a byte. 
    extern void SetClosedLoopControlled(byte channel, bool closedloop)
    
    [<DllImport(dllName, EntryPoint = "GetClosedLoopLimits")>]
    /// Retrieves the closed loop limits.  
    extern void GetClosedLoopLimits(byte channel, [<Out>] float& minimum, [<Out>] float& maximum)
    
    [<DllImport(dllName, EntryPoint = "GetActuatorCoordinate")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern void GetCoordinate(byte, [<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiActuatorCoordinate& coordinate )

    [<DllImport(dllName, EntryPoint = "ChangeChannel")>]
    /// Changes the channel, takes arguments ID as a string form and channel number as a byte.
    extern void ChangeChannel(string id, byte channel)
    
    [<DllImport(dllName, EntryPoint = "CheckChannel")>]
    // ?????????????????????????????????????
    extern void CheckChannel(byte channel)
    
    [<DllImport(dllName, EntryPoint = "SetDesiredOutput")>]
    /// Set desired output for a dingle channel.
    extern void SetOutput (byte channel, float output)
        
    [<DllImport(dllName, EntryPoint = "SetDesiredOutputChunk")>]
    // ???????????????????????????????????????
    extern void SetMultipleOutputs (float [])

    [<DllImport(dllName, EntryPoint = "GetEncoder")>]
    /// Retrieves experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern void GetEncoder([<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode& mode, [<Out>] int& timeMilliseconds, [<Out>] int& steplimit, [<Out>] byte& exponent, [<Out>] float& closedstep, [<Out>] float& openstep )    

    [<DllImport(dllName, EntryPoint = "SetEncoder")>]
    /// Sets experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern void SetEncoder(Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, int timeMilliseconds, int steplimit, byte exponent, float closedstep, float openstep )