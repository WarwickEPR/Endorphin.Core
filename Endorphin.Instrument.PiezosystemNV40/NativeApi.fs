namespace Endorphin.Instrument.PiezosystemNV40

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open System.Text
open System.Runtime.InteropServices

module NativeApi = 

    [<Literal>]
    let dllName = "Piezojena.Protocols.Nv40Multi.dll" 
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetIdentification")>]
    /// Retrieves the Piezojena's identification string.
    extern void GetIdentification(StringBuilder)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetSerialNumber")>]
    /// Retrieves the Piezojena's serial number.
    extern void GetSerialNumber([<Out>] int& serial)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetVersion")>]
    /// Retrieves the Piezojena's version along with the time the version was retrieved.
    extern void GetVersion([<Out>] int& major, [<Out>] int& minor, [<Out>] int& build, [<Out>] System.DateTime& time )

    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetTemperature")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern void GetTemperature([<Out>] float& temperature)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.SetSoftStart")>]
    /// If true then soft start is used, initalises the Piezojena, takes approximately 10 seconds.
    extern void SetSoftStart(bool softstart)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40Multi.SetClosedLoopControlled")>]
    /// If true then the channel set to closed loop control. Channel number is in the form of a byte. 
    extern void SetClosedLoopControlled(byte channel, bool closedloop)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.ChangeChannel")>]
    /// Changes the channel, takes arguments ID as a string form and channel number as a byte.
    extern void ChangeChannel(string id, byte channel)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.CheckChannel")>]
    // ???????????????????????????????????????????????????????????????????????????????????????????????
    extern void CheckChannel(byte channel)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetEncoder")>]
    /// Retrieves experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern void GetEncoder([<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, [<Out>] int& timeMilliseconds, [<Out>] int& steplimit, [<Out>] byte& exponent, [<Out>] float& closedstep, [<Out>] float& openstep )    

    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.SetEncoder")>]
    /// Sets experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern void SetEncoder(Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, int timeMilliseconds, int steplimit, byte exponent, float closedstep, float openstep )