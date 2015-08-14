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
    extern Error GetIdentification(StringBuilder)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetSerialNumber")>]
    /// Retrieves the Piezojena's serial number.
    extern Error GetSerialNumber([<Out>] int& serial)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetVersion")>]
    /// Retrieves the Piezojena's version along with the time the version was retrieved.
    extern Error GetVersion([<Out>] int& major, [<Out>] int& minor, [<Out>] int& build, [<Out>] System.DateTime& time )

    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetTemperature")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern Error GetTemperature([<Out>] float& temperature)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.SetSoftStart")>]
    /// If true then soft start is used, initalises the Piezojena, takes approximately 10 seconds.
    extern Error SetSoftStart(bool softstart)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40Multi.SetClosedLoopControlled")>]
    /// If true then the channel set to closed loop control. Channel number is in the form of a byte. 
    extern Error SetClosedLoopControlled(byte channel, bool closedloop)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.ChangeChannel")>]
    /// Changes the channel, takes arguments ID as a string form and channel number as a byte.
    extern Error ChangeChannel(string id, byte channel)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.CheckChannel")>]
    // ???????????????????????????????????????????????????????????????????????????????????????????????
    extern Error CheckChannel(byte channel)
    
    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetEncoder")>]
    /// Retrieves experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern Error GetEncoder([<Out>] Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, [<Out>] int& timeMilliseconds, [<Out>] int& steplimit, [<Out>] byte& exponent, [<Out>] float& closedstep, [<Out>] float& openstep )    

    [<DllImport(dllName, EntryPoint = "Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.SetEncoder")>]
    /// Sets experiemnt parameters, the mode, the time, the exponent used to calculate the acceleration,
    /// and the distance step in closed and open loop modes. 
    extern Error SetEncoder(Piezojena.Protocols.Nv40Multi.Nv40MultiEncoderMode mode, int timeMilliseconds, int steplimit, byte exponent, float closedstep, float openstep )