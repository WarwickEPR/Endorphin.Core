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
    
    [<DllImport(dllName, EntryPoint = " Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetVersion")>]
    /// Retrieves the Piezojena's version along with the time the version was retrieved.
    extern Error GetVersion([<Out>] int& major, [<Out>] int& minor, [<Out>] int& build, [<Out>] System.DateTime& time )

    [<DllImport(dllName, EntryPoint = " Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.GetTemperature")>]
    /// Retrieves the Piezojena's temperature in kelvin.
    extern Error GetTemperature([<Out>] float& temperature)
    
    [<DllImport(dllName, EntryPoint = " Piezojena.Protocols.Nv40Multi.Nv40MultiCommon.SetSoftStart")>]
    /// If true then soft start is used, initalises the Piezojena, takes approximately 10 seconds.
    extern Error SetSoftStart(bool)
        